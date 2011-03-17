;;; locales.el --- a minor mode for frame-relative buffer lists

;; Copyright (c) 2011 Alp Aker

;; Author: Alp Aker <aker@pitt.edu>
;; Version: 0.76
;; Keywords: convenience

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

;; Locales extends Emacs's buffer menu so that it understands a distinction
;; between those buffers that "belong" to a frame and those that do not.  The
;; buffer menu can be toggled between a list of all buffers and a list of
;; only buffers that are local to a frame.  In addition, buffers can be added
;; to and removed from the frame-local buffer list from within the buffer
;; menu; buffers can also be made non-local interactively using the function
;; `locales-make-non-local'.  (See below under Usage for a fuller description
;; of these features.)  All functions of the default buffer menu remain
;; available.

;; Locales also alters the behavior of `other-buffer' so that, e.g., the
;; default buffer suggested by `switch-to-buffer' is, when possible, a
;; frame-local buffer.  It also respects any re-ordering of buffers that
;; results from the use of bury-buffer.

;; The package does not alter the buffer-list frame parameter or change the
;; result of calls to the function `buffer-list'.

;; The criteria for which buffers are automatically added to the local buffer
;; list can be controlled via several variables; see the documentation string
;; of `locales-mode' for a list of these (or see below under Usage).  See the
;; documentation string of the command `locales-buffer-menu' for a fuller
;; description of the new commands available in the buffer menu itself (or
;; see below under Usage).

;; Caveats
;; =======

;; PLEASE NOTE:  Locales does not presently work if you have `pop-up-frames'
;; set to non-nil.  Fixing that is on the todo list.

;; PLEASE ALSO NOTE: Locales is a relatively new package (I've used it
;; privately for some time, but have only recently cleaned it up for public
;; distribution).  It's therefore likely to conflict with other extensions to
;; the buffer-menu.  If you encounter any such problems, please send me a
;; note so I can ensure compatibility.

;; Installation
;; ============

;; Put this file in your load path and put:
;;
;;   (require 'locales)
;;
;; in your .emacs.  To toggle locales mode on and off, use the command
;;`locales-mode'.  To turn it on automatically when starting emacs, put:
;;
;;  (locales-mode t) 
;;
;; in your .emacs.  

;; When locales mode is enabled, C-x C-b is globally mapped to call
;; `locales-list-buffers'.  When disabled, the key sequence is remapped to
;; its default value `list-buffers'.

;; You might also wish to bind the command `locales-make-non-local' while
;; locales mode is enabled (see below, under Usage, for a description of this
;; command).  To bind it to, e.g, C-c -, add the following two forms to your
;; .emacs:
;;
;;  (add-hook 'locales-mode-on-hook
;;             (global-set-key [(control ?c) ?-] 'locales-make-non-local))
;;  (add-hook 'locales-mode-off-hook
;;             (global-set-key [(control ?c) ?-] 'undefined))
;;

;; Usage
;; =====

;; Locales operates fairly transparently.  A call to the buffer menu command
;; using C-x C-b opens a new buffer menu.  By default it lists only buffers
;; local to the selected frame (we call this `local mode').  By typing `a'
;; one can toggle between listing only local buffers and listing all buffers
;; (we call the latter `global mode').  In global mode, there is a fourth
;; column in the buffer menu after the C, R, M columns: the L
;; column.  Buffers local to the current frame are indicated with an `o' in
;; this column (this indicator bit can be changed via
;; `locales-local-bit').  In local mode, the fourth column is
;; suppressed.  The variable `locales-open-in-global-mode' can be used to
;; control whether the buffer menu opens in local or global mode.

;; All commands available in the default buffer menu are still available,
;; including sorting by buffer name, buffer size, etc., or toggling the
;; files-only.

;; Besides toggling between local and global modes, two other new commands
;; are available in the buffer menu.  By typing `+' a file can be marked as
;; to be added to the local buffer list.  By typing `-' a file can be marked
;; as to be removed from the local buffer list.  As with other actions in the
;; buffer menu, these changes take effect when buffer-menu-execute is
;; called.  (NB: They are also executed when toggling between local and global
;; modes.)  When a buffer is made non-local, it is removed from any windows
;; on the current frame displaying that buffer and replaced with another
;; frame-local buffer.

;; Buffers can also be made non-local outside the buffer menu.  The command
;; `locales-make-non-local' can be called interactively to remove a buffer
;; from the local buffer list; when called with no argument, it acts on the
;; current buffer.  (Locales does not, by default, bind this command to any
;; key-sequence.)

;; The association between buffers and frames is dynamic:  if a buffer is
;; selected on a frame, then it is considered to be local to the
;; frame.  (Note, then, that a buffer can be local to more than one frame.)
;; In addition, several other criteria can be used to control the membership
;; of a frame's local list:

;; o If `locales-include-scratch' is non-nil, then *scratch* is always added
;;   to the local buffer list.
;;
;; o If `locales-include-displayed-buffers' is non-nil, then buffers that are
;;   merely displayed on a frame are considered to be local to the frame,
;;   even if they have not been selected.
;;
;; o The variables `locales-include-new-buffers' and
;;   `locales-include-init-buffer' control which buffers are local to a newly
;;   created frame.  If the command that creates a new frame also creates new
;;   buffers, then one might want those buffers to count as local to the new
;;   frame.  If `locales-include-new-buffers' is non-nil, then such buffers
;;   are added to the new frame's local buffer list, even if they have not
;;   been selected or displayed.  (NB: This applies only to buffers that are
;;   created *after* the new frame is created.)

;;   If `locales-include-init-buffer' is non-nil, then the buffer that is
;;   current when the command creating a new frame is called (the
;;   `init-buffer') will belong to the new frame.  If nil, it will not.  (NB:
;;   Other criteria take precedence over this variable.  If the init-buffer
;;   is selected after the new frame is created, then it will belong to the
;;   new frame.  Similarly, if `locales-include displayed-buffers' is non-nil
;;   and the init-buffer is displayed in some window when the frame-creating
;;   command terminates, then the init-buffer will belong to the new frame.)
;;

;; Interaction with Other Default Functions and Other Packages
;; ==========================================================

;; Locales does not touch the buffer-list or buried-buffer-list frame
;; parameters, nor change the results of any call to the function
;; `buffer-list'.  It sets each frame's buffer predicate so that other-buffer
;; will prefer buffers local to the frame; if another buffer predicate is
;; present when the package is enabled, locales saves that predicate and uses
;; it as well.

;; If you wish to access a frame's local buffer list directly for some
;; purpose, it is recommended that you use the list returned by the function
;; `locales-buffer-list'; don't use the value of the locales-buffer-list
;; frame parameter.  The latter can contain both dead buffers and internal
;; buffers (buffers whose names starts with a space). In addition, its order
;; is meaningless.  The list returned by `locales-buffer-list' will contain
;; only live, non-internal buffers, sorted stably by selection order on the
;; current frame.

;;; Code:

(when (< emacs-major-version 22)
  (error "Locales requires version 22 or later"))

;;; Declarations

;; User Options

(defgroup locales nil
  "Extend buffer-menu to allow listing of a frame's local buffer list only."
  :group 'convenience)

(defgroup locales-buffer-menu nil
  "Appearance of the buffer menu."
  :group 'locales)

(defgroup locales-buffer-list nil
  "Control which buffers belong to the local buffer list."
  :group 'locales)

(defcustom locales-mode-hook nil
  "Hook run when locales mode is enabled or disabled."
  :group 'locales
  :type 'hook)

(defcustom locales-mode-on-hook nil
  "Hook run when locales mode is enabled."
  :group 'locales
  :type 'hook)

(defcustom locales-mode-off-hook nil
  "Hook run when locales mode is disabled."
  :group 'locales
  :type 'hook)

(defcustom locales-include-scratch nil
  "If non-nil, always include *scratch* on the local buffer list.
If necessary, a new buffer *scratch* is created."
  :group 'locales-buffer-list
  :type 'boolean)

(defcustom locales-include-displayed-buffers t
  "If non-nil, buffers displayed on a frame belong to the local buffer list.
If nil, buffers are added to the local buffer list only if they
are selected, not merely displayed."
  :group 'locales-buffer-list
  :type 'boolean)

(defcustom locales-include-new-buffers nil
  "Include new buffers in a new frame's buffer list.
If non-nil, and the command that creates a new frame also creates
new buffers, those buffers will belong to the new frame's buffer
list, even if they have not been selected.  (Buffers created
before the new frame is created are not affected by this
variable.)"
  :group 'locales-buffer-list
  :type 'boolean)

(defcustom locales-include-init-buffer nil
  "Include in a new frame's buffer list the buffer from which make-frame was called.
If non-nil, the buffer that is current when the command creating
a new frame is called is included in the new frame's local buffer
list.  If nil, that buffer is omitted from the new frames initial
buffer list (unless is is displayed on the frame when the
frame-creating command terminates, in which case it is included
on the local buffer list)."
  :group 'locales-buffer-list
  :type 'boolean)

(defcustom locales-local-bit ?o 
  "Charcater used to indicate frame-local buffers in the buffer menu."
  :group 'locales-buffer-menu
  :type 'character)

(defcustom locales-open-in-global-mode nil 
  "If true, the buffer menu opens in global mode when first called."
  :group 'locales-buffer-menu
  :type 'boolean)

(defcustom locales-buffer-menu-use-header-line 'inherit 
  "If non-nil, the buffer menu uses a header line.
If `inherit', use the value of `Buffer-menu-use-header-line'."
  :group 'locales-buffer-menu
  :type '(choice (symbol :tag "Inherit from buff-menu" 'inherit)
                 (const :tag "Use a fixed header line" t)
                 (const :tag "Don't use a fixed header line" nil)))

(defcustom locales-buffer-menu-buffer+size-width 'inherit 
"Non-nil means to use an immovable header-line.
If `inherit', use the value of Buffer-menu-buffer+size-width."
  :group 'locales-buffer-menu
  :type '(choice (symbol :tag "Inherit from buff-menu" 'inherit)
                 (integer :tag "Enter a width")))

(defcustom locales-buffer-menu-mode-width 'inherit 
  "How wide to make the mode name column.
If 'inherit', use the value of Buffer-menu-mode-width."
  :group 'locales-buffer-menu
  :type '(choice (symbol :tag  "Inherit from buff-menu" 'inherit)
                 (integer :tag "Enter a width")))

(defcustom locales-buffer-menu-sort-column 'inherit
  "Initial sort order for the buffer menu.
If `inherit', use the value of `Buffer-menu-sort-column.'"
  :group 'locales-buffer-menu
  :type '(choice (symbol :tag "Inherit from buff-menu" 'inherit)
                 (const :tag "Sort by visited order" nil)
                 (const :tag "Sort by buffer name" 2)
                 (const :tag "Sort by buffer size" 3)
                 (const :tag "Sort by mode name" 4)
                 (const :tag "Sort by file name" 5)))

(defcustom locales-buffer-menu-short-ellipsis 'inherit
  "Character to use when truncating long buffer names in the buffer menu.
If `inherit', use the value of `Buffer-menu-short-ellipsis'."
  :group 'locales-buffer-menu
  :type '(choice (symbol :tag  "Inherit from buff-menu" 'inherit)
                 (character :tag "Enter a character")))

(defface locales-buffer-menu-buffer
  '((t (:inherit buffer-menu-buffer)))
  "Face used to highlight buffer names in the buffer menu."
  :group 'locales-buffer-menu)

;; Internal Variables

(defvar locales-mode nil 
  "True if locales mode is enabled.

Do not set this variable directly.  Use the command
`locales-mode' instead.")

;; Non-nil if the current buffer-menu lists all buffers.  Nil if the current
;; buffer-menu lists only frame-visible buffers.  This variable determines
;; whether reverting the buffer lists only frame-visible buffers.  It affects
;; both manual reverting and reverting by Auto Revert Mode.
(defvar locales-menu-global nil)
(make-variable-buffer-local 'locales-menu-global)

(defvar locales-version-23-24
  (< 22 emacs-major-version))

(defconst locales-buffer-menu-buffer-column 5)

;; Shut up the compiler.
(defvar locales-new-frame nil)
(defvar locales-prev-buffers nil)
(defvar locales-init-buffer nil)

;; Variables we make buffer-local in the locales buffer-menu, either
;; because we always set their values to something other than the buff-menu
;; defaults; or we need to have different values in the buffer menus for
;; different frames; or we need to allow the user to specify custom values
;; for locales's use but don't want to overwrite the buff-menu values.
(defconst locales-local-vars 
  '(Buffer-menu-files-only
    revert-buffer-function
    Buffer-menu-buffer-column
    Buffer-menu-short-ellipsis
    Buffer-menu-sort-column
    Buffer-menu-buffer-column
    Buffer-menu-use-header-line 
    Buffer-menu-buffer+size-width
    Buffer-menu-mode-width))

;; Variables which can inherit their values from the counterpart variables
;; from buff-menu.
(defconst locales-derived-vars
  '(Buffer-menu-buffer-column
    Buffer-menu-short-ellipsis
    Buffer-menu-sort-column
    Buffer-menu-use-header-line 
    Buffer-menu-buffer+size-width
    Buffer-menu-mode-width))

;; We wouldn't need to define our own version of this map if buffer-menu-sort
;; didn't have the default value of Buffer-menu-buffer-menu hard-coded into
;; its definition.
(defvar locales-menu-sort-button-map
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line mouse-1] 'locales-menu-sort-by-column)
    (define-key map [header-line mouse-2] 'locales-menu-sort-by-column)
    (define-key map [mouse-2] 'locales-menu-sort-by-column)
    (define-key map [follow-link] 'mouse-face)
    (define-key map "\C-m" 'locales-menu-sort-by-column)
    map)
  "Local keymap for locales buffer menu sort buttons.")

;;; Mode Definition

(defun locales-mode (&optional arg) 
  "Toggle locales mode on and off.

With argument ARG, turn locales mode on if and only if ARG is t or positive.

Locales mode implements a version of the buffer menu that
understands a distinction between buffers that \"belong\" to a
frame and those that do not.  The menu can be toggled between
showing all buffers and just those buffers that are local to the
selected frame.  Buffers can also be added to and removed from
the local list via the buffer menu.  (See the documentation for
the command `locales-buffer-menu' for a fuller description, or see the Usage
section in locales.el.)

Buffers are always added to the local list if they are selected
on a frame.  Other criteria governing which buffers belong to the
local list can be controlled by the variables
`locales-include-displayed-buffers', `locales-include-scratch',
`locales-include-init-buffer', and
`locales-include-new-buffers'."
  (interactive "P")
  (setq locales-mode (if (not arg) 
                           (not locales-mode)
                         (> (prefix-numeric-value arg) 0)))
  (if locales-mode
      (progn
        (dolist (frame (frame-list))
          (locales-set-up-frame frame))
        (global-set-key [(control ?x) (control ?b)] 'locales-list-buffers)
        (add-hook 'window-configuration-change-hook	'locales-window-change)
        (add-hook 'after-make-frame-functions 'locales-after-make-frame)
        (run-hooks 'locales-mode-on-hook)
        (message "Per-frame buffer menus are enabled"))
    (dolist (frame (frame-list))
      (locales-clear-frame frame))
    (global-set-key [(control ?x) (control ?b)] 'list-buffers)
    (remove-hook 'window-configuration-change-hook 'locales-window-change)
    (remove-hook 'after-make-frame-functions 'locales-after-make-frame)
    (run-hooks 'locales-mode-off-hook)
    (message "Per-frame buffer menus are disabled"))
  (run-hooks 'locales-mode-hook))

;;; Initialization and Clean Up

;; Set the local buffer list initially to the frame-parameter
;; buffer-list.  This is used only for frames that already exist when locales
;; is enabled.
(defun locales-set-up-frame (frame)
  (locales-set-buffer-predicate frame)
  (set-frame-parameter frame
                       'locales-buffer-list 
                       (append (frame-parameter frame 
                                                'buffer-list)
                               ;; Need to copy this list because append
                               ;; doesn't copy its last argument.
                               (locales-copy-list 
                                (frame-parameter frame
                                                 'buried-buffer-list))))
  (when locales-include-displayed-buffers
    (dolist (win (window-list frame 'no-minibuf))
      (locales-push-buffer (window-buffer win) frame))))

;; Set the buffer predicate to our buffer predicate.  Save any existing
;; buffer predicate so we can check that too when our buffer predicate is
;; called (as opposed to quashing the existing buffer predicate).
(defun locales-set-buffer-predicate (frame)
  (let ((buffer-pred  (frame-parameter frame 'buffer-predicate)))
    (unless (eq buffer-pred 'locales-ok-to-display-p)
      (set-frame-parameter frame
                           'locales-saved-buffer-pred
                           buffer-pred)
      (set-frame-parameter frame 
                           'buffer-predicate 
                           'locales-ok-to-display-p))))

;; Set up the routine called after the command creating a new frame is
;; finished.
(defun locales-after-make-frame (frame)
  (add-hook 'post-command-hook 'locales-update-after-command)
  ;; We record (a) the identity of the new frame (it might not be selected
  ;; when the command that created it terminates); (b) the buffers in
  ;; existence when the frame was created, so that we can tell which other
  ;; buffers the frame-creating command created; and (c) the buffer that was
  ;; current when the new frame was created, so we can decide whether to
  ;; include it in the new frame's local buffer list.
  (setq locales-new-frame frame
        locales-prev-buffers (buffer-list)
        locales-init-buffer (current-buffer)))

;; After a command that creates a new frame is finished, decide what buffers
;; should initially belong to the new frame's local buffer list and set up the
;; frame.
(defun locales-update-after-command ()
  (remove-hook 'post-command-hook 'locales-update-after-command)
  (when (frame-live-p locales-new-frame)
    (locales-set-buffer-predicate locales-new-frame)
    (if (or locales-include-init-buffer
            (get-buffer-window locales-init-buffer))
        (locales-push-buffer locales-init-buffer locales-new-frame)
      (locales-pop-buffer locales-init-buffer locales-new-frame))
    (when locales-include-new-buffers
      (let ((new-bufs (locales-set-minus locales-prev-buffers (buffer-list))))
        (dolist (buf new-bufs)
          (locales-push-buffer buf locales-new-frame)))))
  (setq locales-new-frame nil
        locales-prev-buffers nil
        locales-init-buffer nil))

(defun locales-clear-frame (frame)
  (set-frame-parameter frame 'buffer-predicate 
                       (frame-parameter frame 'locales-saved-buffer-predicate))
  (set-frame-parameter frame 'locales-saved-buffer-predicate nil)
  (set-frame-parameter frame 'locales-buffer-menu-buffer nil)
  (set-frame-parameter frame 'locales-buffer-list nil))

;;; Local Buffer List Maintenance and Manipulation

;; Called by window-configuration-change-hook to update the local buffer
;; list.
(defun locales-window-change ()
  (let ((frame (selected-frame)))
    (dolist (win (window-list frame 'no-minibuf))
      (let ((buf (window-buffer win)))
        ;; If merely displayed buffers are ok add buf.  If not, add buf if
        ;; it's been selected on the frame.
        (when (or locales-include-displayed-buffers
                  (memq buf (frame-parameter frame 'buffer-list))
                  (memq buf (frame-parameter frame 'buried-buffer-list)))
          (locales-push-buffer buf frame))))))

;; Remove BUF from FRAME's local buffer list.
(defsubst locales-pop-buffer (buf frame)
  (set-frame-parameter frame
                       'locales-buffer-list
                       (delq buf
                             (frame-parameter frame 'locales-buffer-list))))

;; Add BUF to FRAME's local buffer list if not already present.
(defsubst locales-push-buffer (buf frame)
  (let ((local-bufs (frame-parameter frame 'locales-buffer-list)))
    (unless (memq buf local-bufs)
      (set-frame-parameter frame 'locales-buffer-list (cons buf local-bufs)))))

;; Remove BUF from FRAME's local buffer list.  In addition, if any windows
;; on FRAME are currently displaying BUF, replace BUF in those windows with
;; some other buffer local to FRAME.
(defun locales-make-non-local (&optional buf frame)
  (interactive)
  (unless buf 
    (setq buf (current-buffer)))
  (unless frame
    (setq frame (selected-frame)))
  (locales-pop-buffer buf frame)
  ;; We loop over the windows ourselves because replace-buffer-in-windows
  ;; acts on all frames; we only want to act on the selected frame.
  (dolist (win (get-buffer-window-list buf 'no-minibuf frame))
    (set-window-buffer win (other-buffer buf))))

;; When called with argument GLOBAL non-nil, return the normal buffer
;; list.  With GLOBAL nil, return the local buffer list.
(defun locales-buffer-list (frame &optional global)
  ;; Include scratch if necessary.
  (when locales-include-scratch
    (locales-push-buffer (get-buffer-create "*scratch*") frame))
  ;; Filter out internal buffers.
  (locales-filter-buffers
   ;; If in global mode, return the full buffer-list, but sorted for this
   ;; frame.
   (if global
       (buffer-list frame)
     ;; In local mode, return the local buffer list sorted appropriately for
     ;; this frame.  First remove any dead buffers.
     (set-frame-parameter frame 
                          'locales-buffer-list
                          (delq nil 
                                (mapcar 
                                 (lambda (x) (if (buffer-live-p x) x))
                                 (frame-parameter frame
                                                  'locales-buffer-list))))
     ;; Now sort and return the updated local buffer list.
     (locales-sort-buffers frame 
                           (locales-copy-list 
                            (frame-parameter frame 'locales-buffer-list))))))

;;; Utilities and Predicates

(defun locales-copy-list (l)
  (mapcar 'identity l))

;; Return a list in which BUFS are sorted according to selection order on
;; FRAME.
(defun locales-sort-buffers (frame bufs)
  (let ((l (buffer-list frame)))
    (sort bufs 
          (lambda (x y) (> (length (memq x l))
                           (length (memq y l)))))))

;; Remove internal buffers from BUFS.
(defun locales-filter-buffers (bufs)
  (delq nil
        (mapcar (lambda (x) (if (not (string-match "^ " (buffer-name x))) x))
                bufs)))

(defun locales-set-minus (set1 set2)
  (dolist (element set1)
    (setq set2 (delq element set2)))
  set2)

;; This is set as the buffer predicate for all frames when locales is
;; enabled.  Check BUF against any other predicate that might have been
;; present, then check whether BUF is local to the current frame.  Return t
;; if both tests succeed.
(defun locales-ok-to-display-p (buf)
  (let ((other-pred (frame-parameter nil 'locales-saved-buffer-pred)))
    (and (locales-local-p buf)
         (if (functionp other-pred)
             (funcall other-pred buf)
           t))))

;; Check if BUF is local to FRAME.
(defun locales-local-p (buf &optional frame)
  (memq buf (frame-parameter frame 'locales-buffer-list)))

;;; Reimplementation of Buffer Menu:  Keymaps and Mode Definition

(defvar locales-buffer-menu-mode-map nil
  "Local keymap for `locales-menu-mode' buffers.")

;; Key bindings common to 23 and earlier versions
(unless locales-buffer-menu-mode-map 
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map "q" 'quit-window)
    (define-key map "?" 'describe-mode)
    (define-key map "a" 'locales-buffer-menu-toggle-global)
    (define-key map "+" 'locales-buffer-menu-make-local)
    (define-key map "-" 'locales-buffer-menu-make-non-local)
    (define-key map "g" 'revert-buffer)
    (define-key map "v" 'locales-buffer-menu-select)                 
    (define-key map "2" 'locales-buffer-menu-2-window)               
    (define-key map "1" 'locales-buffer-menu-1-window)               
    (define-key map "f" 'locales-buffer-menu-this-window)            
    (define-key map "e" 'locales-buffer-menu-this-window)            
    (define-key map "\C-m" 'locales-buffer-menu-this-window)         
    (define-key map "o" 'locales-buffer-menu-other-window)           
    (define-key map "\C-o" 'locales-buffer-menu-switch-other-window) 
    (define-key map "s" 'locales-buffer-menu-save)                    
    (define-key map "d" 'locales-buffer-menu-delete)                 
    (define-key map "k" 'locales-buffer-menu-delete)                 
    (define-key map "\C-d" 'locales-buffer-menu-delete-backwards)    
    (define-key map "\C-k" 'locales-buffer-menu-delete)              
    (define-key map "x" 'locales-buffer-menu-execute)
    (define-key map " " 'next-line)
    (define-key map "n" 'next-line)
    (define-key map "p" 'previous-line)
    (define-key map  [(backspace)] 'locales-buffer-menu-backup-unmark)
    (define-key map "~" 'locales-buffer-menu-not-modified) 
    (define-key map "u" 'locales-buffer-menu-unmark)
    (define-key map "m" 'locales-buffer-menu-mark)               
    (define-key map "t" 'locales-buffer-menu-visit-tags-table)   
    (define-key map "%" 'locales-buffer-menu-toggle-read-only)   
    (define-key map "b" 'locales-buffer-menu-bury)               
    (define-key map "V" 'locales-buffer-menu-view)               
    (define-key map "T" 'locales-buffer-menu-toggle-files-only)         
    (define-key map [mouse-2] 'locales-buffer-menu-mouse-select) 
    (define-key map [follow-link] 'mouse-face)
    (setq locales-buffer-menu-mode-map map)))

;; Key bindings specific to 23 and later.
(when locales-version-23-24
  (let ((map locales-buffer-menu-mode-map)
        (menu-map (make-sparse-keymap)))
    (define-key map "\C-?" 'scroll-down)
    (define-key map ">" 'end-of-buffer)
    (define-key map "<" 'beginning-of-buffer)
    (define-key map (kbd "M-s a C-s")   'locales-buffer-menu-isearch-buffers)  
    (define-key map (kbd "M-s a M-C-s") 'locales-buffer-menu-isearch-buffers-regexp) 
    (define-key locales-buffer-menu-mode-map [menu-bar locales-buffer-menu-mode] (cons "Locales-Menu" menu-map))
    (define-key menu-map [quit]
      '(menu-item  "Quit" quit-window
                   :help  "Remove the buffer menu from the display"))
    (define-key menu-map [rev]
      '(menu-item  "Refresh" revert-buffer
                   :help  "Refresh the *Buffer List* buffer contents"))
    (define-key menu-map [s0] menu-bar-separator)
    (define-key menu-map [tf]
      '(menu-item  "Show only file buffers" locales-buffer-menu-toggle-files-only
                   :button (:toggle . Buffer-menu-files-only)
                   :help  "Toggle whether the current buffer-menu displays only file buffers"))
    (define-key menu-map [s1] menu-bar-separator)
    (define-key menu-map [sel]
      '(menu-item  "Select marked" locales-buffer-menu-select
                   :help  "Select this line's buffer; also display buffers marked with `>'"))
    (define-key menu-map [bm2]
      '(menu-item  "Select two" locales-buffer-menu-2-window
                   :help  "Select this line's buffer, with previous buffer in second window"))
    (define-key menu-map [bm1]
      '(menu-item  "Select current" locales-buffer-menu-1-window
                   :help  "Select this line's buffer, alone, in full frame"))
    (define-key menu-map [ow]
      '(menu-item  "Select in other window" locales-buffer-menu-other-window
                   :help  "Select this line's buffer in other window, leaving buffer menu visible"))
    (define-key menu-map [tw]
      '(menu-item  "Select in current window" locales-buffer-menu-this-window
                   :help  "Select this line's buffer in this window"))
    (define-key menu-map [s2] menu-bar-separator)
    (define-key menu-map [is]
      '(menu-item  "Regexp Isearch marked buffers" locales-buffer-menu-isearch-buffers-regexp 
                   :help  "Search for a regexp through all marked buffers using Isearch"))
    (define-key menu-map [ir]
      '(menu-item  "Isearch marked buffers" locales-buffer-menu-isearch-buffers
                   :help  "Search for a string through all marked buffers using Isearch"))
    (define-key menu-map [s3] menu-bar-separator)
    (define-key menu-map [by]
      '(menu-item  "Bury" locales-buffer-menu-bury
                   :help  "Bury the buffer listed on this line"))
    (define-key menu-map [vt]
      '(menu-item  "Set unmodified" locales-buffer-menu-not-modified
                   :help  "Mark buffer on this line as unmodified (no changes to save)"))
    (define-key menu-map [ex]
      '(menu-item  "Execute" locales-buffer-menu-execute
                   :help  "Save and/or delete buffers marked with s or k commands"))
    (define-key menu-map [s4] menu-bar-separator)
    (define-key menu-map [delb]
      '(menu-item  "Mark for delete and move backwards" locales-buffer-menu-delete-backwards
                   :help  "Mark buffer on this line to be deleted by x command and move up one line"))
    (define-key menu-map [del]
      '(menu-item  "Mark for delete" locales-buffer-menu-delete
                   :help  "Mark buffer on this line to be deleted by x command"))
    (define-key menu-map [sv]
      '(menu-item  "Mark for save" locales-buffer-menu-save
                   :help  "Mark buffer on this line to be saved by x command"))
    (define-key menu-map [umk]
      '(menu-item  "Unmark" locales-buffer-menu-unmark
                   :help  "Cancel all requested operations on buffer on this line and move down"))
    (define-key menu-map [mk]
      '(menu-item  "Mark" locales-buffer-menu-mark
                   :help  "Mark buffer on this line for being displayed by v command"))
    (define-key menu-map [s5] menu-bar-separator)
    (define-key menu-map [tog]
      '(menu-item  "Toggle global" locales-buffer-menu-toggle-global
                   :help "Toggle between listing frame-local buffers and all buffers"))
    (define-key menu-map [mkl]
      '(menu-item "Mark for making local" locales-buffer-menu-make-local
                  :help "Mark buffer on this line to be made frame-local"))
    (define-key menu-map [mknl]
      '(menu-item "Mark for making non-local" locales-buffer-menu-make-non-local
                  :help  "Mark buffer on this line to be made non-frame-local"))))

(put 'locales-menu-mode 'mode-class 'special)

(define-derived-mode locales-buffer-menu-mode nil
      "Locales Menu"
      "Major mode for editing a list of buffers.
Each line describes one of the buffers in Emacs.
Letters do not insert themselves; instead, they are commands.
\\<locales-buffer-menu-mode-map>
\\[locales-buffer-menu-toggle-global] -- toggle whether the buffer menu displays all buffers or local buffers only.
\\[locales-buffer-menu-make-local] -- mark that buffer to be added to the local buffer list.
\\[locales-buffer-menu-make-non-local] -- mark that buffer to be removed from the local buffer list.
\\[locales-buffer-menu-mouse-select] -- select buffer you click on, in place of the buffer menu.
\\[locales-buffer-menu-this-window] -- select current line's buffer in place of the buffer menu.
\\[locales-buffer-menu-other-window] -- select that buffer in another window,
  so the buffer menu buffer remains visible in its window.
\\[locales-buffer-menu-view] -- select current line's buffer, but in view-mode.
\\[locales-buffer-menu-view-other-window] -- select that buffer in
  another window, in view-mode.
\\[locales-buffer-menu-switch-other-window] -- make another window display that buffer.
\\[locales-buffer-menu-mark] -- mark current line's buffer to be displayed.
\\[locales-buffer-menu-select] -- select current line's buffer.
  Also show buffers marked with m, in other windows.
\\[locales-buffer-menu-1-window] -- select that buffer in full-frame window.
\\[locales-buffer-menu-2-window] -- select that buffer in one window,
  together with buffer selected before this one in another window.
\\[locales-buffer-menu-isearch-buffers] -- Do incremental search in the marked buffers.
\\[locales-buffer-menu-isearch-buffers-regexp] -- Isearch for regexp in the marked buffers.
\\[locales-buffer-menu-visit-tags-table] -- visit-tags-table this buffer.
\\[locales-buffer-menu-not-modified] -- clear modified-flag on that buffer.
\\[locales-buffer-menu-save] -- mark that buffer to be saved, and move down.
\\[locales-buffer-menu-delete] -- mark that buffer to be deleted, and move down.
\\[locales-buffer-menu-delete-backwards] -- mark that buffer to be deleted, and move up.
\\[locales-buffer-menu-execute] -- delete or save marked buffers.
\\[locales-buffer-menu-unmark] -- remove all kinds of marks from current line.
  With prefix argument, also move up one line.
\\[locales-buffer-menu-backup-unmark] -- back up a line and remove marks.
\\[locales-buffer-menu-toggle-read-only] -- toggle read-only status of buffer on this line.
\\[revert-buffer] -- update the list of buffers.
\\[locales-buffer-menu-toggle-files-only] -- toggle whether the menu displays only file buffers.
\\[locales-buffer-menu-bury] -- bury the buffer listed on this line."
      (dolist (sym locales-local-vars)
        (make-local-variable sym))
      (dolist (sym locales-derived-vars)
        (locales-set-derived-var sym))
      (setq buffer-read-only t)
      (setq revert-buffer-function 'locales-menu-revert-function))

;; Remove some elements from the docstring that don't belong in 21-22.
(unless locales-version-23-24
  (let* ((fun (symbol-function 'locales-buffer-menu-mode))
         (docstring (concat (nth 2 fun))))
    (while (string-match "^\\\\\\[locales-buffer-menu-isearch.+\n" docstring)
      (setq docstring (replace-match "" nil t docstring)))
    (setcdr (cdr fun) (cons docstring (cdr (cddr fun))))))

;; If the locales version of SYM has value 'inherit, we use the value of
;; the corresponding buff-menu variable.  Otherwise, we set the buff-menu
;; variable to take the value specified by the locales option.  (These
;; buff-menu variables are all made buffer-local, so we don't override their
;; buff-menu values globally.)
(defun locales-set-derived-var (sym)
  (let ((locales-sym (locales-make-locales-sym sym)))
    (unless (eq (symbol-value locales-sym) 'inherit)
      (set sym (symbol-value locales-sym)))))

(defun locales-make-locales-sym (sym)
  (car (read-from-string (concat "locales-" (downcase (symbol-name sym))))))

;; The following functions from buff-menu.el work as in the locales buffer
;; menu.  We alias them for aesthetic reasons.

(defalias 'locales-buffer-menu-1-window 'Buffer-menu-1-window)
(defalias 'locales-buffer-menu-2-window 'Buffer-menu-2-window)
(defalias 'locales-buffer-menu-bury 'Buffer-menu-bury)
(defalias 'locales-buffer-menu-delete 'Buffer-menu-delete)
(defalias 'locales-buffer-menu-delete-backwards 'Buffer-menu-delete-backwards)
(defalias 'locales-buffer-menu-mark 'Buffer-menu-mark)
(defalias 'locales-buffer-menu-mouse-select 'Buffer-menu-mouse-select)
(defalias 'locales-buffer-menu-not-modified 'Buffer-menu-not-modified)
(defalias 'locales-buffer-menu-save 'Buffer-menu-save)
(defalias 'locales-buffer-menu-select 'Buffer-menu-select)
(defalias 'locales-buffer-menu-switch-other-window 'Buffer-menu-switch-other-window)
(defalias 'locales-buffer-menu-this-window 'Buffer-menu-this-window)
(defalias 'locales-buffer-menu-toggle-read-only 'Buffer-menu-toggle-read-only)
(defalias 'locales-buffer-menu-view 'Buffer-menu-view)
(defalias 'locales-buffer-menu-visit-tags-table 'Buffer-menu-visit-tags-table)
(when locales-version-23-24
  (defalias 'locales-buffer-menu-isearch-buffers 'Buffer-menu-isearch-buffers)
  (defalias 'locales-buffer-menu-isearch-buffers-regexp 'Buffer-menu-isearch-buffers-regexp))

;;; Reimplementaion of Buffer Menu:  New Buffer Menu Commands

(defun locales-get-buffer-menu-buffer (frame)
  (let ((buf (frame-parameter frame 'locales-buffer-menu-buffer)))
    (if (buffer-live-p buf)
        buf
      (setq buf (get-buffer-create (generate-new-buffer-name "*Buffer List*")))
      (set-frame-parameter frame 'locales-buffer-menu-buffer buf)
      buf)))

(defun locales-buffer-menu-toggle-global (arg)
  "Toggle whether the current buffer-menu displays only buffers local to this frame.
With a positive or true ARG display only local buffers.  With
zero, negative, or nil ARG, display all buffers."
  (interactive "P")
  (locales-buffer-menu-execute t)
  (setq locales-menu-global
        (cond ((not arg) (not locales-menu-global))
              ((<= (prefix-numeric-value arg) 0) t)))
  (revert-buffer))

(defun locales-buffer-menu-make-local (&optional arg)
  "Mark buffer on this line to be made local to this frame by \\<locales-menu-mode-map>\\[locales-menu-execute] command.
Prefix arg is how many buffers to make local.  Negative arg means
work backwards."
  (interactive "p")
  (if (not locales-menu-global)
      (error "The listed buffers are all visible")
  (when (Buffer-menu-no-header)
    (let ((buffer-read-only nil))
      (when (or (null arg) (= arg 0))
          (setq arg 1))
      (while (> arg 0)
        (forward-char 3)
        (when (not (looking-at (char-to-string locales-local-bit)))
              (delete-char 1)
              (insert ?+))
        (forward-line 1)
        (setq arg (1- arg)))
      (while (and (< arg 0)
                  (Buffer-menu-no-header))
        (forward-char 3)
        (when (not (looking-at (char-to-string locales-local-bit)))
              (delete-char 1)
              (insert ?+))
        (forward-line -1)
        (setq arg (1+ arg)))))))

(defun locales-buffer-menu-make-non-local (&optional arg)
  "Mark buffer on this line to be made non-local by \\<locales-menu-mode-map>\\[locales-menu-execute] command.
Prefix arg is how many buffers to to make non-local.
Negative arg means work backwards."
  (interactive "p")
  (when (Buffer-menu-no-header)
    (let ((buffer-read-only nil))
      (when (or (null arg) (= arg 0))
          (setq arg 1))
      (while (> arg 0)
        (forward-char 3)
        (when (or (not locales-menu-global)
                  (looking-at (char-to-string locales-local-bit)))
              (delete-char 1)
              (insert ?-))
        (forward-line 1)
        (setq arg (1- arg)))
      (while (and (< arg 0)
                  (Buffer-menu-no-header))
        (forward-char 3)
        (when (or (not locales-menu-global)
                (looking-at (char-to-string locales-local-bit)))
              (delete-char 1)
              (insert ?-))
        (forward-line -1)
        (setq arg (1+ arg))))))

;;; Reimplementation of Buffer Menu: Redefined Buffer Menu Commands 

(defun locales-buffer-menu-toggle-files-only (arg)
  "Toggle whether the current buffer-menu displays only file buffers.
With a positive ARG display only file buffers.  With zero or
negative ARG, display other buffers as well."
  (interactive "P")
  (setq Buffer-menu-files-only
        (cond ((not arg) (not Buffer-menu-files-only))
              ((> (prefix-numeric-value arg) 0) t)))
  (revert-buffer))

(defun locales-buffer-menu-unmark (&optional backup)
  "Cancel all requested operations on buffer on this line and move down.
Optional ARG means move up."
  (interactive "P")
  (when (Buffer-menu-no-header))
    (let* ((buf (Buffer-menu-buffer t))
           (mod (if (buffer-modified-p buf)
                    "*" " "))
           (readonly (if (with-current-buffer buf buffer-read-only)
                         "%" " "))
           (local (if (and locales-menu-global
                           (locales-local-p buf))
                      (char-to-string locales-local-bit) " "))
           (buffer-read-only nil))
      (delete-char 4)
      (insert (concat " " readonly mod local)))
    (forward-line (if backup -1 1)))

(defun locales-buffer-menu-backup-unmark ()
  "Move up and cancel all requested operations on buffer on line above."
  (interactive)
  (forward-line -1)
  (locales-buffer-menu-unmark)
  (forward-line -1))

(defun locales-buffer-menu-execute (&optional visibles-only)
  "Save, make visible, make frame-invisible, and/or delete
buffers marked with
\\<locales-menu-mode-map>\\[locales-menu-save],\\<locales-menu-mode-map>\\[locales-menu-delete],\\<locales-menu-mode-map>\\[locales-menu-mark-visible],
or \\<locales-menu-mode-map>\\[locales-menu-mark-invisible]
commands."
  (interactive)
    (save-excursion
      (Buffer-menu-beginning)
      (let ((buffer-read-only nil)
            (buffer-menu-buffer (current-buffer)))
        (while (re-search-forward "^...\\+" nil t)
          (forward-char -1)
          (let ((buf (Buffer-menu-buffer t)))
            (locales-push-buffer buf (selected-frame))
            (delete-char 1)
            (insert locales-local-bit)))))
    (save-excursion
      (Buffer-menu-beginning)
      (let ((buffer-read-only nil)
            (buffer-menu-buffer (current-buffer)))
        (while (re-search-forward "^...\\-" nil t)
          (let ((buf (Buffer-menu-buffer t)))
            (locales-pop-buffer buf (selected-frame))
            (if locales-menu-global
                (progn
                  (forward-char -1)
                  (let ((props (text-properties-at (point))))
                    (delete-char 1)
                    (insert (apply 'propertize " " props))))
              (forward-char -4)
              (delete-region (point) (progn (forward-line 1) (point)))
              (unless (bobp)
                (forward-char -1)))))))
    (unless visibles-only
      (save-excursion
        (Buffer-menu-beginning)
        (while (re-search-forward "^..S" nil t)
          (let ((modp nil))
            (with-current-buffer (Buffer-menu-buffer t)
              (save-buffer)
              (setq modp (buffer-modified-p)))
            (let ((buffer-read-only nil))
              (delete-char -1)
              (insert (if modp "*" " "))))))
      (save-excursion
        (Buffer-menu-beginning)
        (let ((buff-menu-buffer (current-buffer))
              (buffer-read-only nil))
          (while (re-search-forward "^D" nil t)
            (forward-char -1)
            (let ((buf (Buffer-menu-buffer nil)))
              (or (eq buf nil)
                  (eq buf buff-menu-buffer)
                  (save-excursion (kill-buffer buf)))
              (if (and buf (buffer-name buf))
                  (progn (delete-char 1)
                         (insert " "))
                (delete-region (point) (progn (forward-line 1) (point)))
                (unless (bobp)
                  (forward-char -1)))))))))

;; This redefinition has nothing to do with our purposes, but it fixes a bug
;; that offends me.  Certain emacs ports, in certain window configurations,
;; get confused by mouse events caused by clicking on the header line in the
;; buffer menu.  In such cases, both this function and
;; Buffer-menu-sort-by-column get called, even though only the latter should
;; be called.  This is a C-level bug that won't get fixed until v24 is
;; released (if then), so we just use a hack here to make sure that this
;; function does nothing if it's called by a mouse click on the header line.
(defun Buffer-menu-mouse-select (event)
  "Select the buffer whose line you click on."
  (interactive "e")
  (unless (eq (nth 1 (nth 1 event)) 'header-line)
    (let (buffer)
      (with-current-buffer (window-buffer (posn-window (event-end event)))
        (save-excursion
          (goto-char (posn-point (event-end event)))
          (setq buffer (Buffer-menu-buffer t))))
      (select-window (posn-window (event-end event)))
      (if (and (window-dedicated-p (selected-window))
               (eq (selected-window) (frame-root-window)))
          (switch-to-buffer-other-frame buffer)
        (switch-to-buffer buffer)))))

;;; Reimplementation of Buffer Menu: Sorting

;; These next three are functions we need to define our own version of only
;; because buffer-menu-sort has the default value of
;; Buffer-menu-buffer-column hard-coded into its definition.

(defun locales-menu-sort-by-column (&optional e)
  "Sort the buffer menu by the column clicked on."
  (interactive (list last-input-event))
  (when e
    (mouse-select-window e))
  (let* ((pos (event-start e))
         (obj (posn-object pos))
         (col (if obj
                  (get-text-property (cdr obj) 'column (car obj))
                (get-text-property (posn-point pos) 'column))))
    (locales-menu-sort col)))

(defun locales-menu-sort (column)
  "Sort the buffer menu by COLUMN."
  ;; It's not clear why the buff-menu version of this function is
  ;; interactive, since the user only runs it by calling
  ;; Buffer-menu-sort-by-column
  ;(interactive "P")
  (when column
    (setq column (prefix-numeric-value column))
    (when (< column 2)
      (setq column 2))
    (when (> column 5)
      (setq column 5)))
  (setq Buffer-menu-sort-column column)
  (let ((inhibit-read-only t) l buf m1 m2)
    (save-excursion
      (Buffer-menu-beginning)
      (while (not (eobp))
        (when (buffer-live-p (setq buf (get-text-property (+ (point) Buffer-menu-buffer-column) 'buffer)))
          (setq m1 (char-after)
                m1 (if (memq m1 '(?> ?D)) m1)
                m2 (char-after (+ (point) 2))
                m2 (if (eq m2 ?S) m2))
          (when (or m1 m2)
              (push (list buf m1 m2) l)))
        (forward-line)))
    (revert-buffer)
    (save-excursion
      (Buffer-menu-beginning)
      (while (not (eobp))
        (when (setq buf (assq (get-text-property (+ (point) Buffer-menu-buffer-column) 'buffer) l))
          (setq m1 (cadr buf)
                m2 (cadr (cdr buf)))
          (when m1
            (delete-char 1)
            (insert m1)
            (backward-char 1))
          (when m2
            (forward-char 2)
            (delete-char 1)
            (insert m2)))
        (forward-line)))))

(defun locales-menu-make-sort-button (name column)
  (when (equal column Buffer-menu-sort-column)
    (setq column nil))
  (propertize name
              'column column
              'help-echo (concat
                          (if Buffer-menu-use-header-line
                              "mouse-1, mouse-2: sort by "
                            "mouse-2, RET: sort by ")
                          (if column (downcase name) "visited order"))
              'mouse-face 'highlight
              'keymap locales-menu-sort-button-map))

;;; Reimplementation of Buffer Menu: Creating and Displaying the Menu

(defun locales-menu-revert-function (ignore1 ignore2)
  (or (eq buffer-undo-list t)
      (setq buffer-undo-list nil))
  ;; We can not use save-excursion here.  The buffer gets erased.
  (let ((ocol (current-column))
        (oline (progn (move-to-column Buffer-menu-buffer-column)
                      (get-text-property (point) 'buffer)))
        (prop (point-min))
        ;; do not make undo records for the reversion.
        (buffer-undo-list t))
    (locales-list-buffers-noselect Buffer-menu-files-only nil
                           locales-menu-global)
    (while (setq prop (next-single-property-change prop 'buffer))
      (when (eq (get-text-property prop 'buffer) oline)
        (goto-char prop)
        (move-to-column ocol)))
    (when (eobp)
          (goto-char (point-min))
          (unless Buffer-menu-use-header-line
            (forward-line 2)))))

(defun locales-buffer-menu-buffer+size (name size &optional name-props size-props)
  (if (> (+ (string-width name) (string-width size) 2)
          Buffer-menu-buffer+size-width)
      (setq name
            (let ((tail
                   (if (string-match "<[0-9]+>$" name)
                       (match-string 0 name)
                     "")))
              (concat (truncate-string-to-width
                       name
                       (- Buffer-menu-buffer+size-width
                          (max (string-width size) 3)
                          (string-width tail)
                          2))
                      Buffer-menu-short-ellipsis
                      tail)))
    ;; Don't put properties on (buffer-name).
    (setq name (copy-sequence name)))
  (add-text-properties 0 (length name) name-props name)
  (add-text-properties 0 (length size) size-props size)
  (let ((name+space-width (- Buffer-menu-buffer+size-width
                             (string-width size))))
    (concat name
            (propertize (make-string (- name+space-width (string-width name))
                                     32)
                        'display `(space :align-to ,(+ 5 name+space-width)))
            size)))

(defun locales-buffer-menu (&optional arg)
  "Make a menu of buffers so you can save, delete or select them.
With argument, show only buffers that are visiting files.
Type `?' after invocation to get help on commands available.
Type `q' to remove the buffer menu from the display.

In local mode, show only buffers local to the selected frame.  In
global mode show all buffers.  Type `a' to toggle between
modes. (For more information on controlling the composition of
the local buffer list, see the documentation of `locales-mode'.)

The first column shows `>' for a buffer you have marked to be
displayed, `D' for one you have marked for deletion, and `.' for
the current buffer.

The C column has a `.' for the buffer from which you came.  
The R column has a `%' if the buffer is read-only.  
The M column has a `*' if it is modified, or `S' if you have marked it for
saving.  
In global mode the L column has an `o' for buffers local to the
selected frame.  In local mode, the L column is suppressed: has
header line has `-' in the fourth column and each buffer's line is blank at
that column.

After this come the buffer name, its size in characters,
its major mode, and the visited file name (if any).

The appearance of the buffer menu can be controlled via the
variables `locales-open-in-local-mode', `locales-local-bit',
`locales-buffer-menu-mode-width',
`locales-buffer-menu-buffer+size-width',
`locales-buffer-menu-short-ellipsis',
`locales-buffer-menu-sort-column',
`locales-buffer-menu-use-header-line' and the face
`locales-buffer-menu-buffer'."
  (interactive "P")
;;;  (setq Buffer-menu-window-config (current-window-configuration))
  (switch-to-buffer (locales-list-buffers-noselect arg nil locales-open-in-global-mode))
  (message
   "Commands: a, -, +; d, s, x, u; f, o, 1, 2, m, v; ~, %%; q to quit; ? for help."))

(defun locales-buffer-menu-other-window (&optional arg)
  "Display a list of buffers in another window.
With the buffer list buffer, you can save, delete or select the buffers.
With argument, show only buffers that are visiting files.
Type ? after invocation to get help on commands available.
Type q to remove the buffer menu from the display.
For more information, see the function `buffer-menu'."
  (interactive "P")
  (switch-to-buffer-other-window (locales-list-buffers-noselect arg  nil locales-open-in-global-mode))
  (message
   "Commands: a, -, +; d, s, x, u; f, o, 1, 2, m, v; ~, %%; q to quit; ? for help."))

(defun locales-list-buffers (&optional files-only)
  "Display a list of names of existing buffers.
The list is displayed in a buffer named `*Buffer List*'.
Note that buffers with names starting with spaces are omitted.
Non-null optional arg FILES-ONLY means mention only file buffers.

For more information, see the function `locales-buffer-menu'."
  (interactive "P")
  (display-buffer (locales-list-buffers-noselect files-only nil locales-open-in-global-mode)))

(defun locales-list-buffers-noselect (&optional files-only buffer-list global)
  "Create and return a buffer with a list of names of existing buffers.
The buffer is named `*Buffer List*'.
Note that buffers with names starting with spaces are omitted.
Non-null optional arg FILES-ONLY means mention only file buffers.

By default this functions lists buffers local to the selected
frame.  Non-null optional arg GLOBAL means list all buffers.

If BUFFER-LIST is non-nil, it should be a list of buffers;
it means list those buffers and no others.

For more information, see the function `locales-buffer-menu'."
  (let* ((old-buffer (current-buffer))
         (standard-output standard-output)
         (mode-end (make-string (- Buffer-menu-mode-width
                                   2) ? ))
         (header (concat (if global "CRML " "CRM- ")
                         (locales-buffer-menu-buffer+size
                          (locales-menu-make-sort-button "Buffer" 2)
                          (locales-menu-make-sort-button "Size" 3))
                         "  "
                         (locales-menu-make-sort-button "Mode" 4) mode-end
                         (locales-menu-make-sort-button "File" 5) 
                         (if Buffer-menu-use-header-line "" "\n")))
         list desired-point)
    (when Buffer-menu-use-header-line
      (let ((pos 0))
        ;; Turn spaces in the header into stretch specs so they work
        ;; regardless of the header-line face.
        (while (string-match "[ \t]+" header pos)
          (setq pos (match-end 0))
          (put-text-property (match-beginning 0) pos 'display
                             ;; Assume fixed-size chars in the buffer.
                             (list 'space :align-to pos)
                             header)))
      ;; Try to better align the one-char headers.
      (put-text-property 0 4 'face 'fixed-pitch header)
      ;; Add a "dummy" leading space to align the beginning of the header
      ;; line with the beginning of the text (rather than with the left
      ;; scrollbar or the left fringe).  --Stef
      (setq header (concat (propertize " " 'display '(space :align-to 0))
                           header)))
    (with-current-buffer (locales-get-buffer-menu-buffer (selected-frame))
      (setq buffer-read-only nil)
      (erase-buffer)
      (setq standard-output (current-buffer))
      (unless Buffer-menu-use-header-line
        (let ((underline (if (char-displayable-p ?—) ?— ?-)))
          (insert header
                  (apply 'string
                         (mapcar (lambda (c)
                                   (if (memq c '(?\n ?\ )) c underline))
                                 header)))))
      ;; Collect info for every buffer we're interested in.
      (dolist (buffer (or (locales-filter-buffers buffer-list) 
                          (locales-buffer-list (selected-frame) global)))
        (with-current-buffer buffer
          (let ((name (buffer-name))
                (file buffer-file-name))
            (unless (and (null buffer-list)
                         (or
                          ;; Maybe don't mention buffers without files.
                          (and files-only (not file))
                          (string-match "^\\*Buffer List" name)))
              ;; Otherwise output info.
              (let ((mode (concat (format-mode-line mode-name nil nil buffer)
                                  (if mode-line-process
                                      (format-mode-line mode-line-process
                                                        nil nil buffer))))
                    (bits 
                     (string
                      (if (eq buffer old-buffer) ?. ?\ )
                      ;; Handle readonly status.  The output buffer
                      ;; is special cased to appear readonly; it is
                      ;; actually made so at a later date.
                      (if (or (eq buffer standard-output)
                              buffer-read-only)
                          ?% ?\ )
                      ;; Identify modified buffers.
                      (if (buffer-modified-p) ?* ?\ )
                      ;; local status
                      (if (and global 
                               (locales-local-p buffer))
                          locales-local-bit ? )
                      ;; Space separator.
                      ?\ )))
                (unless file
                  ;; No visited file.  Check local value of
                  ;; list-buffers-directory.
                  (when (and (boundp 'list-buffers-directory)
                             list-buffers-directory)
                    (setq file list-buffers-directory)))
                (push (list buffer bits name (buffer-size) mode file)
                      list))))))
      ;; Preserve the original buffer-list ordering, just in case.
      (setq list (nreverse list))
      ;; Place the buffers's info in the output buffer, sorted if necessary.
      (dolist (buffer
               (if Buffer-menu-sort-column
                   (sort list
                         (if (eq Buffer-menu-sort-column 3)
                             (lambda (a b)
                               (< (nth Buffer-menu-sort-column a)
                                  (nth Buffer-menu-sort-column b)))
                           (lambda (a b)
                             (string< (nth Buffer-menu-sort-column a)
                                      (nth Buffer-menu-sort-column b)))))
                 list))
        (when (eq (car buffer) old-buffer)
          (setq desired-point (point)))
        (insert (cadr buffer)
                ;; Put the buffer name into a text property
                ;; so we don't have to extract it from the text.
                ;; This way we avoid problems with unusual buffer names.
                (locales-buffer-menu-buffer+size (nth 2 buffer)
                                                 (int-to-string (nth 3 buffer))
                                                 `(buffer-name ,(nth 2 buffer)
                                                               buffer ,(car buffer)
                                                               face locales-buffer-menu-buffer
                                                               mouse-face highlight
                                                               help-echo "mouse-2: select this buffer"))
                "  "
                (if (> (length (nth 4 buffer))  Buffer-menu-mode-width)
                    (substring (nth 4 buffer) 0 Buffer-menu-mode-width)
                  (nth 4 buffer)))
        (when (nth 5 buffer)
          (indent-to (+ Buffer-menu-buffer-column Buffer-menu-buffer+size-width
                        Buffer-menu-mode-width 4) 1)
          (princ (abbreviate-file-name (nth 5 buffer))))
        (princ "\n"))

      ;; Buffer-menu-sort-column is a local variable we don't want killed
      ;; when the mode hook is run.
      (let ((sort-col Buffer-menu-sort-column))
        (locales-buffer-menu-mode)
        (setq Buffer-menu-sort-column sort-col))
      (setq Buffer-menu-files-only files-only)
      (when Buffer-menu-use-header-line
        (setq header-line-format header))
      ;; DESIRED-POINT doesn't have to be set; it is not when the
      ;; current buffer is not displayed for some reason.
      (when desired-point
        (goto-char desired-point))
      (set-buffer-modified-p nil)
      (setq locales-menu-global global)
      (current-buffer))))

(provide 'locales)

;;; locales.el ends here
