;;; locales.el --- a minor mode for frame-relative buffer lists

;; Copyright (c) 2011 Alp Aker

;; Author: Alp Aker <aker@pitt.edu>
;; Version: 0.84b
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
;; only buffers that are local to a frame.  Buffers can be added to and
;; removed from the frame-local buffer list from within or outside the buffer
;; menu.  The criteria for which buffers belong to a frame can be customized.

;; Installation
;; ============

;; Put this file in your load path and put:
;;
;;   (require 'locales)
;;
;; in your .emacs.  To toggle locales mode on and off, use the command
;;`locales-mode'.  To turn it on automatically when starting Emacs, put:
;;
;;  (locales-mode t) 
;;
;; in your .emacs.  

;; Usage
;; =====

;; Locales operates fairly transparently.  By default a new buffer-menu
;; initially lists only buffers local to the selected frame (we call this
;; "local mode").  By typing `a' one can toggle between listing only local
;; buffers and listing all buffers (we call the latter "global mode").

;; In global mode, there is a fourth column in the buffer menu after the C,
;; R, M columns: the L column.  Buffers local to the current frame are
;; indicated with an `o' in this column (this indicator bit can be changed
;; via `Buffer-menu-local-bit').  In local mode, the fourth column is
;; suppressed.  The variable `Buffer-menu-open-in-global-mode' can be used to
;; control whether the buffer menu opens in local or global mode.

;; All commands available in the default buffer menu are still available,
;; including sorting by buffer name, buffer size, etc., or toggling
;; files-only.

;; In addition to toggling between local and global modes, two other new
;; commands are available in the buffer menu.  By typing `]' a file can be
;; marked as to be added to the local buffer list.  By typing `[' a file can
;; be marked as to be removed from the local buffer list.  As with other
;; actions in the buffer menu, these changes take effect when
;; `Buffer-menu-execute' is called.  When a buffer is made non-local, it is
;; removed from any windows on the current frame displaying it and replaced
;; with another frame-local buffer.

;; Buffers can also be made non-local outside the buffer menu.  The command
;; `locales-make-non-local' can be called interactively to remove a buffer
;; from the local buffer list; when called with no argument, it acts on the
;; current buffer.  Locales does not, by default, bind this command to any
;; key-sequence.  To bind it to, e.g, C-c [, add the following two forms to
;; your .emacs:
;;
;;  (add-hook 'locales-mode-on-hook
;;             (global-set-key [(control ?c) ?\[] 'locales-make-non-local))
;;  (add-hook 'locales-mode-off-hook
;;             (global-set-key [(control ?c) ?\[] 'undefined))

;; Criteria That Control the Local Buffer List
;; ===========================================

;; The association between buffers and frames is dynamic:  if a buffer is
;; selected on a frame, then it is considered to be local to the
;; frame.  (Note, then, that a buffer can be local to more than one frame.)

;; In addition, several other criteria can be used to control the membership
;; of a frame's local buffer list:

;; o If `locales-include-scratch' is non-nil, then *scratch* always belongs
;;   to the local buffer list.

;; o If `locales-include-displayed-buffers' is non-nil, then buffers that are
;;   merely displayed on a frame are considered to be local to the frame,
;;   even if they have not been selected.

;; o Three variables control which buffers are local to a newly created
;;   frame:
;;
;;   - `locales-new-frames-inherit': If non-nil, a new frame's local buffer
;;      list includes (at least) the buffers that were local to the frame
;;      from which the new frame was created.
;;   - `locales-include-new-buffers': If non-nil, and the command that
;;      creates a new frame also creates new buffers, the new buffers belong
;;      to the new frame.  (This applies only to buffers that are created
;;      *after* the new frame is created.)
;;   - `locales-include-init-buffer':  If non-nil, then the buffer that is
;;      current when the command creating a new frame is called will belong to
;;      the new frame.  If nil, it will not--unless locales-new-frames-inherit
;;      is non-nil.  (NB:  If the buffer in question is displayed on the new
;;      frame when the frame-creating command terminates, it will still belong
;;      to the new frame's local list.)

;; Compatibility with Other Features
;; =================================

;; Locales is compatible with buff-menu+, as long as buff-menu+ is loaded
;; first.

;; Locales does not touch the buffer-list or buried-buffer-list frame
;; parameters, nor change the results of any call to the function
;; `buffer-list'.  (There is one exception to this rule:  If the buffer menu
;; is automatically displayed on another frame [e.g., when `pop-up-frames' is
;; non-nil], then the frame used to display the buffer menu has its
;; buffer-list and buried-buffer-list set to those of the frame from which
;; the buffer-menu command is called.)

;; The package sets each frame's buffer predicate so that `other-buffer' will
;; prefer buffers local to the frame; if another buffer predicate is present
;; when the package is enabled, locales saves that predicate and uses it as
;; well.

;; If you wish to access a frame's local buffer list directly for some
;; purpose, it is recommended that you use the list returned by the function
;; `locales-buffer-list'; don't use the value of the locales-buffer-list
;; frame parameter.  The latter can contain internal buffers (buffers whose
;; names starts with a space) and dead buffers; it is not guaranteed to
;; respect `locales-include-scratch'; and its order is meaningless.  The list
;; returned by `locales-buffer-list' will contain only live, non-internal
;; buffers, include *scratch* if necessary, and be sorted stably by selection
;; order on the current frame.

;;; Code:

(when (< emacs-major-version 22)
  (error "Locales requires version 22 or later"))

;;; Declarations

;; User Options

(defgroup locales nil
  "Extend buffer-menu to allow listing of a frame's local buffer list only."
  :group 'convenience)

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
  :group 'locales
  :type 'boolean)

(defcustom locales-include-displayed-buffers t
  "If non-nil, buffers displayed on a frame belong to the local buffer list.
If nil, buffers are added to the local buffer list only if they
are selected, not merely displayed."
  :group 'locales
  :type 'boolean)

(defcustom locales-include-new-buffers nil
  "Include new buffers in a new frame's buffer list.
If non-nil, and the command that creates a new frame also creates
new buffers, those buffers will belong to the new frame's buffer
list, even if they have not been selected.  (Buffers created
before the new frame is created are not affected by this
variable.)"
  :group 'locales
  :type 'boolean)

(defcustom locales-new-frames-inherit nil
  "Whether a new frame inherits the local buffer list of its \"parent\".
If non-nil, the local buffer list of a newly created frame will
include (at least) those buffers that were local to the frame
that was selected when the frame-creating command was called."
  :group 'locales
  :type 'boolean)

(defcustom locales-include-init-buffer nil
  "Whether a new frame's local list includes the last buffer before creation.
If non-nil, then the buffer that is current when a frame-creating
command is called--the \"init buffer\"--belongs to the new
frame's local buffer list.  If nil, it does not.  Note:  If the
init buffer is displayed on the new frame after the
frame-creating command terminates, then it belongs to the new
frame's local buffer list even if this variable is nil."
  :group 'locales
  :type 'boolean)

(defvar locales-mode nil 
  "True if locales mode is enabled.

Do not set this variable directly.  Use the command
`locales-mode' instead.")

;; Internal Variables

;; Used in initializing the local buffer list of a newly created frame.
(defvar locales-parent-buffer-list nil)
(defvar locales-prev-buffers nil)
(defvar locales-init-buffer nil)
(defvar locales-new-frame nil)
(defvar locales-no-list-initialization nil)

;;; Mode Definition

(defun locales-mode (&optional arg) 
  "Toggle locales mode on and off.

qWith argument ARG, turn locales mode on if and only if ARG is t or positive.

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
 and `locales-include-new-buffers'."
  (interactive "P")
  (setq locales-mode (if (not arg) 
                         (not locales-mode)
                       (> (prefix-numeric-value arg) 0)))
  (if locales-mode
      (progn
        (dolist (frame (frame-list))
          (locales-set-up-frame frame))
        (setq Buffer-menu-buffer-column 5)
        (add-hook 'window-configuration-change-hook 'locales-window-change)
        (add-hook 'before-make-frame-hook 'locales-before-make-frame)
        (add-hook 'after-make-frame-functions 'locales-after-make-frame)
        (run-hooks 'locales-mode-on-hook)
        (message "Per-frame buffer menus are enabled"))
    (dolist (frame (frame-list))
      (locales-clear-frame frame))
    (setq Buffer-menu-buffer-column 4)
    (remove-hook 'window-configuration-change-hook 'locales-window-change)
    (remove-hook 'before-make-frame-hook 'locales-before-make-frame)
    (remove-hook 'after-make-frame-functions 'locales-after-make-frame)
    (run-hooks 'locales-mode-off-hook)
    (message "Per-frame buffer menus are disabled"))
  ;; In case we toggle the mode while the buffer list is visible on selected
  ;; frame.
  ;;; (let* ((buf (get-buffer "*Buffer List*"))
  ;;;        (wins (get-buffer-window-list buf 'no-minibuf)))
  ;;;   (when (and buf wins)
  ;;;     (with-current-buffer buf
  ;;;       (revert-buffer))))
  (run-mode-hooks 'locales-mode-hook))

;;; Initialization and Clean Up

;; Set the local buffer list initially to the frame-parameter buffer-list.  This
;; is used only for frames that already exist when locales mode is enabled.
(defun locales-set-up-frame (frame)
  (locales-set-buffer-predicate frame)
  (set-frame-parameter frame
                       'locales-buffer-list 
                       (append (frame-parameter frame 'buffer-list)
                               (locales-copy-list 
                                (frame-parameter frame 'buried-buffer-list))))
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

;; Prepare data for initializing a new frame's local buffer list.
(defun locales-before-make-frame ()
  (unless locales-no-list-initialization
    (setq locales-init-buffer (current-buffer)
          locales-prev-buffers (buffer-list)
          locales-parent-buffer-list (locales-copy-list 
                                      (frame-parameter (selected-frame) 
                                                       'locales-buffer-list)))))

;; Set the buffer predicate and arrange for buffer list initialization to
;; take place after command that creates the new frame terminates.
(defun locales-after-make-frame (frame)
  (locales-set-buffer-predicate locales-new-frame)
  (unless locales-no-list-initialization
    (add-hook 'pre-command-hook 'locales-update-new-frame)
    (setq locales-new-frame frame)))

;; Set buffer predicate and initialize local buffer list.
(defun locales-update-new-frame ()
  (remove-hook 'pre-command-hook 'locales-update-new-frame)
  (unwind-protect
      (when (frame-live-p locales-new-frame)
        (when locales-new-frames-inherit
          (locales-push-buffers locales-parent-buffer-list locales-new-frame))
        (when locales-include-new-buffers
          (locales-push-buffers (locales-set-minus locales-prev-buffers (buffer-list))
                                locales-new-frame))
        (unless (or locales-include-init-buffer
                    locales-new-frames-inherit
                    (memq locales-init-buffer 
                          (mapcar #'(lambda (x) (window-buffer x))
                                  (window-list locales-new-frame 'no-minibuf))))
          (locales-pop-buffer locales-init-buffer locales-new-frame)))
    (setq locales-new-frame nil
          locales-initial-buffer-list nil
          locales-init-buffer nil
          locales-prev-buffers nil)))

(defun locales-clear-frame (frame)
  (set-frame-parameter frame 
                       'buffer-predicate 
                       (frame-parameter frame 'locales-saved-buffer-predicate))
  (set-frame-parameter frame 'locales-saved-buffer-predicate nil)
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
(defun locales-pop-buffer (buf frame)
  (set-frame-parameter frame
                       'locales-buffer-list
                       (delq buf (frame-parameter frame 'locales-buffer-list))))

;; Interactive version of preceding.  Remove BUF from FRAME's local buffer
;; list.  In addition, if any windows on FRAME are currently displaying BUF,
;; replace BUF in those windows with some other buffer local to FRAME.
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

;; Add BUF to FRAME's local buffer list if not already present.
(defun locales-push-buffer (buf frame)
  (let ((local-bufs (frame-parameter frame 'locales-buffer-list)))
    (unless (memq buf local-bufs)
      (set-frame-parameter frame 'locales-buffer-list (cons buf local-bufs)))))

(defun locales-push-buffers (bufs frame)
  (dolist (buf bufs)
    (locales-push-buffer buf frame)))

;; When called with argument GLOBAL non-nil, return the normal buffer list,
;; sorted by selection order for the current frame.  With GLOBAL nil, update
;; the local buffer list and return it, also sorted.
(defun locales-buffer-list (frame &optional global)
  ;; Filter out internal buffers.
  (locales-filter-buffers
   (if global
       (buffer-list frame)
     ;; Include scratch if necessary.
     (when locales-include-scratch
       (locales-push-buffer (get-buffer-create "*scratch*") frame))
     ;; Remove dead buffers.  (Should be able to do this via kill-buffer-hook,
     ;; but there are a few corner cases that let dead buffers slip through that
     ;; way.)
     (set-frame-parameter frame 
                          'locales-buffer-list
                          (delq nil 
                                (mapcar #'(lambda (x) (if (buffer-live-p x) x))
                                        (frame-parameter frame
                                                         'locales-buffer-list))))
     ;; Return the local buffer list sorted appropriately for this frame.
     (locales-sort-buffers frame 
                           (locales-copy-list 
                            (frame-parameter frame 'locales-buffer-list))))))

;;; Utilities and Predicates

;; A simple form of copy-list, which we define ourselves in order to avoid
;; requiring the entire cl library at run-time.
(defun locales-copy-list (l)
  (mapcar #'identity l))

;; Return a list in which BUFS are sorted according to selection order on
;; FRAME.
(defun locales-sort-buffers (frame bufs)
  (let ((l (buffer-list frame)))
    (sort bufs 
          #'(lambda (x y) (> (length (memq x l))
                             (length (memq y l)))))))

;; Remove internal buffers from BUFS.
(defun locales-filter-buffers (bufs)
  (delq nil
        (mapcar #'(lambda (x) (if (not (string-match "^ " (buffer-name x))) x))
                bufs)))

(defun locales-set-minus (set1 set2)
  (dolist (element set1)
    (setq set2 (delq element set2)))
  set2)

;; Set as the buffer predicate for all frames when locales is enabled.  Check
;; BUF against any other predicate that might have been present, then check
;; whether BUF is local to the current frame.  Return t if both tests
;; succeed.
(defun locales-ok-to-display-p (buf)
  (let ((other-pred (frame-parameter nil 'locales-saved-buffer-pred)))
    (and (locales-local-p buf)
         (if (functionp other-pred)
             (funcall other-pred buf)
           t))))

;; Check if BUF is local to FRAME.
(defun locales-local-p (buf &optional frame)
  (memq buf (frame-parameter frame 'locales-buffer-list)))

;;; Changes to the Buffer Menu:  Keymap and Declarations

(let ((map Buffer-menu-mode-map))
  (define-key map "a" 'Buffer-menu-toggle-global)
  (define-key map "]" 'Buffer-menu-make-local)
  (define-key map "[" 'Buffer-menu-make-non-local))

(defcustom Buffer-menu-open-in-global-mode nil 
  "If non-nil, and locales-mode is enabled, the buffer menu opens in global mode."
  :group 'locales
  :group 'Buffer-menu
  :type 'boolean)

(defcustom Buffer-menu-local-bit ?o 
  "Charcater used to indicate frame-local buffers in the buffer menu."
  :group 'locales
  :group 'Buffer-menu
  :type 'character)

;; Non-nil if the buffer-menu lists all buffers.  Nil if the buffer-menu
;; lists only frame-visible buffers.  This variable determines whether
;; reverting the buffer lists only frame-visible buffers.  It affects both
;; manual reverting and reverting by Auto Revert Mode.
(defvar Buffer-menu-global nil)

;;; Changes to the Buffer Menu:  New Buffer Menu Commands

(defun Buffer-menu-toggle-global (arg)
  "Toggle whether the current buffer-menu displays only buffers local to this frame.
With a positive or true ARG display only local buffers.  With
zero, negative, or nil ARG, display all buffers.

If locales-mode is disabled, do nothing."
  (interactive "P")
  (when locales-mode
    (setq Buffer-menu-global
          (cond ((not arg) (not Buffer-menu-global))
                ((<= (prefix-numeric-value arg) 0) t)))
    (revert-buffer)))

(defun Buffer-menu-make-local (&optional arg)
  "Mark buffer on this line to be made local to this frame by \\<Buffer-menu-mode-map>\\[Buffer-menu-execute] command.
Prefix arg is how many buffers to make local.  Negative arg means
work backwards.

If locales-mode is disabled, do nothing."
  (interactive "p")
  (when locales-mode
    (when (Buffer-menu-no-header)
      (let ((buffer-read-only nil))
        (when (or (null arg) (= arg 0))
          (setq arg 1))
        (while (> arg 0)
          (forward-char 3)
          (when (or (not (locales-local-p (Buffer-menu-buffer nil)))
                    (looking-at "-"))
            (delete-char 1)
            (insert 
             (if (locales-local-p (Buffer-menu-buffer nil))
                 (if Buffer-menu-global
                     Buffer-menu-local-bit
                   " ")
               "+")))
          (forward-line 1)
          (setq arg (1- arg)))
        (while (and (< arg 0)
                    (Buffer-menu-no-header))
          (forward-char 3)
          (when (or (not (locales-local-p (Buffer-menu-buffer nil)))
                    (looking-at "-"))
            (delete-char 1)
            (insert 
             (if (locales-local-p (Buffer-menu-buffer nil))
                 (if Buffer-menu-global
                     Buffer-menu-local-bit
                   " ")
               "+")))
          (forward-line -1)
          (setq arg (1+ arg)))))))

(defun Buffer-menu-make-non-local (&optional arg)
  "Mark buffer on this line to be made non-local by \\<Buffer-menu-mode-map>\\[Buffer-menu-execute] command.
Prefix arg is how many buffers to to make non-local.
Negative arg means work backwards.

If locales-mode is disabled, do nothing."
  (interactive "p")
  (when locales-mode
    (when (Buffer-menu-no-header)
      (let ((buffer-read-only nil))
        (when (or (null arg) (= arg 0))
          (setq arg 1))
        (while (> arg 0)
          (forward-char 3)
          (when (or (locales-local-p (Buffer-menu-buffer nil))
                    (looking-at "\\+"))
            (delete-char 1)
            (insert (if (or (not Buffer-menu-global)
                            (locales-local-p (Buffer-menu-buffer nil)))
                        "-"
                      " ")))
          (forward-line 1)
          (setq arg (1- arg)))
        (while (and (< arg 0)
                    (Buffer-menu-no-header))
          (forward-char 3)
          (when (or (locales-local-p (Buffer-menu-buffer nil))
                    (looking-at "\\+"))
            (delete-char 1)
            (insert (if (or (not Buffer-menu-global)
                            (locales-local-p (Buffer-menu-buffer nil)))
                        "-"
                      " ")))
          (forward-line -1)
          (setq arg (1+ arg)))))))

;;;  Changes to the Buffer Menu: Redefined Buffer Menu Commands 

(defun Buffer-menu-unmark (&optional backup)
  "Cancel all requested operations on buffer on this line and move down.
Optional ARG means move up."
  (interactive "P")
  (when (Buffer-menu-no-header))
  (let* ((buf (Buffer-menu-buffer t))
         (mod (if (buffer-modified-p buf)
                  "*" " "))
         (readonly (if (with-current-buffer buf buffer-read-only)
                       "%" " "))
         (local (if locales-mode
                    (if (and Buffer-menu-global
                             (locales-local-p buf))
                        (char-to-string Buffer-menu-local-bit) " ")
                  ""))
         (buffer-read-only nil))
    (delete-char (if locales-mode 4 3))
    (insert (concat " " readonly mod local)))
  (forward-line (if backup -1 1)))

(defun Buffer-menu-execute ()
  "Save and/or delete buffers marked with \\<Buffer-menu-mode-map>\\[Buffer-menu-save] or \\<Buffer-menu-mode-map>\\[Buffer-menu-delete] commands.

If locales-mode is enabled, also make frame-local and/or make frame-non-local  
buffers marked with \\<Buffer-menu-mode-map>\\[Buffer-menu-make-local] or \\<Buffer-menu-mode-map>\\[Buffer-menu-make-non-local] commands."
  (interactive)
  (when locales-mode
    (save-excursion
      (Buffer-menu-beginning)
      (let ((buffer-read-only nil))
        (while (re-search-forward "^...\\+" nil t)
          (forward-char -1)
          (let ((buf (Buffer-menu-buffer t)))
            (locales-push-buffer buf (selected-frame))
            (delete-char 1)
            (insert Buffer-menu-local-bit)))))
    (save-excursion
      (Buffer-menu-beginning)
      (let ((buffer-read-only nil))
        (while (re-search-forward "^...-" nil t)
          (let ((buf (Buffer-menu-buffer t)))
            (locales-pop-buffer buf (selected-frame))
            (if Buffer-menu-global
                (progn
                  (forward-char -1)
                  (let ((props (text-properties-at (point))))
                    (delete-char 1)
                    (insert (apply 'propertize " " props))))
              (forward-char -4)
              (delete-region (point) (progn (forward-line 1) (point)))
              (unless (bobp)
                (forward-char -1))))))))
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
              (forward-char -1))))))))

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

;; Buffer-menu-sort has the default value of Buffer-menu-buffer-column
;; hard-coded into its definition.  We change that.
(defun Buffer-menu-sort (column)
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

(defun Buffer-menu-revert-function (ignore1 ignore2)
  (or (eq buffer-undo-list t)
      (setq buffer-undo-list nil))
  ;; We can not use save-excursion here.  The buffer gets erased.
  (let ((ocol (current-column))
        (oline (progn (move-to-column Buffer-menu-buffer-column)
                      (get-text-property (point) 'buffer)))
        (prop (point-min))
        ;; do not make undo records for the reversion.
        (buffer-undo-list t))
    (with-current-buffer (window-buffer)
      (list-buffers-noselect Buffer-menu-files-only nil
                             Buffer-menu-global))
    (while (setq prop (next-single-property-change prop 'buffer))
      (when (eq (get-text-property prop 'buffer) oline)
        (goto-char prop)
        (move-to-column ocol)))
    (when (eobp)
      (goto-char (point-min))
      (unless Buffer-menu-use-header-line
        (forward-line 2)))))

(defun buffer-menu (&optional files-only)
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
its major mode, and the visited file name (if any)."
  (interactive "P")
  (switch-to-buffer (list-buffers-noselect files-only nil Buffer-menu-open-in-global-mode))
  (message "Commands: a, [, ]; d, s, x, u; f, o, 1, 2, m, v; ~, %%; q to quit; ? for help."))

;; Brute hack to accomodate non-nil pop-up-frames and similar cases.
(defun Buffer-menu-show-buffer-list (fn files-only)
  (if (not locales-mode)
      (funcall fn (list-buffers-noselect files-only))
    (let* ((locales-no-list-initialization t); disable normal new frame initialization
           (oframe (selected-frame))
           (res (funcall fn (list-buffers-noselect files-only 
                                                   nil 
                                                   Buffer-menu-open-in-global-mode)))
           ;; Some buffer display functions just raise the frame they display
           ;; the buffer on, rather than selecting it.  Those return the
           ;; window they use, though, so we detect the display frame that
           ;; way.
           (frame (if (windowp res)
                      (window-frame res)
                    (selected-frame))))
      ;; If we displayed the buffer menu on a different frame, reset the frame
      ;; parameters on the buffer menu's frame.
      (unless (eq frame oframe)
        (dolist (param '(buffer-list buried-buffer-list locales-buffer-list))
          (set-frame-parameter frame 
                               param
                               (locales-copy-list (frame-parameter oframe param))))))))

(defun buffer-menu-other-window (&optional files-only)
  "Display a list of buffers in another window.
With the buffer list buffer, you can save, delete or select the buffers.
With argument, show only buffers that are visiting files.
Type ? after invocation to get help on commands available.
Type q to remove the buffer menu from the display.
For more information, see the function `buffer-menu'."
  (interactive "P")
  (Buffer-menu-show-buffer-list 'switch-to-buffer-other-window files-only)
  (message "Commands: a, [, ]; d, s, x, u; f, o, 1, 2, m, v; ~, %%; q to quit; ? for help."))

(defun list-buffers (&optional files-only)
  "Display a list of names of existing buffers.
The list is displayed in a buffer named `*Buffer List*'.
Note that buffers with names starting with spaces are omitted.
Non-null optional arg FILES-ONLY means mention only file buffers.
v
For more information, see the function `locales-buffer-menu'."
  (interactive "P")
  (Buffer-menu-show-buffer-list 'display-buffer files-only))

(defun list-buffers-noselect (&optional files-only buffer-list global)
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
         (header (concat (if locales-mode
                             (if global "CRML " "CRM- ")
                           "CRM ")
                         (Buffer-menu-buffer+size
                          (Buffer-menu-make-sort-button "Buffer" 2)
                          (Buffer-menu-make-sort-button "Size" 3))
                         "  "
                         (Buffer-menu-make-sort-button "Mode" 4) mode-end
                         (Buffer-menu-make-sort-button "File" 5) 
                         "\n"))
         list desired-point)
    (when Buffer-menu-use-header-line
      (let ((pos 0))
        ;; Turn spaces in the header into stretch specs so they work
        ;; regardless of the header-line face.
        (while (string-match "[ \t\n]+" header pos)
          (setq pos (match-end 0))
          (put-text-property (match-beginning 0) pos 'display
                             ;; Assume fixed-size chars in the buffer.
                             (list 'space :align-to pos)
                             header)))
      ;; Try to better align the one-char headers.
      (put-text-property 0 (if locales-mode 4 3) 'face 'fixed-pitch header)
      ;; Add a "dummy" leading space to align the beginning of the header
      ;; line with the beginning of the text (rather than with the left
      ;; scrollbar or the left fringe).  --Stef
      (setq header (concat (propertize " " 'display '(space :align-to 0))
                           header)))
    (with-current-buffer (get-buffer-create "*Buffer List*")
      (setq buffer-read-only nil)
      (erase-buffer)
      (setq standard-output (current-buffer))
      (unless Buffer-menu-use-header-line
        (let ((underline (if (char-displayable-p #x2014) #x2014 ?-)))
          (insert header
                  (apply 'string
                         (mapcar #'(lambda (c)
                                     (if (memq c '(?\n ?\s)) c underline))
                                 header)))))
      ;; Collect info for every buffer we're interested in.
      (dolist (buffer (or buffer-list
                          (and locales-mode 
                               (locales-buffer-list (selected-frame) global))
                          (buffer-list (and (boundp 'Buffer-menu-use-frame-buffer-list)
                                            Buffer-menu-use-frame-buffer-list))))
        (with-current-buffer buffer
          (let ((name (buffer-name))
                (file buffer-file-name))
            (unless (and (null buffer-list)
                         (or
                          ;; Don't mention internal buffers.
                          (and (string= (substring name 0 1) " ") (null file))
                          ;; Maybe don't mention buffers without files.
                          (and files-only (not file))
                          (string= name "*Buffer List*")))
              ;; Otherwise output info.
              (let ((mode (concat (format-mode-line mode-name nil nil buffer)
                                  (if mode-line-process
                                      (format-mode-line mode-line-process
                                                        nil nil buffer))))
                    (bits 
                     (concat
                      (if (eq buffer old-buffer) "." " ")
                      ;; Handle readonly status.  The output buffer
                      ;; is special cased to appear readonly; it is
                      ;; actually made so at a later date.
                      (if (or (eq buffer standard-output)
                              buffer-read-only)
                          "%" " ")
                      ;; Identify modified buffers.
                      (if (buffer-modified-p) "*" " ")
                      ;; local status
                      (if locales-mode
                          (if (and global 
                                   (locales-local-p buffer))
                              (char-to-string Buffer-menu-local-bit) " ")
                        "")
                      ;; Space separator.
                      " ")))
                (unless file
                  ;; No visited file.  Check local value of
                  ;; list-buffers-directory and, for Info buffers,
                  ;; Info-current-file.
                  (cond ((and (boundp 'list-buffers-directory)
                              list-buffers-directory)
                         (setq file list-buffers-directory))
                        ((eq major-mode 'Info-mode)
                         (setq file Info-current-file)
                         (cond
                          ((equal file "dir")
                           (setq file "*Info Directory*"))
                          ((eq file 'apropos)
                           (setq file "*Info Apropos*"))
                          ((eq file 'history)
                           (setq file "*Info History*"))
                          ((eq file 'toc)
                           (setq file "*Info TOC*"))
                          ((not (stringp file))  ;; avoid errors
                           (setq file nil))
                          (t
                           (setq file (concat "("
                                              (file-name-nondirectory file)
                                              ") "
                                              Info-current-node)))))))
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
                (let ((name (nth 2 buffer))
                      (size (int-to-string (nth 3 buffer))))
                  (Buffer-menu-buffer+size name size
                                           `(buffer-name ,name
                                                         buffer ,(car buffer)
                                                         font-lock-face buffer-menu-buffer
                                                         mouse-face highlight
                                                         help-echo
                                                         ,(if (>= (length name)
                                                                  (- Buffer-menu-buffer+size-width
                                                                     (max (length size) 3)
                                                                     2))
                                                              name
                                                            "mouse-2: select this buffer"))))
                "  "
                (if (> (string-width (nth 4 buffer)) Buffer-menu-mode-width)
                    (truncate-string-to-width (nth 4 buffer)
                                              Buffer-menu-mode-width)
                  (nth 4 buffer)))
        (when (nth 5 buffer)
          (indent-to (+ Buffer-menu-buffer-column Buffer-menu-buffer+size-width
                        Buffer-menu-mode-width 4) 1)
          (princ (abbreviate-file-name (nth 5 buffer))))
        (princ "\n"))
      (Buffer-menu-mode)
      (setq Buffer-menu-files-only files-only)
      (when (boundp 'Buffer-menu--buffers)
        (setq Buffer-menu--buffers buffer-list))
      (when Buffer-menu-use-header-line
        (setq header-line-format header))
      ;; DESIRED-POINT doesn't have to be set; it is not when the
      ;; current buffer is not displayed for some reason.
      (when desired-point
        (goto-char desired-point))
      (set-buffer-modified-p nil)
      (setq Buffer-menu-global global)
      (current-buffer))))

;;; Additions to Electric Buffer Menu

(require 'ebuff-menu)

(let ((map electric-buffer-menu-mode-map))
  (define-key map "a" 'Buffer-menu-toggle-global)
  (define-key map "]" 'Buffer-menu-make-local)
  (define-key map "[" 'Buffer-menu-make-non-local))

(add-hook 'electric-buffer-menu-mode-hook 
          (lambda () (set (make-local-variable 'revert-buffer-function)
                          'electric-buffer-menu-revert-function)))

(defun electric-buffer-menu-revert-function (ignore1 ignore2)
  (or (eq buffer-undo-list t)
      (setq buffer-undo-list nil))
  ;; We can not use save-excursion here.  The buffer gets erased.
  (let ((ocol (current-column))
        (oline (progn (move-to-column Buffer-menu-buffer-column)
                      (get-text-property (point) 'buffer)))
        (prop (point-min))
        ;; do not make undo records for the reversion.
        (buffer-undo-list t))
    (with-current-buffer (window-buffer)
      (list-buffers-noselect Buffer-menu-files-only nil global)
      (Electric-buffer-menu-mode))
    (while (setq prop (next-single-property-change prop 'buffer))
      (when (eq (get-text-property prop 'buffer) oline)
        (goto-char prop)
        (move-to-column ocol)))
    (when (eobp)
      (goto-char (point-min))
      (unless Buffer-menu-use-header-line
        (forward-line 2)))
    (setq loop-condition nil
          loop-state (cons (point-min) (point-max)))))

(provide 'locales)

;;; locales.el ends here
