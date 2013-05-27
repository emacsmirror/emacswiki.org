;;; dired-efap.el --- Edit Filename At Point in a dired buffer

;; Copyright (C) 2001, 2013 Juan-Leon Lahoz

;; Filename: dired-efap.el
;; Author: Juan-Leon Lahoz <juanleon1@gmail.com>
;; Version: 0.8
;; Keywords: dired, environment, files, renaming

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 2, or (at your option) any
;; later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License along
;; with GNU Emacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Commentary:

;; dired-efap.el allows the user to edit the filename at point, by hitting
;; a key (like f2) or double-clicking it. The name is edited in the dired
;; buffer, and the renaming takes effect when the user hits the RET
;; key. Only the name of the file at point is tangible and editable, and it
;; uses an special font.
;;
;; This package provides a similar user interface to renaming files to the
;; interface used by some graphical file managers (or "explorers", however they
;; are called).
;;
;; The idea for this file was originated from a suggestion I received from Eli
;; Tziperman to improve wdired.
;;
;; Comments, bug reports, etc. welcomed

;;; Usage:

;; Add this file to your load-path and this line to your config file:
;;
;; (require 'dired-efap)
;; (define-key dired-mode-map [f2] 'dired-efap)
;;
;; To edit a name you can hit f2 (or the mapping of your choosing) or
;; double-click over it. Pressing RET the file is actually renamed.  C-g
;; aborts.
;;
;; The behavior of the mouse can be customized. There are three options:
;; double-click to edit, click once in the file where the cursor is (this
;; includes double-click, because the first click moves the cursor) and
;; disallow the use of the mouse to edit names. See and customize the
;; variable `dired-efap-use-mouse' to change the default behavior of
;; double-click to edit
;;
;; You can customize also the face of the name being edited. This face is
;; called `dired-efap-face'
;;
;; Type M-x customize-group RET dired-efap if you want make changes to
;; the default behavior.

;;; Change Log:

;; 0.8
;;
;; - Now `dired-efap-face' works on modern Emacs versions, and the default
;;   face works well both with dark and light backgrounds, as well as text
;;   terminals.
;;
;; - Now, initial filename selection works properly.  Variable to customize
;;   the behavior is renamed to `dired-efap-initial-filename-selection'.
;;
;; - A lot of dirty hacks removed from code

;;; Homepage

;; https://github.com/juan-leon/dired-efap

;;; Code:

(eval-and-compile
  (require 'dired)
  (autoload 'dired-do-create-files-regexp "dired-aux"))

(defgroup dired-efap nil
  "Mode to rename a file by editing its name at point in a dired buffer."
  :group 'dired)

(defcustom dired-efap-use-mouse t
  "*Control the use on the mouse for renaming filename at point. If
you use the mouse to access this functionality, you'll lose the current
use of the left button (usually drag region, witch has no great
utility in dired mode) other than moving the cursor. The others mouse
buttons are unaffected. Possibles values:

If t, you can rename a file double-clicking in its line in the
buffer.

If `selected', you can rename a by clicking in its name when the
cursor is already over it. It includes double-clicking the name.

If nil, the mouse is not used for renaming files at point."
  :type '(choice (const :tag "Double click on file" t)
                 (const :tag "Click on selected file" selected)
                 (other :tag "Do not use mouse" nil))
  :group 'dired-efap)

(defcustom dired-efap-initial-filename-selection 'no-extension
  "*Control if the file name will be selected when starting the edition.

Setting this to non-nil is handy for those users that want to be able
to easily replace the whole filename.  If t, the whole name is
selected.  Use `no-extension' for selecting only the name without
extension."
  :type '(choice (const :tag "Select the whole filename" t)
                 (const :tag "Select name without extension" no-extension)
                 (other :tag "Do not select name" nil))
  :group 'dired-efap)

(defface dired-efap-face
  (if (>= emacs-major-version 21)
      '((((supports '(:box t)) (background dark))
         (:box (:line-width 2 :color "white" :style pressed-button)))
        (((supports '(:box t)) (background light))
         (:box (:line-width 2 :color "black" :style pressed-button)))
        (t (:inverse-video t)))
    '((t (:inverse-video t))))
  "Face used for filenames being edited."
  :group 'dired-efap)

(defvar dired-efap-face 'dired-efap-face)

(defvar dired-efap-mode-hooks nil
  "Hooks run when changing to dired-efap mode.")

(defvar dired-efap-load-hooks nil
  "Hooks run after loading dired-efap code.")

(defvar dired-efap-old-mouse-func
  (let ((current-map (current-local-map))
        (func))
    (use-local-map dired-mode-map)
    (setq func (key-binding [down-mouse-1]))
    (use-local-map current-map)
    func)
  "Original function bounded to down-mouse-1 en dired mode.")

(define-key dired-mode-map [down-mouse-1] 'dired-efap-click)

(defvar dired-efap-mode-map
  (let ((map (make-sparse-keymap)))
    (mapc #'(lambda (arg)
              (define-key map arg 'dired-efap-finish))
          '("\C-c\C-c" [remap newline] [remap newline-and-indent]
            [remap open-line] [remap save-buffer]))
    (mapc #'(lambda (arg)
              (define-key map arg 'dired-efap-abort))
          '("\C-x\C-q" [remap keyboard-quit] [remap keyboard-escape-quit]))
    (define-key map [mouse-1]  'dired-efap-mouse-clicked)
    map)
  "Keymap used in `dired-efap-mode'.")


;; Local variables
(defvar dired-efap-overlay)

(defun dired-efap-click (event)
  "Move to the point and, depending of the value of
`dired-efap-use-mouse', if the click has been double and the
previous position of the point, edit filename at point.

See `dired-efap-use-mouse' and `dired-efap'"
  (interactive "e")
  (if dired-efap-use-mouse
      (if (equal dired-efap-use-mouse 'selected)
          (let ((previous-file (dired-get-filename nil t)))
            (mouse-set-point event)
            (if (and previous-file
                     (equal previous-file (dired-get-filename nil t)))
                (dired-efap t)))
        (if (member 'double (event-modifiers event))
            (dired-efap t)))
    (funcall dired-efap-old-mouse-func event)))

(defun dired-efap-mouse-clicked (event)
  "Finish the edition of the filename at point, performing the
necessary changes in disk. This only happens if the click is outside
the filename but in the dired buffer. Anyway, point is moved to the
click point. See also `dired-efap' and `dired-efap-mode'"
  (interactive "e")
  (let ((point-clicked (posn-point (event-start event))))
    (if (or (< point-clicked (overlay-start dired-efap-overlay))
            (> point-clicked (overlay-end dired-efap-overlay)))
        (dired-efap-finish)))
  (mouse-set-point event))

(defun dired-efap-mode ()
  "\\<dired-efap-mode-map>Mode for rename the file at point. Edit the
name of the file at point and then press RET to rename it. To abort
the changes, use \\[dired-efap-abort]."
  (interactive)
  (error
   "This mode can be enabled only by `dired-efap-change-to-dired-efap-mode'"))
(put 'dired-efap-mode 'mode-class 'special)

(defun dired-efap (&optional from-mouse)
  "Change the mode of a dired buffer to another in witch the filename
at point becomes editable.  Press RET to actually rename the file or
directory in disk, and C-g to abort.

If FROM-MOUSE is not nil, the mode is being set because of a mouse event."
  (interactive)
  (let ((original-name (dired-get-filename 'no-dir)))
    (if (string-match "^\\.\\.?$" original-name)
        (error ". and .. cannot be edited"))
    (use-local-map dired-efap-mode-map)
    (setq buffer-read-only nil)
    (dired-unadvertise default-directory)
    (setq major-mode 'dired-efap-mode)
    (setq mode-name "Filename edit")
    (set (make-local-variable 'revert-buffer-function) 'dired-efap-abort)
    (message "Press RET when finished")
    (add-hook 'post-command-hook 'dired-efap--keep-in-name t t)
    (dired-efap--set-properties original-name)
    (buffer-disable-undo)
    (buffer-enable-undo)
    (run-hooks dired-efap-mode-hooks))
  (if from-mouse
      ;; Move down the mouse, to have a better visibility.
      (set-mouse-position (selected-frame) (cadr (mouse-position))
                          (1+ (cddr (mouse-position)))))
  ;; Using a timer avoids emacs post command processing to mess with the
  ;; selection and cursor position we want
  (if dired-efap-initial-filename-selection
      (run-with-timer (if from-mouse 0.1 0) nil
                      'dired-efap--select-filename)))

(defun dired-efap--select-filename ()
  (push-mark (overlay-start dired-efap-overlay) t t)
  (goto-char (overlay-end dired-efap-overlay))
  (if (equal dired-efap-initial-filename-selection 'no-extension)
      (let ((new-point 
             (search-backward "." (overlay-start dired-efap-overlay) t)))
        (if new-point (goto-char new-point)))))


(defun dired-efap--change-to-dired-mode ()
  "Change the mode to dired."
  (remove-hook 'post-command-hook 'dired-efap--keep-in-name)
  (delete-overlay dired-efap-overlay)
  (makunbound 'dired-efap-overlay)
  (use-local-map dired-mode-map)
  (setq buffer-read-only t)
  (setq major-mode 'dired-mode)
  (setq mode-name "Dired")
  (dired-advertise)
  (setq revert-buffer-function 'dired-revert)
  (revert-buffer))

(defun dired-efap-finish ()
  "Finish the edition of the filename at point, performing the
necessary changes in disk. See also `dired-efap' and
`dired-efap-mode'"
  (interactive)
  (if (equal (overlay-start dired-efap-overlay)
             (overlay-end dired-efap-overlay))
      (error "Filename empty"))
  (let ((filename-ori (expand-file-name (overlay-get dired-efap-overlay 'original-name)))
        (filename-new (expand-file-name (buffer-substring-no-properties
                                         (overlay-start dired-efap-overlay)
                                         (overlay-end dired-efap-overlay))))
        (errors nil))
    (if (not (equal filename-ori filename-new))
        (if (file-exists-p filename-new)
            (progn
              (dired-efap-abort)
              (dired-do-create-files-regexp
               (function dired-rename-file)
               "Move" 1 ".*" filename-new nil t))
          (condition-case err
              (progn
                (dired-rename-file filename-ori filename-new nil)
                (dired-efap--change-to-dired-mode))
            (error
             (dired-log (concat "Rename `" filename-ori "' to `"
                                filename-new "' failed:\n%s\n")
                        err)
             (dired-efap-abort)
             (dired-log-summary "Rename action failed" nil))))
      (dired-efap--change-to-dired-mode))))


(defun dired-efap-abort (&optional &rest args)
  "Stop editing filename at point and abort changes."
  (interactive)
  (dired-efap--change-to-dired-mode))

;; Protect the buffer so only the filename at the current line can be
;; changed.
(defun dired-efap--set-properties (original-name)
  (save-excursion
    (let ((inhibit-read-only t)
          (start (dired-move-to-filename))
          (end (dired-move-to-end-of-filename)))
      (set-text-properties (point-min) (point-max) nil)
      (put-text-property (point-min) start 'rear-nonsticky t)
      (put-text-property (point-min) start 'read-only t)
      (put-text-property end (point-max)   'read-only t)
      ;; Create overlay
      (set (make-local-variable 'dired-efap-overlay)
           (make-overlay start end (current-buffer) nil t))
      (overlay-put dired-efap-overlay 'priority 2013)
      (overlay-put dired-efap-overlay 'face dired-efap-face)
      (overlay-put dired-efap-overlay 'original-name original-name)
      (overlay-put dired-efap-overlay 'mouse-face nil))))

(defun dired-efap--keep-in-name ()
  "Make sure point do not leaves filename being edited."
  (if (boundp 'dired-efap-overlay)
      (let ((new-point (if (< (point) (overlay-start dired-efap-overlay))
                           (overlay-start dired-efap-overlay)
                         (if (> (point) (overlay-end dired-efap-overlay))
                             (overlay-end dired-efap-overlay)))))
        (when new-point
          (goto-char new-point)))))

(provide 'dired-efap)
(run-hooks dired-efap-load-hooks)

;;; dired-efap.el ends here
