;;; dired-efap.el --- Edit Filename At Point in a dired buffer

;; Copyright (C) 2001 Juan León Lahoz García

;; Filename: dired-efap.el
;; Author: Juan León Lahoz García <juan-leon.lahoz@tecsidel.es>
;; Version: 0.4
;; Keywords: dired, environment, files, renaming

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; dired-efap.el allows the user to edit the filename at point, by
;; hitting a key (like f2) or double-clicking it. The name is edited
;; in the dired buffer, and the renaming takes effect when the user
;; hit the RET key. Only the name of the file at point is tangible and
;; editable, and it uses an special font.
;;
;; This package is an attempt to provide the same user interface to
;; renaming files that some graphical file managers offer.
;;
;; Please, don't confuse dired-efap with wdired package, which I also
;; wrote. Both "packages" provide alternative ways of renaming for
;; dired users, but, while they are both compatible between them, they
;; are oriented to different kind of users. wdired is for users who
;; like the incredible flexibility and power of Emacs editing
;; features. dired-efap is for users that are habituated to use other
;; graphical file managers (or "explorers", however they are called).
;;
;; The idea for this file was originated from a suggestion from Eli
;; Tziperman to improve wdired.
;;
;; Comments, bug reports, etc. welcomed

;;; Usage:

;; Add this file (byte-compile it if you want) to your load-path and
;; this line to your config (I've choose f2 because is used in some
;; file explorers, but you can change it if you want):
;;
;; (require 'dired-efap)
;; (define-key dired-mode-map [f2] 'dired-efap)
;;
;; To edit a name you can hit f2 (or the key you chose) or
;; double-click over it. Pressing RET the file is actually renamed.
;; C-g aborts.
;;
;; The behavior of the mouse can be customized. There are three
;; options: double-click to edit, click once in the file where the
;; cursor is (this includes double-click, because the first click
;; moves the cursor) and no use the mouse to edit names. See and
;; customize the variable `dired-efap-use-mouse' to change the default
;; behavior of double-click to edit
;;
;; You can customize also the face of the name being edited. This face
;; is called `dired-efap-face'
;;
;; Type M-x customize-group RET dired-efap if you want make changes to
;; the default behavior.

;;; Change Log:

;; From 0.2 to 0.3
;;
;; - Now, edited filenames get highlighted when the mouse is over
;;   them, just like any other filename.
;;
;; - Now, the filename being edited doesn't get highlighted when the
;;   mouse is over it.
;;
;; - Now, the default `dired-efap-face' for emacs 20.x users is like
;;   it used to be in 0.1.

;; From 0.2 to 0.3
;;
;; - Now, user is warned before overwriting another file.
;;
;; - Now, when editing filename at point, clicking out of it with the
;;   mouse, but in the same buffer (_even_ _after_ the _end_ of the
;;   _text_ :-)), works as pressing RET and then clicking the mouse (i.e.
;;   the renaming is done).
;;
;; - Now, `dired-efap-select-whole-filename' works as supposed when in
;;   `transient-mark-mode' when using the keyboard.

;; From 0.1 to 0.2
;;
;; - Bug fixed: now the last filename can be edited.
;;
;; - `dired-efap-mode-hooks' and `dired-efap-load-hooks' have been
;;   added, and they are run in the right places.
;;
;; - Now, if you are using emacs 21 (or greater, :-)), the default
;;   face is a black bordered box.
;;
;; - Now, customizing `dired-efap-select-whole-filename', you can
;;   choose if the whole filename gets automatically selected when
;;   entering in `dired-efap-mode'. This can be nice to some
;;   `pc-selection-mode' users.
;;
;; - Now, when editing filename at point, clicking out of it with the
;;   mouse, but in the same buffer, works as pressing RET and then
;;   clicking the mouse (i.e. the renaming is done)

;;; Bugs

;; - A minor one: `dired-efap-select-whole-filename' doesn't work as
;;   supposed when in `transient-mark-mode' when using the mouse: the
;;   selection goes from the point where the user clicked to the end
;;   of the filename. Does anybody know why?.

;;; Code:

(eval-when-compile
  (set (make-local-variable 'byte-compile-dynamic) t))

(eval-and-compile
  (require 'dired)
  (require 'font-lock)
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

(defcustom dired-efap-select-whole-filename nil
  "*If non-nil the whole filename gets initially selected.

Setting this to t is recommended to `pc-selection-mode' or
`delete-selection-mode' users that want to be able to easily replace
the whole filename."
  :type 'boolean
  :group 'dired-efap)

(defgroup dired-efap-faces nil
  "Faces used in dired edit filename at point."
  :prefix "dired-efap-"
  :group 'dired-efap
  :group 'faces)

(defface dired-efap-face
  (if (>= emacs-major-version 21)
      '((t (:box (:line-width 3 :color "black" :style pressed-button))))
    '((t (:inverse-video t))))
  "Face used for filenames being edited."
  :group 'dired-efap-faces)

(defvar dired-efap-old-mouse-func nil
  "Original function binded to down-mouse-1 en dired mode.")
(unless dired-efap-old-mouse-func
  (let (current-map (current-local-map))
    (use-local-map dired-mode-map)
    (setq dired-efap-old-mouse-func (key-binding [down-mouse-1]))
    (use-local-map current-map)))

(defvar dired-efap-mode-hooks nil
  "Hooks run when changing to dired-efap mode.")

(defvar dired-efap-load-hooks nil
  "Hooks run after loading dired-efap code.")

(define-key dired-mode-map [down-mouse-1] 'dired-efap-click)

(defvar dired-efap-mode-map nil)
(unless dired-efap-mode-map
  (setq dired-efap-mode-map (make-sparse-keymap))
  (define-key dired-efap-mode-map "\C-c\C-k" 'dired-efap-abort)
  (define-key dired-efap-mode-map "\C-g"     'dired-efap-abort)
  (define-key dired-efap-mode-map "\C-x\C-s" 'dired-efap-finish)
  (define-key dired-efap-mode-map "\C-c\C-c" 'dired-efap-finish)
  (define-key dired-efap-mode-map "\C-j"     'dired-efap-finish)
  (define-key dired-efap-mode-map "\C-o"     'dired-efap-finish)
  (define-key dired-efap-mode-map [mouse-1]  'dired-efap-click-to-finish)
  (define-key dired-efap-mode-map [return]   'dired-efap-finish)
  (define-key dired-efap-mode-map [next]     'dired-efap-noop)
  (define-key dired-efap-mode-map [prior]    'dired-efap-noop)
  (define-key dired-efap-mode-map "\C-k"     'dired-efap-kill-line))

;; Local variables
(defvar dired-efap-filename-ori)
(defvar dired-efap-starting-point)
(defvar dired-efap-font-lock)


(defun dired-efap-click (event)
  "Move to the point and, depending of the value of
`dired-efap-use-mouse', if the click has been double and the previous
position of the point, edit filename at point.

See `dired-efap-use-mouse' and `dired-efap'"
  (interactive "e")
  (if dired-efap-use-mouse
      (if (equal dired-efap-use-mouse 'selected)
	  (let ((previous-file (dired-get-filename nil t)))
	    (mouse-set-point event)
	    (if (and previous-file
		     (equal previous-file (dired-get-filename nil t)))
		(progn
		  ;; Move down the mouse, to have a better visibility.
		  (set-mouse-position (selected-frame) (cadr (mouse-position))
				      (+ (cddr (mouse-position)) 1))
		  (dired-efap))))
	(if (member 'double (event-modifiers event))
	    (progn
	      ;; Move down the mouse, to have a better visibility.
	      (set-mouse-position (selected-frame) (cadr (mouse-position))
				  (+ (cddr (mouse-position)) 1))
	      (dired-efap))))
    (funcall dired-efap-old-mouse-func event)))

(defun dired-efap-click-to-finish (event)
  "Finish the edition of the filename at point, performing the
necessary changes in disk. This only happens if the click is outside
the filename but in the dired buffer. Anyway, point is moved to the
click point. See also `dired-efap' and `dired-efap-mode'"
  (interactive "e")
  (let ((posn (event-start event)))
    (if (and (eq (posn-window posn) (get-buffer-window (current-buffer)))
             (or (>= (posn-point posn) (point-max))
                 (get-text-property (posn-point posn) 'intangible)))
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

(defun dired-efap ()
  "Change the mode of a dired buffer to another in witch the filename
at point becomes editable.  Press RET to actually rename the file or
directory in disk, and C-g to abort."
  (interactive)
  (set (make-local-variable 'dired-efap-filename-ori)
       (dired-get-filename 'no-dir))
  (if (string-match "^\\.\\.?$" dired-efap-filename-ori)
      (error ". and .. cannot be edited"))
  (save-excursion
    (set (make-local-variable 'dired-efap-starting-point)
	 (dired-move-to-filename)))
  (use-local-map dired-efap-mode-map)
  (menu-bar-mode (or menu-bar-mode -1)) ;Force redisplay menu
  (setq buffer-read-only nil)
  (dired-unadvertise default-directory)
  (setq major-mode 'dired-efap-mode)
  (setq mode-name "Filename edit")
  (setq revert-buffer-function 'dired-efap-revert)
  (message "Press RET when finished")
  (dired-efap-protect-text)
  (run-hooks dired-efap-mode-hooks)
  (if dired-efap-select-whole-filename
      (if transient-mark-mode
	  (if pc-selection-mode
              ;; Why the mouse set the mark where the user clicked
              ;; after executing the macros ???
	      (execute-kbd-macro [home S-end])
	    (execute-kbd-macro [?\C-a ?\C-  ?\C-e]))
	(progn ; Why does this not work in transient-mark-mode ???
	  (dired-efap-goto-end nil nil)
	  (push-mark dired-efap-starting-point t t)))))




(defun dired-efap-change-to-dired-mode ()
  "Change the mode to dired."
  (dired-efap-goto-end nil nil)
  (let ((inhibit-read-only t))
    (delete-region (point) (+ (point) 2))
    (put-text-property dired-efap-starting-point (point)
                       'mouse-face 'highlight)
    (put-text-property (point-min) (point-max) 'read-only nil))
  (put-text-property (point-min) (point-max) 'intangible nil)
  (put-text-property (point-min) (point-max) 'point-entered nil)
  (put-text-property (point-min) (point-max) 'rear-nonsticky nil)
  (put-text-property (point-min) (point-max) 'front-sticky nil)
  (font-lock-mode (if dired-efap-font-lock 1 0))
  (use-local-map dired-mode-map)
  (menu-bar-mode (or menu-bar-mode -1)) ;Force redisplay menu
  (setq buffer-read-only t)
  (setq major-mode 'dired-efap-mode)
  (setq mode-name "Dired")
  (dired-advertise)
  (setq revert-buffer-function 'dired-revert))

(defun dired-efap-finish ()
  "Finish the edition of the filename at point, performing the
necessary changes in disk. See also `dired-efap' and
`dired-efap-mode'"
  (interactive)
  (dired-efap-goto-end nil nil)
  (if (equal (point) dired-efap-starting-point)
      (error "Filename empty"))
  (let ((filename-ori (expand-file-name dired-efap-filename-ori))
	(filename-new (expand-file-name (buffer-substring-no-properties
					 dired-efap-starting-point (point))))
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
		(dired-efap-change-to-dired-mode))
	    (error
	     (dired-log (concat "Rename `" filename-ori "' to `"
				filename-new "' failed:\n%s\n")
			err)
	     (dired-efap-abort)
	     (dired-log-summary "Rename action failed" nil))))
      (dired-efap-change-to-dired-mode))))


(defun dired-efap-abort ()
  "Stop editing filename at point and abort changes."
  (interactive)
  (dired-efap-goto-end nil nil)
  (delete-region dired-efap-starting-point (point))
  (insert dired-efap-filename-ori)
  (dired-efap-change-to-dired-mode))

;; Protect the buffer so only the filename at the current line can be
;; changed
(defun dired-efap-protect-text ()
  (let ((cursor-position (point))
	(filename-end (save-excursion (dired-move-to-filename)
				      (dired-move-to-end-of-filename)))
        (content (buffer-substring (point-min) (point-max)))
        (inhibit-read-only t)
        (tricky-end " ?"))
    ;; hack to stop font-lock changing the faces, while keeping the
    ;; initial ones
    (set (make-local-variable 'dired-efap-font-lock) font-lock-mode)
    (erase-buffer)
    (and font-lock-mode (font-lock-mode))
    (insert content)
    (goto-char cursor-position)
    ;; Disallow move the cursor to the beginning of the buffer
    (put-text-property 1 2 'front-sticky t)
    (put-text-property 1 2 'point-entered 'dired-efap-goto-beginning)

    (save-excursion
      ;; Make editable and tangible only the filename
      (put-text-property (point-min) dired-efap-starting-point
			 'rear-nonsticky t)
      (put-text-property dired-efap-starting-point filename-end
			 'front-sticky t)
      (put-text-property dired-efap-starting-point filename-end
			 'face 'dired-efap-face)
      (put-text-property dired-efap-starting-point filename-end
			 'mouse-face nil)
      (put-text-property (point-min) dired-efap-starting-point 'read-only t)
      (put-text-property filename-end (point-max) 'read-only t)
      (put-text-property (point-min) dired-efap-starting-point 'intangible t)
      (put-text-property filename-end (point-max) 'intangible t)

      ;; Hyper dirty trick to make possible to the user to delete the
      ;; whole filename while keeping some chars "tangibles": add some
      ;; invisible chars to the end of the filename.
      (goto-char filename-end)
      (put-text-property 0 2 'invisible t tricky-end)
      (put-text-property 0 2 'read-only t tricky-end)
      (put-text-property 1 2 'point-entered 'dired-efap-goto-end tricky-end)
      (put-text-property 0 1 'insert-in-front-hooks
                         '(dired-efap-restore-face) tricky-end)
      (insert tricky-end)

      ;; Disallow move the cursor to the end of the buffer
      (put-text-property (- (point-max) 1) (point-max) 'point-entered
                         'dired-efap-goto-end)
      (buffer-disable-undo)
      (buffer-enable-undo))))  ; No way to undo these changes.

(defun dired-efap-goto-beginning (degb-x degb-y) ;; :WAS (x x)
  "Go to the beginning of the filename being edited."
  (interactive)
  (goto-char dired-efap-starting-point))

(defun dired-efap-goto-end (dege-x dege-y) ;; :WAS (x x)
  "Go to the end of the filename being edited."
  (interactive)
  (goto-char dired-efap-starting-point)
  (while (not (get-text-property (point) 'invisible))
    (forward-char)))

(defun dired-efap-kill-line (&optional arg)
  "Kill the rest of the current filename being edited."
  (interactive)
  (while (not (get-text-property (point) 'invisible))
    (delete-char 1)))

;; Hack to restore the face of the filename after it has been deleted.
;; To put additional invisible text does not work well with "boxed"
;; faces, because the right side of the box is invisible
(defun dired-efap-restore-face (beg end)
  (if (not (eq (get-text-property dired-efap-starting-point 'face)
               'dired-efap-face))
      (progn
        (put-text-property beg end 'front-sticky t)
        (put-text-property beg end 'face 'dired-efap-face))))

(defun dired-efap-noop ()
  "Do nothing."
  (interactive))

(provide 'dired-efap)
(run-hooks dired-efap-load-hooks)

;;; dired-efap.el ends here
