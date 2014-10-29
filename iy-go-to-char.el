;;; iy-go-to-char.el --- Go to next CHAR which is similar to "f" and "t" in vim
;; Copyright (C) 2009 Ian Yang

;; Author: Ian Yang <doit dot ian (at) gmail dot com>
;; Keywords: navigation, search
;; Filename: iy-go-to-char.el
;; Description: Go to char
;; Created: 2009-08-23 01:27:34
;; Version: 3.2.2
;; Last-Updated: 2014-10-29 22:39:29
;; URL: https://github.com/doitian/iy-go-to-char
;; Compatibility: GNU Emacs 23.1.1

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package defines the function `iy-go-to-char' which behaves like "f" in
;; vim, and `iy-go-up-to-char` like "t" in vim.  It reads a char and go the
;; next Nth occurence of the char.  User can continue such search using that
;; char key.

;; To use, make sure this file is on your `load-path' and put the
;; following in your .emacs file:
;;
;;     (require 'iy-go-to-char)
;;
;; To make `iy-go-to-char' works better with `multiple-cursors', add
;; `iy-go-to-char-start-pos' to `mc/cursor-specific-vars' when mc is loaded:
;;
;;     (add-to-list 'mc/cursor-specific-vars 'iy-go-to-char-start-pos)
;;
;; Then you can bind functions like:
;;
;;     (global-set-key (kbd "C-c f") 'iy-go-to-char)
;;     (global-set-key (kbd "C-c F") 'iy-go-to-char-backward)
;;     (global-set-key (kbd "C-c ;") 'iy-go-to-or-up-to-continue)
;;     (global-set-key (kbd "C-c ,") 'iy-go-to-or-up-to-continue-backward)
;;
;; Or if you prefer up-to (vim "t") versions:
;;
;;     (global-set-key (kbd "C-c f") 'iy-go-up-to-char)
;;     (global-set-key (kbd "C-c F") 'iy-go-up-to-char-backward)
;;
;; You also can bind go-to methods and up-to methods to different keys.
;;
;; Except repeating the char key, followings keys are defined before
;; quitting the search (which can be disabled by setting
;; `iy-go-to-char-override-local-map' to nil):
;;
;;    X   -- where X is the char to be searched. Repeating it will search
;;           forward the char. Can be disabled through
;;           `iy-go-to-char-continue-when-repeating'
;;
;;    ;   -- search forward the char, customizable:
;;           `iy-go-to-char-key-forward', `iy-go-to-char-use-key-forward'
;;
;;    ,   -- search backward the char, customizable:
;;           `iy-go-to-char-key-backward', `iy-go-to-char-use-key-backward'
;;
;;    C-g -- quit
;;
;;    C-s -- start `isearch-forward' using char as initial search
;;           string
;;
;;    C-r -- start `isearch-backward' using char as initial search
;;           string
;;
;;    C-w -- quit and kill region between start and current point.  If region is
;;           activated before search, then use the original mark instead of the
;;           start position.
;;
;;    M-w -- quit and save region between start and current point.  If region is
;;           activated before search, use the mark instead of start position.
;;
;; All other keys will quit the search.  Then the key event is
;; intepreted in the original environment before search.
;;
;; if the search quits because of error or using "C-g", point is set
;; back to the start position.  Otherwise, point is not change and the
;; start position is set as marker.  So you can use "C-x C-x" back to
;; that position.

;; `iy-go-to-char-backward' search backward by default.  Also the search can
;; cross lines.  To continue search last char, use `iy-go-to-char-continue' and
;; `iy-go-to-char-continue-backward'.

;;; Change Log:
;; 2014-10-29 (3.2.2)
;;
;;    - Add options `iy-go-to-char-use-key-forward',
;;      `iy-go-to-char-use-key-backward' and
;;      `iy-go-to-char-continue-when-repeating' to toggle the feature that
;;      continuing search by repeat typing a single key.
;;    - Add option `iy-go-to-char-override-local-map' to disable the temporary
;;      map after activate `iy-go-to-char'.
;;
;; 2013-04-28 (3.2.1)
;;
;;    - Fix documentations.
;;    - Add up-to versions: `iy-go-up-to-char', `iy-go-up-to-char-backward',
;;      `iy-go-up-to-char-continue' and `iy-go-up-to-char-continue-backward'.
;;
;; 2013-04-28 (3.1)
;;
;;    - Better integration with `multiple-cursors'.
;;    - Refactoring documentations.
;;
;; 2013-04-08 (3.0)
;;
;;    - When jump is started backward (with negative parameter, or
;;      `iy-go-to-char-backward'). Repeating continues the search
;;      backward. Also `iy-go-to-char-key-forward' jumps with the same
;;      direction when jump is started, and `iy-go-to-char-key-backward' jumps
;;      to reverse direction.
;;
;; 2013-03-25 (2.1)
;;    - Fix a but that I forget to set `mc--this-command'
;;
;; 2013-03-25 (2.0)
;;    - Use overriding-local-map to setup keymap
;;    - multiple-cursors compatible
;;
;; 2012-04-16 (1.1)
;;    - fix C-s/C-r to enter isearch

;;; Code:

(defgroup iy-go-to-char nil
  "go to char like f in vim."
  :link '(emacs-commentary-link "iy-go-to-char")
  :prefix "iy-go-to-char-"
  :group 'matching)

(defcustom iy-go-to-char-key-forward ?\;
  "Default key used to go to next occurrence of the char."
  :type 'character
  :group 'iy-go-to-char)
(defcustom iy-go-to-char-use-key-forward t
  "Whether bind `iy-go-to-char-key-forward' to go to next occurrence of the char."
  :type 'boolean
  :group 'iy-go-to-char)
(defcustom iy-go-to-char-key-backward ?\,
  "Default key used to go to previous occurrence of the char."
  :type 'character
  :group 'iy-go-to-char)
(defcustom iy-go-to-char-use-key-backward t
  "Whether bind `iy-go-to-char-key-backward' to go to next occurrence of the char."
  :type 'boolean
  :group 'iy-go-to-char)
(defcustom iy-go-to-char-continue-when-repeating t
  "Whether continue the search by repeating the search char."
  :type 'boolean
  :group 'iy-go-to-char)
(defcustom iy-go-to-char-override-local-map t
  "Whether use the temporary key bindings following `iy-go-to-char'."
  :type 'boolean
  :group 'iy-go-to-char)

(defvar iy-go-to-char-start-pos nil
  "Position where go to char mode is enabled.")

(defvar iy-go-to-char-start-dir 1
  "Jump start direction.")

(defvar iy-go-to-char-last-char nil
  "Last char used in iy-go-to-char.")

(defvar iy-go-to-char-last-step 1
  "Last jump step used in iy-go-to-char.")

(defvar iy-go-to-char-stop-position 'include
  "Control where to place the point after found a match.
Set to `include' so the next matched char is included in the
region between start search position and current point.

Set to `exclude' so the next matched char is excluded in the region.")

(defvar iy-go-to-char-keymap (let ((map (make-sparse-keymap)))
                               (define-key map (kbd "C-s") 'iy-go-to-char-isearch)
                               (define-key map (kbd "C-r") 'iy-go-to-char-isearch-backward)
                               (define-key map (kbd "C-w") 'iy-go-to-char-kill-region)
                               (define-key map (kbd "M-w") 'iy-go-to-char-kill-ring-save)
                               (define-key map (kbd "C-g") 'iy-go-to-char-quit)

                               map)
  "Keymap used when iy-go-to-char is ongoing.")

 

(defun iy-go-to-char--set-mc-command (command)
  "Set COMMAND as multiple cursors this command."
  (when (and (boundp 'multiple-cursors-mode) multiple-cursors-mode (boundp 'mc--this-command))
    (setq mc--this-command command)))

(defun iy-go-to-char--set-mc-specific-vars ()
  "Add `iy-go-to-char-start-pos' to `mc/cursor-specific-vars'."
  (when (boundp 'mc/cursor-specific-vars)
    (add-to-list 'mc/cursor-specific-vars 'iy-go-to-char-start-pos))
  (remove-hook 'multiple-cursors-mode-hook 'iy-go-to-char--set-mc-specific-vars))
(add-hook 'multiple-cursors-mode-hook 'iy-go-to-char--set-mc-specific-vars)

(defun iy-go-to-char--isearch-setup ()
  "Setup jump char as initial string for isearch."
  (remove-hook 'isearch-mode-hook 'iy-go-to-char--isearch-setup)
  (setq isearch-string (if iy-go-to-char-last-char (string iy-go-to-char-last-char) ""))
  (isearch-search-and-update))

(defun iy-go-to-char--override-local-map (char)
  "Override the local key map for jump char CHAR."
  (when iy-go-to-char-override-local-map
   (setq overriding-local-map
        (let ((map (copy-keymap iy-go-to-char-keymap)))

          (when iy-go-to-char-continue-when-repeating
            (define-key map (string char) 'iy-go-to-or-up-to-continue))
          (when iy-go-to-char-use-key-forward
            (define-key map (string iy-go-to-char-key-forward) 'iy-go-to-or-up-to-continue))
          (when iy-go-to-char-use-key-backward
            (define-key map (string iy-go-to-char-key-backward) 'iy-go-to-or-up-to-continue-backward))

          (define-key map [t] 'iy-go-to-char-pass-through)

          map))))

 

(defun iy-go-to-char-done ()
  "Finish iy-go-to-char-mode."
  (interactive)
  (push-mark iy-go-to-char-start-pos t)
  (setq iy-go-to-char-start-pos nil)
  (setq overriding-local-map nil))

(defun iy-go-to-char-quit ()
  "Quit iy-go-to-char-mode."
  (interactive)
  (goto-char iy-go-to-char-start-pos)
  (setq iy-go-to-char-start-pos nil)
  (setq overriding-local-map nil))

(defun iy-go-to-char-pass-through ()
  "Finish iy-go-to-char-mode and invoke the corresponding command."
  (interactive)
  (iy-go-to-char-done)
  (let* ((keys (progn
                 (setq unread-command-events
                       (append (this-single-command-raw-keys)
                               unread-command-events))
                 (read-key-sequence-vector "")))
         (command (and keys (key-binding keys))))
    (when (commandp command)
      (setq this-command command
            this-original-command command)
      (iy-go-to-char--set-mc-command `(lambda ()
                                        (interactive)
                                        (push-mark iy-go-to-char-start-pos t)
                                        (setq iy-go-to-char-start-pos nil)
                                        (call-interactively ',command)))
      (call-interactively command))))

(defun iy-go-to-char-isearch ()
  "Start isearch using the char."
  (interactive)
  (iy-go-to-char-done)
  (add-hook 'isearch-mode-hook 'iy-go-to-char--isearch-setup)
  (isearch-forward))

(defun iy-go-to-char-isearch-backward ()
  "Start isearch backward using the char."
  (interactive)
  (iy-go-to-char-done)
  (add-hook 'isearch-mode-hook 'iy-go-to-char--isearch-setup)
  (isearch-backward))

(defun iy-go-to-char-kill-region ()
  "Kill region between jump start position and current position."
  (interactive)
  (iy-go-to-char-done)
  (iy-go-to-char--set-mc-command (lambda ()
                                   (interactive)
                                   (kill-region (point) iy-go-to-char-start-pos)
                                   (setq iy-go-to-char-start-pos nil)))
  (kill-region (point) (mark)))

(defun iy-go-to-char-kill-ring-save ()
  "Save region between jump start position and current position."
  (interactive)
  (iy-go-to-char-done)
  (iy-go-to-char--set-mc-command (lambda ()
                                   (interactive)
                                   (kill-ring-save (point) iy-go-to-char-start-pos)
                                   (setq iy-go-to-char-start-pos nil)))
  (kill-ring-save (point) (mark)))

(defun iy-go-to-char--command ()
  "Repeatable command to really move cursor."
  (interactive)
  (setq iy-go-to-char-start-pos (or iy-go-to-char-start-pos (point)))
  (let* ((pos (point))
         (n (if (< iy-go-to-char-start-dir 0)
                (- iy-go-to-char-last-step)
              iy-go-to-char-last-step))
         (dir (if (< n 0) -1 1)))
    (condition-case err
        (progn
          (unless (or (eq iy-go-to-char-stop-position 'include)
                     (if (< dir 0) (bobp) (eobp)))
            (forward-char dir))
          (search-forward (string iy-go-to-char-last-char) nil nil n)
          (unless (eq iy-go-to-char-stop-position 'include)
            (forward-char (- dir))))
      (error (goto-char pos)
             (signal (car err) (cdr err))))))

(defun iy-go-to-char--internal (n char stop-position)
  "Store jump step N and jump CHAR for `iy-go-to-char--command'.
If STOP-POSITION is not `include', jump up to char but excluding the char."
  (interactive "p\ncGo to char: ")
  (setq iy-go-to-char-last-step n)
  (setq iy-go-to-char-last-char char)
  (setq iy-go-to-char-stop-position stop-position)
  (unless iy-go-to-char-start-pos
    (setq iy-go-to-char-start-pos (point))
    (iy-go-to-char--override-local-map char))

  (setq this-original-command 'iy-go-to-char--command
        this-command 'iy-go-to-char--command)
  (iy-go-to-char--set-mc-command 'iy-go-to-char--command)
  (call-interactively 'iy-go-to-char--command))

;;;###autoload
(defun iy-go-to-char (n char)
  "Move forward to N occurrences of CHAR.
\\<iy-go-to-char-keymap>

Typing key of CHAR will move to the next occurence of CHAR.

Typing `iy-go-to-char-key-forward' will move to the next
occurence of CHAR.

Typing `iy-go-to-char-key-backward', will move to the previous
occurence of CHAR.

Typing \\[iy-go-to-char-quit] will quit and return to the
original point.

Typing \\[iy-go-to-char-isearch] or
\\[iy-go-to-char-isearch-backward]] will start `isearch` using
CHAR.

Typing \\[iy-go-to-char-kill-region] or
\\[iy-go-to-char-kill-ring-save] will kill/copy between current
point and the start point.

Unless quit using \\[iy-go-to-char-quit] or the region is
activated before searching, the start point is set as mark."
  (interactive "p\ncGo to char: ")
  (setq iy-go-to-char-start-dir (if (< n 0) -1 1))
  (iy-go-to-char--internal n char 'include))

;;;###autoload
(defun iy-go-to-char-backward (n char)
  "Move backward to N occurence of CHAR.
\\<iy-go-to-char-keymap>

Typing key of CHAR will move to the previous occurence of CHAR.

Typing `iy-go-to-char-key-forward' moves to the next occurrence
of CHAR.

Typing `iy-go-to-char-key-backward', moves to the previous
occurrence of CHAR.

Typing \\[iy-go-to-char-quit] will quit and return to the
original point.

Typing \\[iy-go-to-char-isearch] or
\\[iy-go-to-char-isearch-backward]] will start `isearch` using
CHAR."
  (interactive "p\ncGo back to char: ")
  (setq iy-go-to-char-start-dir (if (< n 0) 1 -1))
  (iy-go-to-char--internal n char 'include))

;;;###autoload
(defun iy-go-up-to-char (n char)
  "Move forward to N occurrences of CHAR.
Like `iy-go-to-char' but jump up to the CHAR so it is
not included in the region between search start position and
current point."
  (interactive "p\ncGo up to char: ")
  (setq iy-go-to-char-start-dir (if (< n 0) -1 1))
  (iy-go-to-char--internal n char 'exclude))

;;;###autoload
(defun iy-go-up-to-char-backward (n char)
  "Move backward to N occurrences of CHAR.
Like `iy-go-to-char-backward' but jump up to the CHAR so it is
not included in the region between search start position and
current point."
  (interactive "p\ncGo back up to char: ")
  (setq iy-go-to-char-start-dir (if (< n 0) 1 -1))
  (iy-go-to-char--internal n char 'exclude))

;;;###autoload
(defun iy-go-to-or-up-to-continue (n &optional stop-position)
  "Continue last `iy-go-to-char' or `iy-go-to-char-backward' by N steps.
Set `STOP-POSITION' to overwrite the last used stop position strategy."
  (interactive "p")
  (when iy-go-to-char-last-char
    (iy-go-to-char--internal n iy-go-to-char-last-char (or stop-position
                                                           iy-go-to-char-stop-position
                                                           'include))))

;;;###autoload
(defun iy-go-to-or-up-to-continue-backward (n &optional stop-position)
  "Continue last `iy-go-to-char' or `iy-go-to-char-backward' by N steps.
Set `STOP-POSITION' to overwrite the last used stop position strategy."
  (interactive "p")
  (when iy-go-to-char-last-char
    (iy-go-to-char--internal (- n) iy-go-to-char-last-char (or stop-position
                                                               iy-go-to-char-stop-position
                                                               'include))))

;;;###autoload
(defun iy-go-to-char-continue (n)
  "Continue last `iy-go-to-char' or `iy-go-to-char-backward' by N steps."
  (interactive "p")
  (iy-go-to-or-up-to-continue n 'include))

;;;###autoload
(defun iy-go-to-char-continue-backward (n)
  "Continue last `iy-go-to-char' or `iy-go-to-char-backward' by N steps."
  (interactive "p")
  (iy-go-to-or-up-to-continue-backward n 'include))

;;;###autoload
(defun iy-go-up-to-char-continue (n)
  "Continue last `iy-go-up-to-char' or `iy-go-up-to-char-backward' by N steps."
  (interactive "p")
  (iy-go-to-or-up-to-continue n 'exclude))

;;;###autoload
(defun iy-go-up-to-char-continue-backward (n)
  "Continue last `iy-go-up-to-char' or `iy-go-up-to-char-backward' by N steps."
  (interactive "p")
  (iy-go-to-or-up-to-continue-backward n 'exclude))

 

(provide 'iy-go-to-char)

;;; iy-go-to-char.el ends here

;;  LocalWords:  customizable
