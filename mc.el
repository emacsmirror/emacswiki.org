;;; mc.el --- Midnight Commander emulation for emacs using dired

;; $Id: mc.el,v 1.65 2003/01/10 05:57:59 burton Exp $

;; Copyright (C) 1997-2000 Free Software Foundation, Inc.

;; Author: Kevin A. Burton (burton@openprivacy.org)
;; Maintainer: Kevin A. Burton (burton@openprivacy.org)
;; Location: http://relativity.yi.org
;; Keywords: Midnight Commander
;; Version: 2.0

;; This file is [not yet] part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2 of the License, or any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program; if not, write to the Free Software Foundation, Inc., 59 Temple
;; Place - Suite 330, Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package provides Midnight Commander style emulation for Emacs.  Midnight
;; Commander is a UNIX application with a User Interface similar to the old DOS
;; program named Norton Commander.  Basically this provides the user with a
;; quick, two pane interface to their file system.  I wanted to model it after
;; Midnight Commander because it is Free Software (Norton Commander is not) and
;; recent UNIX/Linux users will generally be more familiar with Midnight
;; Commander.  Users of dired will be familiar with mc because dired features
;; are still present.

;; There is Midnight Commander style package which does not use dired.  It is
;; named nc.el and is located at:
;; ftp://ftp.math.ohio-state.edu/pub/users/ilya/emacs.  It generally tries to
;; target the DOS equivalent Norton Commander.

;;; Install/Usage
;;
;; Add the lines (require 'mc) to your .emacs file.
;;
;; In order to run mc just type M-x mc.  This will invoke mc.
;;
;; The standard Midnight Commander key bindings should now be available.

;; TAB -> Change between windows

;; F2      -> Change directory
;; F3/F4   -> Open/Edit
;; F5      -> Copy
;; F6      -> Move/Rename
;; F8      -> Delete
;; F10     -> Quit

;;; History:

;; Thu Nov 22 2001 11:35 PM (burton@openprivacy.org): BUG: if you run `mc' twice
;; the right window will be reset to the home directory.

;;    - this is a noop.  Emacs 21 fixed this.

;; Thu Nov 22 2001 11:29 PM (burton@openprivacy.org): created history section.

;;; VERSION 2.0 TODO:

;; - create a new toplevel project called emc.
;; 

;; - Do not rebind dired keys.  Instead use advice which we enable from mc-mode
;;
;;     - mc-dired-advertised-find-file
;;     - mc-dired-view-file

;; - Stop using font-lock-add-keywords
;; 

;; - doesn't work under the ECB
;;
;; - can we localize the exit-recursive-edit to mc-quit??
;;
;; - remove the help browser at the bottom.
;;
;; - add support for mc style browsing modes (only one side can have a
;;   non-standard mode)
;;
;;   - quick-view
;;   - info-view
;;   - tree-view
;;
;; - is there a way to automatically change (mc highlight) the other buffer ifit
;; is selected?

;;; TODO:

;; - consider rewriting the way this works.  The main window should be the help
;;   window.  Then we can have an emulated terminal here.  That would be really
;;   nice.
;;
;;   - the help buffer should have a better menu-bar-layout
;; 
;;


;; should the window split settings be frame local???

;; if keyboard-escape-quit is called while mc is in a non-standard windown
;; configuration... make sure we restore the default window configuration.  This
;; is usually because we called another command which split the window, etc and
;; mc will exit too soon. 



;; BUG: if the help buffer has been destroyed... recreate it.

;; I should be able to mark like in dired and mc (toggle marking needs to work)
;; and then bould commands should work correctly...

;; [insert] should be bound to 'dired-mark just like "m" dired.  This should be
;; a new function named dired-mark-toggle

;; delete should work for directories....  currently it only works on files :(

;; + is already bould to dired-create-directory... need to wrap this around an
;; mc-dired-create-directory... and then revert.

;; when I select the other window via mouse I need to correctly change the
;; highlighted window (blue)... there is a hook for this.

;; recursive edits are not working correctly I think... when a help message pops
;; up... and I break... it goes all the way back (not just one level)

;; the MC menu should show the correct key binding just like regular Midnight
;; Commander.  Right now it just shows some 'random' key..  (Ex: Quit is C-b
;; instead of F10

;; add some advice to kill-buffer so that if the buffer being killed is one from
;; mc... then quit mc.

;; when I am using a wild card ( /home/burton/el/*.el ) make sure I include the
;; .. and . directories...

;;; History:
;; 

(require 'dired)
(require 'easymenu)
(require 'font-lock)
(require 'browse-url)

;;; Code:
(defface mc-directory-face '((t (:bold t)))
  "Face used to highlight directories.")

(defface mc-symlink-face '((t (:italic t)))
  "Face used to highlight symbolic links.")

(defface mc-symlink-directory-face '((t (:italic t)))
  "Face used to highlight symbolic directory links.")

(defface mc-help-face '((t (:foreground "Black" :background "MediumSpringGreen")))
  "Face used to display MC help keys.")

(defface mc-window-selected-face '((t (:background "blue"))) "Face used to show a selected window")

(defface mc-window-not-selected-face '((t (:foreground "white")))
  "Face used to show an unselected window")

(defvar mc-restore-buffer nil "Buffer to restore when mc is quit.")

(defvar mc-prior-window-configuration nil "Window configuration before mc was started.")

(defvar mc-running nil "True when midnight commander mode is running.")

(defvar mc-current-window-overlay nil
  "Holds the current overlay which marks the current dired buffer.")

(defvar mc-left-directory "~"
  "Dired directory for the left window.  See variable `dired-directory'.")

(defvar mc-right-directory "~"
  "Dired directory for the right window.  See variable `dired-directory'.")

(defvar mc-left-window nil "The left window of dired.")

(defvar mc-right-window nil "The right window of dired.")

(defvar mc-selected-window 'left "The window to select when mc starts up.")

(defvar mc-help-buffer "*mc-help*" "Buffer used to store help info.")

(defvar mc-help-buffer-created nil "True if the help buffer has been created.")

(defvar mc-help-window nil "Window used to store help info.")

(defcustom mc-window-split-style 'horizontal
  "The current window split configuration.  May be either 'horizontal or 'vertical."
  :group 'mc
  :type '(choice
          (const horizontal)
          (const vertical)))

(defvar mc-start-message
  "Midnight Commander emulation enabled.  F-10 (or escape) to quit." 
  "Message to display when `mc' is started.")

(defvar mc-mode-map (let ((map (make-sparse-keymap)))
                      (define-key map [(f2)] 'mc-dired)
                      (define-key map [(f3)] 'mc-dired-advertised-find-file)
                      (define-key map [(f4)] 'mc-dired-advertised-find-file)
                      (define-key map [(f5)] 'dired-do-copy)
                      (define-key map [(f6)] 'dired-do-rename)
                      (define-key map [(f7)] 'dired-create-directory)
                      (define-key map [(f8)] 'dired-do-delete)
                      (define-key map [(f10)] 'keyboard-escape-quit)
                      (define-key map [return] 'mc-dired-advertised-find-file)
                      (define-key map [tab] 'mc-change-window)
                      (define-key map [(insert)] 'dired-mark)
                      (define-key map [(C-home)] 'mc-beginning-of-buffer)
                      (define-key map [(C-end)] 'mc-end-of-buffer)
                      (define-key map [?\C-c?\C-s] 'mc-split-toggle)
                      (define-key map "b" 'mc-browse)
                      (define-key map "e" 'mc-dired-advertised-find-file)
                      (define-key map "f" 'mc-dired-advertised-find-file)
                      (define-key map "g" 'mc-revert-buffer)
                      (define-key map "v" 'mc-dired-view-file)
                      (define-key map "q" 'keyboard-escape-quit)
                      (define-key map "U" 'mc-dired-prev-subdir) map)
  "Mode specific keymap for function `mc-mode'.")

(defvar mc-mode-menu nil "Menu for `mc-mode'.")
(easy-menu-define mc-mode-menu mc-mode-map
  "MC menu"
  (list
   "MC"
   ["Change Window" mc-change-window [:keys "TAB"] t]
   ["Change Directory" mc-dired t]
   ["View/Edit current file" dired-advertised-find-file t]
   ["Copy" dired-do-copy [:keys "f5"] t]
   ["Rename" dired-do-rename t]
   ["Delete" dired-do-delete t]
   ["Previous directory" mc-dired-prev-subdir t]
   "----"
   ["Toggle window split (horizontal or vertical)" mc-split-toggle t]
   "----"
   ["Quit" keyboard-escape-quit t]))

(defun mc(&optional left-directory right-directory) 
  "Start emulated midnight commander.  If the param `left-directory' is given
the left window will display this directory (the same for `right-directory').
Specifying nil for any of these values uses the default."
  (interactive)

  (message "Entering midnight commander...")
  
  (if (not mc-running)
      (progn
        (catch 'exit

          (if left-directory
              (setq mc-left-directory left-directory))

          (if right-directory
              (setq mc-right-directory right-directory))
          
          (setq mc-running t)
          
          (setq mc-restore-buffer (current-buffer))
          
          (setq mc-prior-window-configuration (current-window-configuration))
          
          (mc-setup-windows)

          (message mc-start-message)
          (recursive-edit))
        (mc-quit))
    (progn
      ;;handle an situation of mc already running...
      ;;NOTE:  this might not be really fast because we have to restart dired..  :(

      (message "Midnight commander already running...")
      (mc-setup-windows)
      (message mc-start-message))))

(defun mc-cd()
  "Run mc but give it the current directory to use."
  (interactive)
  
  (let((left-directory default-directory))
    
    (mc left-directory)))

(defun mc-setup-windows()
  "Setup the MC window configuration (two windows both running dired.)"
  
  (mc-help-buffer-init)
  
  ;;get rid of other windows if they exist.
  (delete-other-windows)

  ;;now create the bottom window which is used as a UI hint and for completion
  (let((window-min-height 1))
    (split-window (selected-window) (- (window-height) 3)))
          
  (setq mc-help-window (next-window))
          
  (set-window-buffer mc-help-window (get-buffer-create mc-help-buffer))

  (if (equal mc-window-split-style
             'horizontal)
      (split-window-horizontally)
    (if (equal mc-window-split-style
             'vertical)
        (split-window-vertically)
      (error "Don't know how to split this window: %s" mc-window-split-style)))
  
  ;;setup dired in both windows
  (mc-dired mc-left-directory)
  (setq mc-left-window (selected-window))
          
  (other-window 1)
  (mc-dired mc-right-directory)
  (setq mc-right-window (selected-window))

  ;;select the correct window
  (mc-select-window mc-selected-window))

(defun mc-split-horizontally()
  "If mc is running split it right now... else split this way for all future
buffers."
  (interactive)

  (mc-split-setup 'horizontal))

(defun mc-split-vertically()
  "If mc is running split it right now... else split this way for all future
buffers."
  (interactive)

  (mc-split-setup 'vertical))

(defun mc-split-toggle()
  "If mc is currently configured for vertical splitting... change it to
horizontal and vice-versa."
  (interactive)

  (if (equal mc-window-split-style
             'horizontal)
      (mc-split-setup 'vertical)
    (mc-split-setup 'horizontal)))

(defun mc-split-setup(split-type)

  (setq mc-window-split-style split-type)

  (if mc-running
      (progn
        (delete-other-windows)
        (mc-setup-windows)))

  (redraw-display)
  (message "Split is now %s." (symbol-name split-type)))

(defun mc-help-buffer-init()
  "Init the help buffer so it looks just like MC."
  
  (set-buffer (get-buffer-create mc-help-buffer))
  
  (if (featurep 'highline)
      (progn

        ;;need to turn off highline (via local-mode) if it is enabled...
        ;;highline is an optional emacs package but I use it so I don't want
        ;;strange behavior on other users systems.
        
        (highline-local-mode 1)
        (highline-local-mode -1)))
        
  (if (or (not mc-help-buffer-created)
          (equal (buffer-size)
                 0))
      (progn

        (toggle-read-only -1)
        (erase-buffer)

        ;;need to have the same key bindings in this buffer as in the other ones...
        (mc-mode-on)
    
        ;;  (insert "he
    
        (mc-help-buffer-insert-option "F1" "Help")
        (mc-help-buffer-insert-option "F2" "Chdir")
        (mc-help-buffer-insert-option "F3" "View")
        (mc-help-buffer-insert-option "F4" "Edit")
        (mc-help-buffer-insert-option "F5" "Copy")
        (mc-help-buffer-insert-option "F6" "RenMov")
        (mc-help-buffer-insert-option "F7" "Mkdir")
        (mc-help-buffer-insert-option "F8" "Delete")
        (mc-help-buffer-insert-option "F10" "Quit")
    
        (toggle-read-only 1)
        (setq mc-help-buffer-created t))))

(defun mc-help-buffer-insert-option(key option)
  "Insert the given help option with the given key in the help buffer."
  
  (insert (format "%s " key))

  (let(begin end overlay)
    (setq begin (point))

    (insert (format "%s " option))

    (setq end (point))

    ;;create an overlay for the help keyword...

    (setq overlay (make-overlay begin end (current-buffer)))

    (overlay-put overlay 'priority 0)

    (overlay-put overlay 'face 'mc-help-face)

    (overlay-put overlay 'buffer (current-buffer))

    (insert " ")))

(defun mc-sort-buffer-move-regexp(regexp line)
  "Given a regular expression, move all matches to the given line"

  (save-excursion
    (goto-char (point-min))
    
    (if (re-search-forward regexp nil t)
        
        (let(match)
          
          (setq match (format "%s\n" (match-string 0)))
          (delete-region (match-beginning 0) (match-end 0))
          (kill-line 1)
          (save-excursion
            
            (goto-char (point-min))
            (forward-line line)
            (insert match))))))

(defun mc-change-window()
  "Change to the other mc buffer"
  (interactive)

  ;; this is much smarter than using (other-window) because we don't want to
  ;; include windows that were created accidentally.
  (if (equal (selected-window)
             mc-right-window)
      (mc-select-window 'left)
    (mc-select-window 'right)))

(defun mc-select-window(window)
  "Select/highlight the given mc window (right or left)."

  (if (string= (symbol-name window)
               "left")
      (progn
        (select-window mc-left-window)
        (setq mc-selected-window 'left))
    (progn
      (select-window mc-right-window)
      (setq mc-selected-window 'right)))

  (mc-highlight))

(defun mc-browse()
  "Browse the directory/file on the current line."
  (interactive)

  (let(filename)
    (setq filename (dired-get-filename))
    (if filename
        (let(url)
          (setq url (concat "file://" filename))
          (message "Browsing %s " url)
          (browse-url url)))))

(defun mc-quit()
  "Quit emulated mc and restore emacs to previous operation."
  (interactive)

  ;;don't run if mc-quit is already shut down
  (if mc-running
      (progn
        (setq mc-running nil)

        (mc-save-directories)
        
        ;;restore previous window setup
        (delete-other-windows)

        (set-window-configuration mc-prior-window-configuration)

        (set-buffer mc-restore-buffer)

        ;;NOTE: never exit the recursive edit here.  functions should do this themselves
        (toggle-read-only -1))))

(defun mc-revert-buffer()
  "Revert the dired buffer"
  (interactive)

  (revert-buffer)

  (if (equal major-mode
             'dired-mode)
      (mc-mode 1)))
  
(defun mc-save-directories()
  "Save the current directories in the mc buffer to use the next time mc starts
  up."
  
  ;;update directory variables..

  (if (window-live-p mc-left-window)
      (progn
        (set-buffer (window-buffer mc-left-window))
        (if (equal major-mode
                   'dired-mode)
            (setq mc-left-directory (mc-get-dired-directory)))))

  (if (window-live-p mc-right-window)
      (progn
        (set-buffer (window-buffer mc-right-window))
        (if (equal major-mode
                   'dired-mode)
            (setq mc-right-directory (mc-get-dired-directory))))))

(defun mc-get-dired-directory()
  "Get the current dired directory."

  dired-directory)
  
(defun mc-dired(directory)
  "Turn mc and dired."
  (interactive
   (list 
    (read-file-name "Change directory (file or pattern): " nil nil nil)))

  ;;if the user specifies a file... open if with find file... else use dired
  ;;note that if it is not readable... then it must be a pattern

  (if (and (not (file-directory-p directory))
           (file-exists-p directory)
           (file-readable-p directory))
      (progn
        (mc-quit)
        (exit-recursive-edit)
        (find-file directory)))
  
  ;;if directory or pattern else...
  (if (and (file-directory-p directory)
           (file-readable-p directory))
      (progn
        (dired directory)
        (mc-mode 1))
    (find-file directory)))

(defun mc-ensure()
  "After dired changed to a new buffer, if mc mode is supposed to be on but
isn't... turn it on. "
  (interactive)
  
  (if mc-running
      (mc-mode 1)))

(defun mc-highlight()
  "Highlight the current buffer, destroying the previous buffer highlight if
  necessary."

  ;;update the last overlay
  (if mc-current-window-overlay
      (overlay-put mc-current-window-overlay 'face 'mc-window-not-selected-face))
  
  (save-excursion
    (let(begin end)

      ;;determine begining and end
      (goto-char (point-min))
      (search-forward "/" nil t)
      (setq begin (1- (point)))

      (search-forward ":" nil t)
      (setq end (1- (point)))

      ;;setup overlay
      (setq mc-current-window-overlay (make-overlay begin end))

      (overlay-put mc-current-window-overlay 'face 'mc-window-selected-face)

      (overlay-put mc-current-window-overlay 'window (selected-window)))))

(defun mc-dired-advertised-find-file()
  "Call dired-advertised-find-file but also perform additional actions"
  (interactive)

  ;;if the current line is not a directory ~exit..
  (save-excursion
    ;;(goto-char (point-at-bol))

    (let(filename)
      (setq filename (dired-get-filename))

      (if filename
          (if (file-directory-p filename)
              (progn
                (dired-advertised-find-file)
                (mc-mode 1))
            (progn
              
              (mc-quit)

              (find-file filename)
              ;;(dired-advertised-find-file)
              
              ;;(delete-other-windows)
              
              ;;need to make mc is pulled off the stack.
              (mc-quit)

              (exit-recursive-edit)))))))

(defadvice dired-view-file(after mc-dired-view-file())
  "See `dired-view-file'.  This version will quit MC after the file is viewed."
  (interactive)

  (if mc-mode
      (exit-recursive-edit)))
(ad-activate 'dired-view-file)

(defun mc-dired-prev-subdir()
  "Go to the previous subdirectory."
  (interactive)

  (if (not (string= dired-directory
                    "/"))
      (mc-dired "..")
    (error "Already at root")))

(defun mc-sort-buffer()
  "Go through the current dired buffer and sort it according to user settings."
  
  ;;TODO: the algorigthm is fairly fixed right now.  This should pay attention
  ;;to user settings on how they want to sort their buffer.

  (toggle-read-only -1)

  (save-excursion
    (goto-char (point-min))
    
    (let(match result)

      (setq result '())

      ;;pop out all directory based lines
      (while (re-search-forward "^..[ld].*/$" nil t)
        
        (setq match (format "%s\n" (match-string 0)))

        ;;add this match to the result list
        (add-to-list 'result
                     match)
        
        (delete-region (match-beginning 0) (match-end 0))
        (kill-line 1))      ;;now go through the list and insert each line into the buffer
      (goto-char (point-min))
      (forward-line 2)
      
      (let(i)
        (setq i (1- (length result)))
        (while (>= i 0)
          (insert (nth i result))
          (setq i (1- i))))))

  ;;make sure the directories ".." and "." are at the beginning fo the buffer
  (mc-sort-buffer-move-regexp "^.*[^.]\\.$" 2)
  (mc-sort-buffer-move-regexp "^.*\\.\\.$" 3)

  (toggle-read-only 1))

(defun mc-beginning-of-buffer()
  "Go to the first directory/file in dired."
  (interactive)

  (goto-char (point-min))
  (if (re-search-forward "\\.\\./$" nil t)
      (goto-char (match-beginning 0))
    (progn
      (goto-char (point-min))
      (dired-next-line 2))))

(defun mc-end-of-buffer()
  "Go to the last directory/file in dired."
  (interactive)

  (goto-char (point-max))
  (dired-next-line -1))

(defun mc-mode(&optional arg)
  "turn mc-mode on/off"
  (interactive)

  (if (if arg
          (> (prefix-numeric-value arg) 0)
        (not mc-mode))
      (mc-mode-on)
    (mc-mode-off)))

(defun mc-mode-on()
  "Turn on mc-mode."
  (interactive)
  (setq mc-mode t)
  
  (if (equal major-mode
             'dired-mode)
      (progn
        (mc-highlight)
        (font-lock-mode 1)

        ;;update the mode with the current directory

        (let(basic-line-format)

          (setq basic-line-format (concat " " (expand-file-name dired-directory)))
        
          (setq mode-line-format basic-line-format)

          ;;For GNU Emacs 21
          (if (functionp 'file-within-header)
              (file-within-header)))

        ;;if the point is below the .. directory... this is OK.  else set it to
        ;;the correct dir.

        (let(first-logic-point)
          (save-excursion
            (if (re-search-forward "\\.\\./$" nil t)
                (setq first-logic-point (match-beginning 0))))
          
          ;;if the current point is less than the idea point... first-logic-point....
          (if (and first-logic-point
                   (< (point) first-logic-point))
              (goto-char first-logic-point)))))
  
  (run-hooks 'mc-hook)
  (easy-menu-add mc-mode-menu))

(defun mc-mode-off()
  "Turn off mc-mode."
  (interactive)
  (setq mc-mode nil)
  (easy-menu-remove mc-mode-menu))

;;add this to thie list of known minor modes.
(add-to-list 'minor-mode-alist (list 'mc-mode " MC"))

;;directories which are symlinked.
(font-lock-add-keywords 'dired-mode '(("\\(^..l.*/$\\)" 1 'mc-symlink-directory-face keep)))

;;symbolic links (which do not end with a trailing slash
(font-lock-add-keywords 'dired-mode '(("\\(^..l.*[^/]$\\)" 1 'mc-symlink-face keep)))

(add-hook 'dired-after-load--hook 'mc-ensure)

;;need to have the dired buffer sorted after every read...
(add-hook 'dired-after-readin-hook 'mc-sort-buffer)

(setq dired-listing-switches "-alp")

(provide 'mc)

;;; mc.el ends here

