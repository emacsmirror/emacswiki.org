;;; mss.el - Make smart shortcuts to programs in your Win95/98/NT4 start menu

;; Copyright (C) 2000 Mathias Dahl

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; The best explanation of what this program does is to test it, but
;; I'll try to explain the best I can.

;; A more clear explanation of the concept itself can be seen on my
;; homepage: http://mathias.dahl.net/gqsmc.html

;; It's only useful (I think) if you are running NTEmacs with
;; Win95/98/NT4/2000. I now that in W98 and W2000, the system does
;; some smart placing of the shortcuts depending on how often you use
;; one.  I don't know how this will affect the behaviour of this
;; program, but I'll guess you have to test and see. I've heard that
;; you can turn off this feature in W98.

;; mss uses the standard windows Start Menu as the root for a shortcut
;; tree from where you can call your favourite shortcuts without
;; fiddling around with the mouse (of course you can use the mouse if
;; you want to).

;; As most of you know, you can activate the Start Menu with the
;; WIN-key (if you haven't got such a key, C-ESC does the same thing)
;; and the press the key that matches the menu choise you want.

;; The problem is when two or more menu choices begin with the same
;; key. This is why I place the shortcut tree under the folder "-". A
;; hyphen is not likely the first character of some other menu
;; choice. Of course you can use another valid character as the root
;; tree but I think the hyphen is easy to access on my swedish
;; keyboard.

;; Note that it doesn't matter if the root directory s called "-", "-
;; My smart shortcuts" or something, the important thing is that it
;; starts with an unusual character.

;; The program doesn't do anything that you cannot do yourself, it's
;; just a little more convenient. It uses standard windows Start Menu
;; functionality, just in a little different way.

;; This program will place a standard windows-shortcut file (actually
;; any kind of file) at the right place in your Start Menu "shortcut
;; tree". It will place it and name it in such a way that for you to
;; call your shortcut you only have to type Win - (the windows key
;; plus a hyphen) and the first few characters of the name of the
;; shortcut.

;; These characters depends on how many other shortcuts with similar
;; starting name you have. I.e if you only have one shorctut named
;; "Emacs.lnk", it will be placed just below the root of the tree and
;; to call it you'd type: Win - E (Windows key, a hyphen and an E).

;; As I said, the easiest way to see what it does is to test it. Make
;; a couple of shortcuts on your desktop (as this is the default path
;; for fetching shortcuts to place in the tree) and run the function
;; mss.

;; An example of how a shortcut tree can look like:

;; (START = language dependent path to start menu, taken from the
;; registry, for example d:\winnt\profiles\mathias\Start Menu\)

;; START\
;;       - My shorcuts\
;;                     C\H - cHaracter map
;;                       A - cAlc
;;                       Y - cYgwin
;;                     E - Emacs
;;                     N\O - nOtepad
;;                       E - nEtscape
;;                     O - Opera
;;                     W\I - wIndow blinds
;;                       E\
;;                         B\W - webWasher
;;                           T - webTime

;; Asuming you have a tree in the example above, if I wanted to place
;; a shortcut named Explorer, I'd have to create a subdirectory and
;; rename the Emacs shortcut and then place this new shortcut so that
;; the new tree would look like this:

;;  E\M - eMacs
;;    X - eXplorer

;; And maybe later, I want a shortcut to Excel, this would look like
;; this:

;;  E\M - eMacs
;;    X\C - exCel
;;      P - exPlorer

;; The result is a unique sequence of characters for each shortcut. To
;; start Excel you'd type: Win - e x c

;; This means that you dont have to remember strange key combinations,
;; just remember the name of the shortcut and type on until it starts.

;; To run the mss-function itself, I have a shortcut called mss that
;; uses gnudoit to raise or start Emacs and call mss.

;; Example of shortcut command line for starting mss:

;; %HOME%\bin\gnudoit.exe [continued on the next line] (progn
;; (load-library \"mss\") (raise-frame (selected-frame)) (mss))

;;; History

;;  2000-04-01  Mathias Dahl       0.0.1

;;                                 First release

;;  2000-04-03  Mathias Dahl       0.0.2

;;                                 Made all free variables local (I think)

;;                                 Bug corrected: the user could
;;                                 choose and invalid path for the
;;                                 shortcut tree and this gave
;;                                 *strange* effects.

;;                                 The user can now choose whether to
;;                                 move or copy the shortcut with the
;;                                 variable mss-move-shortcut-flag.

;;                                 Handles the situation when the user
;;                                 wants both these shortcuts
;;                                 (example): hello.lnk and helloo.lnk

;;  2000-04-13  Mathias Dahl       0.0.3

;;                                 Noticed problems with
;;                                 null-characters (ascii 0) in
;;                                 mss.tmp. These characters were not
;;                                 there in NT4.  Had to rewrite parts
;;                                 of mss-get-shell-folder-path.  All
;;                                 in all, some things dont seem to
;;                                 work in W2k

;; 2000-04-14   Mathias Dahl       0.0.4

;;                                 Fixed minor problems (found some
;;                                 more free vars)

;; 2000-04-15   Mathias Dahl       0.0.5

;;                                 Added simple drag-n-drop
;;                                 functionality. This is provided
;;                                 with the mss-drag-n-drop-mode

;;; Bugs

;; There are probably situations with unknown outcome that can
;; arise. Not much error catching for now.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar mss-quick-menu-path "c:/Documents and Settings/mdahse/Start Menu/-/"
  "Path to the root of the smart shortcut tree. If nil, prompt
for the path and use ...\\Start Menu\\-\\ as default.")

(defvar mss-get-shortcut-path nil
  "Path to where mss should default to when picking up your
shortcut. If nil this defaults to the users desktop")

(defvar mss-move-shortcut-flag t
  "This flag determines whether mss should MOVE or COPY the
shortcut to the desired destination. nil means COPY, non-nil
means MOVE")

(defvar mss-frame nil
  "Frame for dropping shortcuts")

(defun mss (&optional FULL-NAME)
  "Start-function for placing shortcut in shortcut tree. The
optional parameter FULL-NAME is the full path name of the
shortcut being placed"
  (interactive)
  
  (while 
      (or
       (not mss-quick-menu-path)
       (not (file-directory-p mss-quick-menu-path))
       (not (string-match "/$" (expand-file-name mss-quick-menu-path))))
    (progn
      (setq mss-quick-menu-path
            (expand-file-name 
             (read-file-name 
              "Path to root of tree: " 
              (concat (mss-get-shell-folder-path "Start Menu") "/") nil t "-")))))  
  (if (or (not mss-get-shortcut-path)
          (not (file-directory-p mss-get-shortcut-path)))
      (setq mss-get-shortcut-path 
            (expand-file-name 
             (concat (mss-get-shell-folder-path "Desktop") "/"))))
  
  (while (or
          (not FULL-NAME)
          (not (file-regular-p FULL-NAME)))
    (setq FULL-NAME 
          (expand-file-name 
           (read-file-name "Select shortcut: " mss-get-shortcut-path))))
  
  (mss-place-shortcut 
   (file-name-nondirectory FULL-NAME) mss-quick-menu-path 0  FULL-NAME))

(defun mss-place-shortcut (file dir pos full-name)
  "Makes the actual recursive lookup of a nice place to put our
shortcut.  If an old shortcut in the current directory begins
with the same letter, we create a sub-directory, rename and move
this shortcut there and finally put our new shortcut in the same
directory. Repeat this process until the current character of the
shortut is unique."
  (let ((char (substring file pos (1+ pos)))
        (new (concat dir (mss-quick-name file pos)))
        (match nil))
    (setq match (directory-files dir nil (concat "^[" char (upcase char) "]")))
    (if (string= char ".")
        (progn
          (copy-file full-name new)
          (error "mss.el: Sorry, \".\" found. Maybe filename is too short. Leaving file here: %s" new)))
    (if match
        (progn
          (if (cdr match)
              (progn
                (error "mss.el: Corrupt quick menu structure (more than one file/dir begins with same letter"))
            (progn
              (if (file-directory-p (concat dir (car match)))
                  (progn
                    ;; Mathing file is a directory
                    (if (> (length (car match)) 1)
                        (error "mss.el: Corrupt quick menu structure (dir length > 1)")
                      (mss-place-shortcut file (concat dir (car match) "/") (1+ pos) full-name)))
                (progn
                  ;; Matching file is a file, not a directory, we have to create sub-directory
                  (make-directory (concat dir (upcase char)))
                  (rename-file (concat dir (car match)) (concat dir char "/" (mss-make-new-quick-name (car match) pos)))
                  (mss-place-shortcut file (concat dir char "/") (1+ pos) full-name))))))
      (progn
        ;; No file with the same leading char as the char 
        ;; in the current pos of our shortcut name
        (if (file-exists-p new)
            (error "mss.el: A shortcut with the name %s already exists in directory %s " file dir)
          (if   mss-move-shortcut-flag
              (rename-file full-name new)
            (copy-file full-name new)))))))

(defun mss-files-with-letter (directory letter)
"Returns a list of files/dirs that begin with `letter´ in
directory `dir´"
  (directory-files directory nil (concat "^[" letter (upcase letter) "]")))

(defun mss-quick-name (short-name pos)
  "Generates the `quick-name´ of the current shortcut.  I.e
netscape becomes E - nEtscape if it is placed under the
sub-directory `N´"
  (let ((char (upcase (substring short-name pos (1+ pos)))))
    (concat char
            " - " 
            (downcase (substring short-name 0 pos))
            char
            (downcase (substring short-name (1+ pos))))))

(defun mss-make-new-quick-name (quick-name pos)
"Short-hand for calling mss-quick-name when moving old shortcut
out of the way"
  (mss-quick-name (substring quick-name 4) (1+ pos)))

(defun mss-get-shell-folder-path (folder-name)
  (let
      ((temp-file)
       (buffer-text)
       (folder-path))
    (setq temp-file (concat (getenv "TEMP") "\\mss.tmp"))
    
    (call-process "cmd.exe" nil "*mss*" nil
                  "/c" "regedit" "/e" temp-file
                  "HKEY_CURRENT_USER\\Software\\Microsoft\\Windows\\CurrentVersion\\Explorer\\Shell Folders")

    (find-file temp-file)  
    (setq buffer-text (buffer-substring (point-min) (point-max)))
    (kill-buffer "mss.tmp")

    (set-buffer "*mss*")
    (kill-region (point-min) (point-max))
    (insert buffer-text)
    
    (goto-char (point-min))
    (while (search-forward "\000" nil t)
      (replace-match "" nil t))

    (goto-char (point-min))
    (while (search-forward "\015" nil t)
      (replace-match "" nil t))

    (goto-char (point-min))
    
    (re-search-forward (concat "^." folder-name ".=.\\(.*\\).$"))
    (setq folder-path (match-string 1 folder-path))
    (kill-buffer "*mss*")
    
    (while (string-match "\\(\\\\\\\\\\\)" folder-path)
      (match-string 1 folder-path)
      (setq folder-path (replace-match "\\" nil t folder-path)))
    
    (setq folder-path (expand-file-name folder-path))
    
    (if (file-exists-p temp-file)
        (delete-file temp-file))
  
  folder-path))

;; Ripped w32-drag-n-drop and modified it a bit

(defun mss-drag-n-drop (event)
  "Run mss on the files listed in the drag-n-drop event."
  (interactive "e")
  (save-excursion
    ;; Make sure the drop target has positive co-ords before setting
    ;; the selected frame - otherwise it won't work.
    ;; <skx@tardis.ed.ac.uk>
    (let* ((window (posn-window (event-start event)))
           (coords (posn-x-y (event-start event)))
           (x (car coords))
           (y (cdr coords)))
      ;;(if (and (> x 0) (> y 0))
      ;;          (set-frame-selected-window nil window))
      (mapcar 'mss (car (cdr (cdr event)))))))

(defvar mss-drag-n-drop-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m [drag-n-drop] 'mss-drag-n-drop)
    (define-key m "q" 'mss-delete-frame)
    m)
  "Keymap for mss-drag-n-drop-mode")

(defun mss-delete-frame ()
  (interactive)
  (kill-buffer "*mss-drag-n-drop*")
  (delete-frame mss-frame))

(defun mss-drag-n-drop-mode ()
  "Mode for dropping shortcuts for mss to swallow
\\{mss-drag-n-drop-mode-map}"
  (interactive)
  (if (or
         (not mss-frame)
         (not (frame-live-p mss-frame)))
        (setq mss-frame 
              (make-frame
               '((name . "mss-drag-n-drop-frame")
                 (minibuffer . nil)
                 (user-position . t)
                 (width . 27)
                 (height . 1)
                 (vertical-scroll-bars . nil)
                 (menu-bar-lines . 0)))))

    (raise-frame mss-frame)
    (select-frame mss-frame)

    (if (not (string-equal (buffer-name) "*mss-drag-n-drop*"))
        (switch-to-buffer  "*mss-drag-n-drop*"))

    (toggle-read-only 0)
    
    (kill-region (point-min) (point-max))
    (insert "[Drop your shortcuts here]")

    (toggle-read-only 1)

    (kill-all-local-variables)  
    (setq major-mode 'mss-drag-n-drop-mode)
    (setq mode-name "mss-drag-n-drop")
    (use-local-map mss-drag-n-drop-mode-map)

    (setq mode-line-format "   << MSS-DRAG-N-DROP >>")
    (force-mode-line-update)

  (run-hooks 'mss-drag-n-drop-mode-hook))

(provide 'mss)
;;; mss.el ends here
