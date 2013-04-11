;;; one-key-regs.el --- Code for handling registers, macros and bookmarks using one-key menus.

;; Filename: one-key-regs.el
;; Description: Code for handling registers, macros and bookmarks using one-key menus.
;; Author: Joe Bloggs <vapniks@yahoo.com>
;; Maintainer: Joe Bloggs <vapniks@yahoo.com>
;; Copyleft (Ↄ) 2012, Joe Bloggs, all rites reversed.
;; Created: 2012-01-22 16:27:08
;; Version: 0.1
;; Last-Updated: 2012-01-22 16:27:08
;;           By: Joe Bloggs
;; URL: http://www.emacswiki.org/emacs/download/one-key-regs.el
;; Keywords: abbrev, convenience, files, frames, tools
;; Compatibility: GNU Emacs 24.0.50.2
;;
;; Features that might be required by this library:
;;
;; one-key.el, cl.el, ido.el
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Bitcoin donations gratefully accepted: 1HnSqGHrVenb1t2V2aijyocWyZcd7qt1k

;; This library adds a new menu type to `one-key' called `one-key-registers' which creates a menu
;; for the currently loaded registers in `register-alist'. For more information on one-key see `one-key.el'.
;; Registers may be edited, created and deleted from within the `one-key-registers' menu, and many
;; new types of registers are available (listed below). When new registers are added they are automatically
;; coloured according to type, and default menu item descriptions are created.
;; You may specify a set of key modifiers and normal keys for executing the registers when the menu is not
;; displayed (see the "Installation" section below).
;; You can also load/save different sets of registers.
;; Finally, you may also define register "key queues" (see `one-key-regs-key-queues' and `one-key-regs-enable-key-queues').
;; These are sequences of keys such that when a new register is stored in one of the keys, the registers currently
;; stored in that key and all subsequent keys are shifted down the queue. This is useful when storing buffer
;; positions in registers, and works a bit like the `mark-ring' in normal emacs usage.
;;
;; To see the various commands available press the f1 key in the `one-key-registers' menu.
;; This will display the help window for `one-key-registers'.

;; For further information on how to use one-key see `one-key.el'.


;;; Executing registers:

;; To executed a stored register either use the `one-key-registers' menu or, if defined (see "Installation" section),
;; press the associated quick key (in combination with the modifier keys).

;;; Creating registers:
;;
;; To create a new register you can either press the keybinding for "Add a register" in the `one-key-registers'
;; menu, or press C-u followed by a quick keybinding (with modifier keys) for a register (see "Installation" below).
;; In both these cases you will be prompted for the register type.
;; Alternatively you may use one of the prefix keys defined in `one-key-regs-prefix-key-associations' to create 
;; registers of different types. If no prefix key is used, and there is no register stored in the quick key then
;; a register of type `one-key-regs-default-register-type' will be created if no region is defined or
;; `one-key-regs-default-region-register-type' if region is defined.

;;; The following register types (and associated creation actions) are available:

;; buffer-marker : current cursor position and buffer (can only be executed if the buffer is available)
;; file-marker : current cursor position and file (file will be opened if not already open)
;; text-region : current text region 
;; cut-text-region : current text region (cuts and stores current region when created)
;; rectangle : rectangle defined by current region
;; cut-rectangle : rectangle defined by current region (cuts rectangle when created)
;; number : number under point 
;; window-config : current window configuration
;; frame-config : current frame configuration 
;; delete-register : deletes the specified register
;; macro : last stored keyboard macro.
;; bookmark : prompts for a bookmark to store.
;; new-bookmark : creates a new bookmark at point.
;; buffer : current buffer. Position of cursor is not stored.
;; file-or-dir : current file or directory (for dired buffers). Position of cursor is not stored.
;; browse-url : prompts for a URL, and opens it with `browse-url' when executed.
;; emacs-command : prompts for an emacs command.
;; eval-sexp : prompts for an sexp.
;; info-file : prompts for an info file.

;;; You may also create your own register types. See `one-key-regs-custom-register-types' for details.

;;
;; TIP: when creating registers its a good idea to reserve different sets of keys for major-mode
;; specific registers, project specific registers and general registers (as saved in `one-key-regs-default-file'),
;; e.g. numbers for major-mode specific, small letters for project-specific and capital letters
;; for general registers. Then you can merge in a new register set when you change buffer
;; without affecting the project or general registers, and similarly when you change project.

;;; Saving/loading/merging register sets:

;; When one-key-regs starts it will load the registers and menu stored in `one-key-regs-default-file'
;; (along with other options defined in `one-key-regs-save-items').
;; You may decide to alter these registers and store them in a different file by pressing C-s in the
;; `one-key-registers' menu. You may then load them at a later time by pressing C-l or M-l in the `one-key-registers'
;; menu. C-l will completely remove all current registers before loading the new ones, and M-l will merge the
;; new registers with the old ones according to the value of `one-key-regs-merge-conflicts'.
;; The default directory for storing register sets is `one-key-regs-default-directory'.

;;; Installation:
;;
;; Put one-key-regs.el in a directory in your load-path, e.g. ~/.emacs.d/
;; You can add a directory to your load-path with the following line in ~/.emacs
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;; where ~/elisp is the directory you want to add
;; (you don't need to do this for ~/.emacs.d - it's added by default).
;;
;; Add the following code to your ~/.emacs startup file:
;;
;; (require 'one-key-regs)
;;
;; You can then add a menu of type `one-key-registers' to the *One-Key* window by pressing the appropriate
;; key for adding a menu (press f1 to see the current special keybindings).
;; You may also want to configure a menu set in `one-key-sets-of-menus-alist' to hold the `one-key-registers' menu.
;; See the documentation for `one-key' for further information.

;; If you also want to bind the individual registers to keys, choose a set of key modifiers that are not in use
;; (e.g. control+super) and add the following to your ~/.emacs file after the previous code:
;;
;; (one-key-regs-define-keybindings '(control super))
;;
;; This will bind all keystrokes formed by combining the modifier keys with the keys defined in `one-key-regs-quick-keys'
;; to the associated registers.


;; The following elisp packages are also required: one-key.el
;; These are available from the emacswiki: http://www.emacswiki.org

;;; Customize:
;;
;;  `one-key-regs-custom-register-types' : A list of different types of registers for use with one-key-regs.
;;  `one-key-regs-colours-alist' : Association list of colours for the different register types.
;;  `one-key-regs-special-keybindings' : List of special keys to be used for one-key-registers menus
;;                                       (see `one-key-default-special-keybindings' in one-key.el for more info).
;;  `one-key-regs-show-legend' : Whether or not to show the `one-key-regs-legend-string' in the one-key menu.
;;  `one-key-regs-colourize-menu' : Whether to colourize the menu items (with colours in `one-key-regs-colours-alist')
;;                                  or not.
;;  `one-key-regs-winconfig-restore-point' : If non-nil then point (cursor position) will be restored when a window
;;                                           or frame config register is executed.
;;  `one-key-regs-prompt-for-description' : If set to t then you will be prompted for a description when creating a
;;                                          new one-key register.
;;  `one-key-regs-prefix-key-associations' : An alist associating prefix keys with register types.
;;  `one-key-regs-default-register-type' : The default register type to create when filling an empty register.
;;  `one-key-regs-default-region-register-type' : The default register type to create when the region is active.
;;  `one-key-regs-save-items' : A list of items to save along with register sets.
;;  `one-key-regs-save-on-exit' : A regular expression to match register filenames that will be saved on exit if
;;                                currently loaded.
;;  `one-key-regs-max-description-length' : The maximum number of chars allowed in the register description shown
;;                                          in the `one-key' menu.
;;  `one-key-regs-default-directory' : The default directory in which to store one-key-regs sets.
;;  `one-key-regs-default-file' : The default registers file to load on startup.
;;  `one-key-regs-merge-conflicts' : What method to use to handle conflicts when loading new registers.
;;  `one-key-regs-quick-keys' : List of keys to use with `one-key-regs-define-keybindings'.
;;  `one-key-regs-key-queues' : List of register key queues. 
;;  `one-key-regs-enable-key-queues' : Whether register key queues should be enabled or not.

;; 
;; 
;; All of the above can customized by:
;;      M-x customize-group RET one-key-regs RET
;;

;;; Change log:
;;	
;; 2012/01/22
;;      * First released.
;; 

;;; Acknowledgements:
;;
;; 
;;

;;; TODO
;; 
;; Update documentation.


;;; Require
(require 'one-key)
(require 'one-key-dir)
(eval-when-compile (require 'cl))
;;; Code:

(defgroup one-key-regs nil
  "Convenient access and backup of many different kinds of registers using one-key menus, and keybindings."
  :group 'one-key)

(defvar one-key-regs-reserved-register-types
  (list '(buffer-marker point-to-register
                        (lambda (reg)
                          (let ((buf (if (and (markerp reg) (marker-buffer reg))
                                         (marker-buffer reg)
                                       (get-buffer (second reg))))
                                (pos (if (markerp reg)
                                         (marker-position reg)
                                       (third reg))))
                            (if buf (one-key-regs-get-marker-description buf pos)
                              (if (second reg) (format "Pos %d in %s" pos (second reg))
                                "buffer-marker"))))
                          (lambda (val) (or (markerp val) (and (listp val) (eq (car val) 'buffer-marker))))
                          (lambda (val)
                            (cond ((and (markerp val) (marker-buffer val))
                                   (switch-to-buffer (marker-buffer val)) (goto-char val))
                                  ((and (eq (car val) 'buffer-marker) (get-buffer (second val)))
                                   (switch-to-buffer (second val)) (goto-char (third val)))
                                  (t (error "That register's buffer no longer exists"))))
                          (lambda (char contents)
                            (let* ((bufname (if (markerp contents) (buffer-name (marker-buffer contents))
                                              (second contents)))
                                   (buf (get-buffer bufname))
                                   (file (if buf (buffer-file-name buf)))
                                   (pos (if (markerp contents) (marker-position contents) (third contents))))
                              (if file
                                  `(set-register ,char (quote (file-query ,file ,pos)))
                                `(set-register ,char (quote (buffer-marker ,bufname ,pos)))))))
        '(file-marker (lambda (char) (set-register char (list 'file-query buffer-file-name (point))))
                      (lambda (reg) (let* ((filename (second reg))
                                           (buf (find-file-noselect filename t))
                                           (pos (third reg)))
                                      (one-key-regs-get-marker-description buf pos)))
                      (lambda (val) (and (consp val) (eq (car val) 'file-query)))
                      (lambda (val) (find-file (second val)) (goto-char (third val)))
                      (lambda (char contents) `(set-register ,char (quote ,contents))))
        '(text-region (lambda (char) (if (use-region-p)
                                         (progn (copy-to-register char (region-beginning) (region-end))
                                                (deactivate-mark))
                                       (set-register char (read-string "Text: "))))
                      (lambda (reg) (format "Text: %s" (substitute ?\C-l ?\n reg)))
                      (lambda (val) (stringp val))
                      (lambda (val) (insert val))
                      (lambda (char contents) `(set-register ,char ,contents)))
        '(cut-text-region (lambda (char) (if (use-region-p)
                                             (progn (copy-to-register char (region-beginning) (region-end) t)
                                                    (deactivate-mark))
                                           (set-register char (read-string "Text: "))))
                          (lambda (reg) (format "Text: %s" (substitute ?\C-l ?\n reg)))
                          (lambda (val) (stringp val))
                          (lambda (val) (insert val))
                          (lambda (char contents) `(set-register ,char ,contents)))
        '(rectangle (lambda (char) (if (use-region-p)
                                       (progn (copy-rectangle-to-register char (region-beginning) (region-end))
                                              (deactivate-mark))
                                     (set-register char (one-key-regs-string-split
                                                         (read-string "Rectangle: ") "\n"))))
                    (lambda (reg) (format "Rect: %s" (mapconcat 'identity reg " ")))
                    (lambda (val) (and (consp val) (stringp (car val))))
                    (lambda (val) (insert-rectangle val))
                    (lambda (char contents) `(set-register ,char (quote ,contents))))
        '(cut-rectangle (lambda (char) (if (use-region-p)
                                           (progn (copy-rectangle-to-register char (region-beginning) (region-end) t)
                                                  (deactivate-mark))
                                         (set-register char (one-key-regs-string-split
                                                             (read-string "Rectangle: ") "\n"))))
                        (lambda (reg) (format "Rect: %s" (mapconcat 'identity reg " ")))
                        (lambda (val) (and (consp val) (stringp (car val))))
                        (lambda (val) (insert-rectangle val))
                        (lambda (char contents) `(set-register ,char (quote ,contents))))
        '(number (lambda (char) (number-to-register nil char))
                 (lambda (reg) (format "Number: %d" reg))
                 (lambda (val) (numberp val))
                 (lambda (val) (princ val (current-buffer)))
                 (lambda (char contents)
                   `(set-register ,char ,contents)))
        '(window-config window-configuration-to-register
                        (lambda (reg) "Window config")
                        (lambda (val) (and (consp val) (window-configuration-p (car val))))
                        (lambda (val) (set-window-configuration (car val))
                          (if one-key-regs-winconfig-restore-point (goto-char (second val))))
                        nil)
        '(frame-config frame-configuration-to-register
                       (lambda (reg) "Frame config")
                       (lambda (val) (and (consp val) (frame-configuration-p (car val))))
                       (lambda (val) (set-frame-configuration (car val))
                         (if one-key-regs-winconfig-restore-point (goto-char (second val))))
                       nil)
        '(delete-register (lambda (char) (if (y-or-n-p (format "Remove \"%c\" register?" char))
                                             (progn (setq register-alist (assq-delete-all char register-alist))
                                                    (message "Register \"%c\" deleted" char))))
                          (lambda (reg) "")
                          ignore
                          ignore))
  "A list of reserved register types and associated functions/code.
Each item in the list contains (in this order):

  1) A symbol name for the register type.

  2) A function which takes a single char argument and stores the appropriate register type in that char.

  3) A function which takes the contents of a register of this type as argument and returns a default label
     for the register.

  4) A predicate function which takes the contents of a register as argument and returns true if
     it is the correct type.

  5) A function which takes the contents of a single register as argument and performs the appropriate actions on it for
     registers of this type (e.g. jump to marker, insert text, etc).

  6) Either nil or a function which takes two arguments: the char and contents of a register (in that order), and returns
     an elisp form which will recreate the register when evalled, e.g. (set-register ?a '(file-query \"foo.txt\" 1234))
     This form is used for saving the state of the register to a file with the `one-key-regs-save-registers' function.
     If this element is nil then registers of this type will not be saved.

This variable should not be altered by the user.
Instead you may add your own types to `one-key-regs-custom-register-types'.
Any new register type defined in `one-key-regs-custom-register-types' cannot share the same name as one of these reserved types.")

(defcustom one-key-regs-custom-register-types
  '((macro
     `(execute-kbd-macro ,last-kbd-macro)
     (lambda (reg) (format "Macro: %s" (delete ?  (key-description (caddr reg))))))
    (bookmark
     `(bookmark-jump ,(completing-read "Jump to bookmark: " (mapcar 'car bookmark-alist)))
     (lambda (reg) (format "Bookmark: %s" (caddr reg))))
    (new-bookmark
     `(bookmark-jump
       ,(let ((name (completing-read "Set bookmark " (mapcar 'car bookmark-alist))))
          (bookmark-set name t) name))
     (lambda (reg) (format "Bookmark: %s" (caddr reg))))
    (buffer
     `(switch-to-buffer ,(buffer-name))
     (lambda (reg) (format "Buffer: %s" (caddr reg))))
    (file-or-dir
     `(let ((buf (or ,(buffer-file-name) ,dired-directory)))
        (if buf (find-file buf) (message "No file associated with buffer!")))
     (lambda (reg) (let* ((path (or (car (cdadar (caddr reg)))
                                    (cadr (cdadar (caddr reg)))))
                          (file (file-name-nondirectory path)))
                     (if (equal file "") (format "Dir: %s" path)
                       (format "File: %s" file)))))
    (browse-url
     `(browse-url ,(read-string "URL: " "http://"))
     (lambda (reg) (format "URL: %s" (caddr reg))))
    (emacs-command
     `(call-interactively ',(read-command "Command: "))
     (lambda (reg) (format "M-x %S" (car (cdaddr reg)))))
    (eval-sexp
     `(eval ',(read-from-minibuffer "Eval: " nil read-expression-map t 'read-expression-history))
     (lambda (reg) (format "(eval %S)" (car (cdaddr reg)))))
    (info-file
     `(let* ((node ,(read-string "Info file: "))
             (infobuf (loop for buf being the buffers
                            if (and (string-match "\\*info\\(<[0-9]+>\\)?\\*" (buffer-name buf))
                                    (with-current-buffer buf
                                      (string-match node Info-current-file)))
                            do (return buf))))
        (if infobuf (switch-to-buffer infobuf)
          (info node)))
     (lambda (reg) (format "%s *info*" (cadr (caaddr reg))))))
  "A list of different types of registers for use with one-key-regs.
Each type contains three elements in this order:

  1) A symbol name to identify the type.

  2) An sexp which is evaluated when the register is created.
     It should return another sexp which will be stored in the register,
     and evaluated when the register is executed.

  3) A function which takes the contents of a register of this type as argument and returns a default label
     for the register. It is called when the register is created.

You can use the backquote to evaluate parts of an elisp form at register creation time instead of execution time.
This allows you to include information about the current marker position, region, etc.
E.g:
                   `(insert ,(buffer-substring (region-beginning) (region-end)))

See Info node `(elisp)Backquote' for more details.

Note: when saving custom registers the sexp stored in the register contents will be saved. This may or may not have
the intended effect when loaded and executed in a new emacs session (bear this in mind when creating custom types)."
  :group 'one-key-regs
  :type '(repeat (list (symbol :tag "Name" :help-echo "A name to identify the type."
                               :match (lambda (w name)
                                        (not (memq name (mapcar 'car one-key-regs-reserved-register-types)))))
                       (sexp :tag "Creation sexp" :help-echo "An sexp which evaluates to another sexp when the register is created.
The second sexp is stored in the register and will be evaluated when the register is executed.")
                       (sexp :tag "Label sexp" :help-echo "An sexp which evaluates to a default label for the register when the register is created."))))

(defcustom one-key-regs-colours-alist '((buffer-marker . "steel blue")
                                        (file-marker . "cadet blue")
                                        (buffer . "steel blue")
                                        (file-or-dir . "cadet blue")
                                        (text-region . "firebrick")
                                        (rectangle . "dark magenta")                                        
                                        (cut-text-region . "firebrick")
                                        (cut-rectangle . "dark magenta")
                                        (number . "dim gray")
                                        (window-config . "chocolate")
                                        (frame-config . "goldenrod")
                                        (macro . "dark green")
                                        (bookmark . "OrangeRed1")
                                        (browse-url . "cyan1")
                                        (new-bookmark . "OrangeRed1")                                        
                                        (emacs-command . "DarkOliveGreen4")
                                        (eval-sexp . "khaki4")
                                        (info-file . "aquamarine4"))
  "Association list of colours for the different register types.
Each element is a (NAME . COLOUR) pair where NAME is the name of a register type (as a symbol),
and COLOUR is the name of the associated colour to use in the `one-key' menu."
  :group 'one-key-regs
  :type '(alist :key-type (symbol :tag "Register type") :value-type color)
  :set (lambda (symb val)
         (setq one-key-regs-legend-string
               (loop for (sym . colour) in val
                     for name = (symbol-name sym)
                     concat (propertize name 'face
                                        (list :background colour
                                              :foreground one-key-item-foreground-colour))))
         (set-default symb val)))

;; Add new special keybindings for one-key-registers menus
(customize-set-variable 'one-key-special-keybindings
                        (one-key-add-elements-to-alist
                         'one-key-special-keybindings
                         '((show-register help "Display register contents"
                                          (lambda nil
                                            (let* ((key (read-event "Enter the key for the register to display"))
                                                   (reg (assoc key register-alist)))
                                              (if reg
                                                  (message "Register \"%c\" contains: %S" key (cdr reg))
                                                (message "No register with key \"%c\"" key))
                                              (setq match-recursion-p t)) t))
                           (edit-registers-file "C-f" "Edit the saved registers file"
                                                (lambda nil
                                                  (with-selected-window (next-window)
                                                    (one-key-regs-open-registers-file))
                                                  (setq one-key-menu-window-configuration nil) nil))
                           (save-registers save-menu "Save registers and menu"
                                           (lambda nil (one-key-regs-save-registers
                                                        one-key-regs-currently-loaded-file
                                                        t) t))
                           (edit-register edit-item "Edit a register"
                                          (lambda nil (one-key-regs-edit-menu-item
                                                       one-key-buffer-filtered-list
                                                       one-key-buffer-full-list) t))
                           (delete-register delete-item "Delete a register"
                                            (lambda nil (one-key-regs-delete-menu-item
                                                         one-key-buffer-filtered-list
                                                         one-key-buffer-full-list) t))
                           (swap-register-keys swap-keys "Swap register keys"
                                               (lambda nil (one-key-regs-swap-menu-items
                                                            one-key-buffer-full-list) t))
                           (add-register add-item "Add a register"
                                         (lambda nil (one-key-regs-prompt-to-add-menu-item
                                                      one-key-buffer-filtered-list
                                                      one-key-buffer-full-list) t))
                           (show-register-prefix-keys "C-p" "Show prefix associations"
                                                      one-key-regs-show-prefix-key-associations)
                           (clear-registers "<C-f6>" "Delete all registers"
                                            (lambda nil (one-key-regs-clear-registers) t))
                           (replace-registers "C-l" "Load registers (replace)"
                                              (lambda nil (one-key-regs-open-register-sets-menu "replace") t))
                           (merge-registers "M-l" "Load registers (merge)"
                                            (lambda nil (one-key-regs-open-register-sets-menu "prompt") t))
                           (regs-documentation documentation "Show one-key-regs documentation"
                                               (lambda nil (finder-commentary (locate-library "one-key-regs"))
                                                 (setq one-key-menu-window-configuration nil)
                                                 nil))
                           ) t))

(defcustom one-key-regs-special-keybindings
  (one-key-add-elements-to-list
   'one-key-general-special-keybindings
   '(show-register show-register-prefix-keys save-registers merge-registers replace-registers regs-documentation limit-items
                   highlight-items edit-register delete-register clear-registers swap-register-keys add-register add-menu
                   remove-menu move-item donate report-bug))
  "List of special keys to be used for one-key-registers menus (see `one-key-default-special-keybindings' for more info)."  
  :group 'one-key-regs
  :type '(repeat (symbol :tag "Name" :help-echo "The name/symbol corresponding to the keybinding.")))

(defvar one-key-regs-legend-string nil
  "A coloured string displayed in the menu indicating which colours correspond with which register types.")

(defcustom one-key-regs-show-legend t
  "Whether or not to show the `one-key-regs-legend-string' in the one-key menu."
  :group 'one-key-regs
  :type '(boolean))

(defcustom one-key-regs-colourize-menu t
  "Whether to colourize the menu items (with colours in `one-key-regs-colours-alist') or not."
  :group 'one-key-regs
  :type '(boolean))

(defcustom one-key-regs-winconfig-restore-point nil
  "If non-nil then point (cursor position) will be restored when a window or frame config register is executed.
Note that this refers to the position of point in the window from which the window/frame-config was originally saved.
Default value is nil which differs from emacs default behaviour (which is to restore point)."
  :group 'one-key-regs
  :type '(boolean))

(defcustom one-key-regs-prompt-for-description nil
  "If set to t then you will be prompted for a description when creating a new one-key register.
If you select the register type from a prompt (by using a non-numeric prefix key with `one-key-regs-function')
you will be prompted for a description regardless of the value of this variable.
Otherwise a default label for the register will be created using the appropriate function in `one-key-regs-reserved-register-types' or `one-key-regs-custom-register-types'."
  :group 'one-key-regs
  :type '(boolean))

(defcustom one-key-regs-prefix-key-associations nil
  "An alist associating prefix keys with register types.
Each element of the alist is of the form (number . name) where number is the numeric prefix arg, and name is an elisp
symbol for the name of the associated register type.
When the `one-key-regs-function' is called with a numeric prefix key it will check this list to see what kind of
register to create. If a normal C-u prefix is used or the numeric prefix doesn't occur in the list then the user
will be prompted for a register type to create."
  :group 'one-key-regs
  :type '(alist :key-type (integer :tag "Numeric prefix arg"
                                   :help-echo "A numeric prefix arg to associate with the register type")
                :value-type (symbol :tag "Register type"
                                    :help-echo "The name of the register type (a lisp symbol)"
                                    :match (lambda (w name)
                                             (or (memq name (mapcar 'car one-key-regs-custom-register-types))
                                                 (memq name (mapcar 'car one-key-regs-reserved-register-types))))))
  :set (lambda (name value)
         (set name (sort value (lambda (x y) (> (car x) (car y)))))))

(defcustom one-key-regs-default-register-type 'buffer-marker
  "The default register type to create when filling an empty register.
If the quick key corresponding to an empty register is pressed, and no prefix key was pressed beforehand then a new
register of this type will be created."
  :group 'one-key-regs
  :type '(symbol :tag "Register type" :help-echo "The name of an existing register type (a lisp symbol)"
                 :match (lambda (w name)
                          (or (memq name (mapcar 'car one-key-regs-custom-register-types))
                              (memq name (mapcar 'car one-key-regs-reserved-register-types))))))

(defcustom one-key-regs-default-region-register-type 'region
  "The default register type to create when the region is active.
If a register quick key is pressed when the region is active, then a register of this type will be created,
unless a prefix key was pressed beforehand in which case the associated register type in `one-key-regs-prefix-key-associations' will be created instead."
  :group 'one-key-regs
  :type '(symbol :tag "Register type" :help-echo "The name of an existing register type (a lisp symbol)"
                 :match (lambda (w name)
                          (or (memq name (mapcar 'car one-key-regs-custom-register-types))
                              (memq name (mapcar 'car one-key-regs-reserved-register-types))))))

(defcustom one-key-regs-save-items nil
  "A list of items to save along with register sets.
Can include the following items: `one-key-regs-default-register-type',
`one-key-regs-default-region-register-type', and `one-key-regs-prefix-key-associations'.
If any of these variables are selected then any such values saved in the registers file will override customized values
the next time the registers are loaded.
This allows you to have different values for different register sets."
  :group 'one-key-regs
  :type '(set (const :tag "Default register type" one-key-regs-default-register-type)
              (const :tag "Default region register type" one-key-regs-default-region-register-type)
              (const :tag "Prefix key associations" one-key-regs-prefix-key-associations)))

(defcustom one-key-regs-save-on-exit nil
  "A regular expression to match register filenames that will be saved on exit if currently loaded.
If the value of `one-key-regs-currently-loaded-file' matches this regular expression when emacs exits
then the currently loaded registers will be saved to `one-key-regs-currently-loaded-file'."
  :group 'one-key-regs
  :type 'regexp)

(defun one-key-regs-save-on-exit-hook nil
  "Save the currently loaded registers to `one-key-regs-currently-loaded-file' if this matches `one-key-regs-save-on-exit'."
  (if (and (stringp one-key-regs-save-on-exit)
           (stringp one-key-regs-currently-loaded-file)
           (file-exists-p one-key-regs-currently-loaded-file)
           (string-match one-key-regs-save-on-exit (file-name-nondirectory one-key-regs-currently-loaded-file)))
      (one-key-regs-save-registers one-key-regs-currently-loaded-file)))

(defcustom one-key-regs-max-description-length 55
  "The maximum number of chars allowed in the register description shown in the `one-key' menu."
  :group 'one-key-regs
  :type '(number))

(defcustom one-key-regs-default-directory "~/.emacs.d/"
  "The default directory in which to store one-key-regs sets.
Should end with a \"/\"."
  :group 'one-key-regs
  :type '(directory))

(defcustom one-key-regs-default-file (concat (file-name-as-directory one-key-regs-default-directory)
                                             "default_registers.el")
  "The default registers file to load on startup."
  :group 'one-key-regs
  :type '(file))

(defvar one-key-regs-currently-loaded-file nil
  "The currently loaded registers file, or nil if none have been loaded.
This variable is used by one-key-regs when saving changes to the current register list or their labels.")

(defcustom one-key-regs-merge-conflicts 'prompt
  "What method to use to handle conflicts when loading new registers.
If a new register uses the same char as a currently loaded register this variable is used to decide how to resolve
the conflict.
It can take one of the following values:

prompt           : prompt the user for how to merge each individual conflict
overwriteold     : overwrite old registers which conflict
discardnew       : discard new registers which conflict
changeold        : change char of old registers which conflict
changenew        : change char of new registers which conflict
replace          : completely replace old register set with new one
"
  :group 'one-key-regs
  :type '(choice (const :tag "Prompt" prompt)
                 (const :tag "Overwrite old registers which conflict" overwriteold)
                 (const :tag "Discard new registers which conflict" discardnew)
                 (const :tag "Change char of old registers which conflict" changeold)
                 (const :tag "Change char of new registers which conflict" changenew)
                 (const :tag "Completely replace old register set with new one" replace)))

(defcustom one-key-regs-quick-keys (number-sequence 33 126)
  "List of keys to use with `one-key-regs-define-keybindings'.
These keys will be used with the prefix key supplied to that function to define
shortcut keys to `one-key-regs-function' which copies/inserts registers."
  :group 'one-key-regs
  :type '(repeat character))

(defcustom one-key-regs-key-queues '((?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?0))
  "List of register key queues. These will be enabled if `one-key-regs-enable-key-queues' is non-nil.
Each queue is a list of keys. When one of the registers corresponding to a key in the queue is overwritten,
then the old register and any that come after it will be pushed forward to the next position in the queue.
If a register is pushed forward into an empty space then no further registers will be pushed forward.
If a register is at the end of the queue and is pushed forward then it is permanently removed."
  :type '(repeat (repeat character))
  :group 'one-key-regs)

(defcustom one-key-regs-enable-key-queues nil
  "Whether register key queues should be enabled or not.
If non-nil then the lists of keys in `one-key-regs-key-queues' will be used for creating key queues.
A key queue is a sequence of keys such that when a new register is placed in one of the keys, then if there was already
a register in that key it will be shifted down to the next key in the sequence, if that key also contained a register
it will also be shifted down to the next key, etc."
  :type 'boolean
  :group 'one-key-regs)

;; This function is copied from string-fns.el 
(defun one-key-regs-string-split (string &optional separator limit)
  "Split STRING at occurences of SEPARATOR.  Return a list of substrings.
Optional argument SEPARATOR can be any regexp, but anything matching the
 separator will never appear in any of the returned substrings.
 If not specified, SEPARATOR defaults to \"[ \\f\\t\\n\\r\\v]+\".
If optional arg LIMIT is specified, split into no more than that many
 fields \(though it may split into fewer\)."
  (or separator (setq separator "[ \f\t\n\r\v]+"))
  (let ((string-list nil)
        (len (length string))
        (pos 0)
        (splits 0)
        str)
    (save-match-data
      (while (<= pos len)
        (setq splits (1+ splits))
        (cond ((and limit
                    (>= splits limit))
               (setq str (substring string pos))
               (setq pos (1+ len)))
              ((string-match separator string pos)
               (setq str (substring string pos (match-beginning 0)))
               (setq pos (match-end 0)))
              (t
               (setq str (substring string pos))
               (setq pos (1+ len))))
        (setq string-list (cons str string-list))))
    (nreverse string-list)))

(defun one-key-regs-get-type (reg)
  "Return the name of the register type for register contents REG.
The type name is one of the symbols listed in the cars of the elements of `one-key-regs-reserved-register-types',
 and `one-key-regs-custom-register-types'."
  (or (loop for type in one-key-regs-reserved-register-types
            if (funcall (fourth type) reg) do (return (car type)))
      (and (consp reg) (symbolp (car reg)) (car reg))))

(defun one-key-regs-chomp-string (str)
  "Chomp leading and tailing non-word chars from STR."
  (while (string-match "\\`\n+\\|^\\Sw+\\|\\Sw+$\\|\n+\\'" str)
    (setq str (replace-match "" t t str))) str)

(defun one-key-regs-get-marker-description (buf pos)
  "Get description of position POS in buffer BUF.
Returns the name of the function at position POS or if it can't find that then the line number."
  (with-current-buffer buf
    (goto-char pos)
    (let ((fname (if (functionp 'which-function)
                     (let* ((wfunc (which-function))
                            (fname1 (if (listp wfunc) (car wfunc) wfunc)))
                       (if (equal fname1 "^\\s-*$") nil (one-key-regs-chomp-string fname1))))))
      (if fname (format "%s in %s" fname (buffer-name buf))
        (format "Line %s in %s" (line-number-at-pos) (buffer-name buf))))))

(defun one-key-regs-get-default-description (reg)
  "Return the default description for register contents REG.
If `one-key-regs-colourize-menu' is non-nil add the appropriate background colour to the description string
according to the associations in `one-key-regs-colours-alist'."
  (let* ((typename (one-key-regs-get-type reg))
         (type (or (assq typename one-key-regs-reserved-register-types)
                   (assq typename one-key-regs-custom-register-types)))
         (str1 (if type (or (and (third type) (condition-case nil
                                                  (funcall (third type) reg)
                                                (error (format "Invalid format for %S" typename))))
                            (symbol-name typename))
                 "Unknown type"))
         (str2 (substring-no-properties str1 0 (min one-key-regs-max-description-length (length str1))))
         (colour (cdr (assq typename one-key-regs-colours-alist))))
    (if (and one-key-regs-colourize-menu colour)
        (propertize str2 'face (list :background colour :foreground one-key-item-foreground-colour))
      str2)))

(defun one-key-regs-add-menu-item (reg &optional desc rest)
  "Add or alter the menu item in `one-key-menu-one-key-registers-alist' corresponding to the register REG.
REG should be a cons cell in the form (CHAR . CONTENTS), containing the register char and contents.
Set the description of the menu item to DESC or to the default description if DESC is nil.
If REST is non-nil set the cdr of the menu item to REST, otherwise set it to a command which calls
`one-key-regs-execute-register' with argument CHAR."
  (let* ((desc (or desc (one-key-regs-get-default-description (cdr reg))))
         (func `(lambda nil (interactive) (one-key-regs-execute-register ,(car reg)))))
    (setq one-key-menu-one-key-registers-alist
          (one-key-add-menu-item (car reg) desc (or rest func) one-key-menu-one-key-registers-alist))))

(defun one-key-regs-highlight-matching-items (info-alist full-list colour pred)
  "Highlight items in FULL-LIST with colour COLOUR using predicate function PRED to select items.
The predicate function should take a single item from FULL-LIST as it's only argument.
INFO-ALIST and FULL-LIST are as in the `one-key-menu' function.
If COLOUR is \"\" then all highlighting (and more generally any text properties) are removed from the item."
  (loop for item in-ref full-list
        for str = (cdar item)
        if (funcall pred item) do
        (if (equal colour "")
            (setf (cdar item) (substring-no-properties str))
          (setf (cdar item)
                (propertize str 'face (list :background colour :foreground one-key-item-foreground-colour)))))
  (setq one-key-menu-call-first-time t)
  (one-key-set-window-state 'close))

(defun one-key-regs-shift-key-queue (char)
  "If CHAR is a member of a key queue in `one-key-regs-key-queues' then shift registers in keys after it in the queue.
A register will only be shifted if it is non-empty, and either comes directly after CHAR or another register needs to be
shifted into it's key. This is a recursive function."
  (let ((keyqueue (find-if (lambda (queue) (memq char queue)) one-key-regs-key-queues)))
    (if keyqueue
        (let* ((pos (position char keyqueue))
               (nextchar (nth (1+ pos) keyqueue))
               (thisreg (assoc char register-alist)))
          (if (and nextchar thisreg)
              (let* ((thisitem (copy-list (one-key-get-menu-item (car thisreg)
                                                                 one-key-menu-one-key-registers-alist)))
                     (nextreg (copy-list (assoc nextchar register-alist))))
                (if nextreg (one-key-regs-shift-key-queue nextchar))
                (set-register nextchar (cdr thisreg))
                (one-key-regs-add-menu-item nextreg (cdar thisitem))))))))

(defun one-key-regs-function (char prefixarg &optional regtype)
  "Create a new register and place in char CHAR, or execute existing register stored in CHAR.
When called interactively the last input event (i.e. last key pressed) will determine the value of CHAR.
If region is active and no prefix is used, create a register of type `one-key-regs-default-region-register-type'.
If region is not active an no prefix arg is used then execute the register in CHAR or create a new one of type
`one-key-regs-default-register-type' if such a register already exists.
If a numeric prefix arg is used then create a register of the appropriate type according to `one-key-regs-prefix-key-associations', and use the default label for the one-key-regs menu.
If a non-numeric prefix arg is used then prompt the user for the type of register to create and associated label.

When called non-interactively, follow the same rules as above using PREFIXARG for the prefixarg, unless the REGTYPE
argument is also specified in which case create a register of that type (REGTYPE should be a symbol).

Finally, if `one-key-regs-enable-key-queues' is non-nil then if the key corresponding to a newly created register
is a member of a key queue (as defined in `one-key-regs-key-queues'), then the old register value and any other registers
further down the queue will be pushed forward to accomodate the new one."
  (interactive (list (if (memq 'shift (event-modifiers last-input-event))
                         (event-convert-list (list 'shift (event-basic-type last-input-event)))
                       (event-basic-type last-input-event))
                     current-prefix-arg))
  ;; check if we are creating a new register or executing an existing one
  (if (or prefixarg regtype (use-region-p) (not (assq char register-alist)))
      ;; get the correct register type 
      (let* ((knownprefix (memq prefixarg (mapcar 'car one-key-regs-prefix-key-associations)))
             (unknownprefix (and prefixarg (not knownprefix)))
             (regtype1 (cond (regtype regtype)
                             ((and (use-region-p) (not prefixarg)) one-key-regs-default-region-register-type)
                             ((not prefixarg) one-key-regs-default-register-type)
                             (knownprefix (cdr (assq prefixarg one-key-regs-prefix-key-associations)))
                             (unknownprefix (let* ((custtypes (mapcar (lambda (x) (symbol-name (car x)))
                                                                      one-key-regs-custom-register-types))
                                                   (restypes (mapcar (lambda (x) (symbol-name (car x)))
                                                                     one-key-regs-reserved-register-types))
                                                   (alltypes (append restypes custtypes)))
                                              (intern (if (featurep 'ido)
                                                          (ido-completing-read "Register type: " alltypes nil t)
                                                        (completing-read "Register type: " alltypes nil t)))))))
             (restype (assq regtype1 one-key-regs-reserved-register-types))
             (custype (assq regtype1 one-key-regs-custom-register-types)))
        ;; shift the appropriate key queue if necessary
        (if one-key-regs-enable-key-queues (one-key-regs-shift-key-queue char))
        ;; create the register
        (if restype (funcall (second restype) char)
          (if custype (set-register char (cons regtype1 (eval (second custype))))
            (error "Invalid register type: %S" regtype1)))
        ;; get the description for the register and add it to `one-key-menu-one-key-registers-alist'
        (let* ((reg (assq char register-alist))
               (defaultdesc (one-key-regs-get-default-description (cdr reg)))
               (thisface (get-text-property 0 'face defaultdesc))
               ;; prompt for description if necessary, using defaultdesc as default value
               (desc (if (and (or unknownprefix one-key-regs-prompt-for-description)
                               (not (eq regtype1 'delete-register)))
                          (let* ((str (read-string (format "Short description of register (default \"%s\"): " defaultdesc)
                                                   nil nil defaultdesc))
                                 (minlen (min (length str) one-key-regs-max-description-length)))
                            (substring (mapconcat 'identity (split-string str "\n") "\\n") 0 minlen))
                        defaultdesc))
               (desc2 (propertize (substring-no-properties desc) 'face thisface)))
          (one-key-regs-add-menu-item reg desc2)))
    (one-key-regs-execute-register char)))

(defun one-key-regs-execute-register (char)
  "Execute the register stored in char CHAR.
If the register stored in char CHAR is a reserved type, then execute the associated function in
`one-key-regs-reserved-register-types', otherwise if the register is a custom type, execute the associated
elisp form in `one-key-regs-custom-register-types'." 
  (interactive "cJump to register: ")
  (let* ((val (get-register char)))
    (if (not (loop for res in one-key-regs-reserved-register-types
                   ;; if it's a reserved register type, call the special function to execute it and return t
                   if (funcall (fourth res) val) do (funcall (fifth res) val) and do (return t)))
        ;; if it's a custom register type we just eval the cdr to execute it
        (if (assq (car val) one-key-regs-custom-register-types)
            (eval (cdr val))
          (error "Invalid register: %S" val)))))

(defun one-key-regs-define-keybindings (modifiers)
  "Bind keys events with modifiers in MODIFIERS to `one-key-regs-function'.
All key events formed by combining MODIFIERS with the chars stored in `one-key-regs-quick-keys' will be bound."
  (dolist (char one-key-regs-quick-keys)
    (let* ((eventlist (append modifiers (list char))))
      (global-set-key (vector (event-convert-list eventlist)) 'one-key-regs-function))))

(defvar one-key-menu-one-key-registers-alist nil
  "The `one-key' menu alist for `one-key-menu-regs'.")

(defun one-key-regs-update-menu-alist nil
  "Check that `one-key-menu-one-key-registers-alist' is consistent with the current value of `register-alist'.
If it is not then update it."
  ;; First add menu items for registers not already in the list
  (loop for reg in register-alist
        for item = (one-key-get-menu-item (car reg) one-key-menu-one-key-registers-alist)
        if (not item) do (one-key-regs-add-menu-item reg))
  ;; next remove items that don't correspond to registers, unless they correspond to non-register keys
  (setq one-key-menu-one-key-registers-alist
        (remove-if-not (lambda (item)
                         (let ((keystr (read-kbd-macro (caar item))))
                           (and (stringp keystr)
                                (assq (string-to-char keystr) register-alist))))
                           one-key-menu-one-key-registers-alist)))

(defun one-key-regs-show-prefix-key-associations nil
  "Show current default registers and prefix key associations.
In other words show the values of variables `one-key-regs-default-register-type',
`one-key-regs-default-region-register-type', and `one-key-regs-prefix-key-associations'."
  (interactive)
  (let* ((lines (nreverse (loop for (arg . name) in one-key-regs-prefix-key-associations
                                collect (format "Numeric prefix %d\t\t: %S" arg name))))
         (str (mapconcat 'identity lines "\n")))
    (message (format "Default register type\t\t: %S
Default region register type\t: %S
%s" one-key-regs-default-register-type one-key-regs-default-region-register-type str))))

(defun one-key-regs-open-registers-file nil
  "Open the file associated with the current register set, or prompt for a file is non is associated.
The file associated with the current register set is `one-key-regs-currently-loaded-file'."
  (interactive)
  (let ((file (or one-key-regs-currently-loaded-file
                  (if (featurep 'ido)
                      (ido-read-file-name "Open registers file: "
                                          (file-name-as-directory one-key-regs-default-directory) nil t)
                    (read-file-name "Open registers file: "
                                    (file-name-as-directory one-key-regs-default-directory) nil t)))))
    (find-file file)
    file))

(defun one-key-regs-save-registers (&optional filename queryp)
  "Save the contents of all currently loaded registers and associated menu items in file FILENAME.
The items will be saved as loadable elisp sexp's that will recreate the registers and menu when loaded.
Cannot save window/frame configurations, but it works with markers, text, rectangles, numbers,
and any custom register types defined in `one-key-regs-custom-register-types'.
This function will also save any items listed in `one-key-regs-save-items'.

When called interactively set FILENAME to `one-key-regs-currently-loaded-file' unless this is nil or QUERYP is non-nil
in which case query the user for the filename.
When called non-interactively if either FILENAME is nil or QUERYP is non-nil then query the user for the FILENAME.
In both cases when querying for a filename, use FILENAME as default if non-nil (i.e. use `one-key-regs-currently-loaded-file' in the interactive case).
If there are no errors then `one-key-regs-currently-loaded-file' will be set to the value of FILENAME when the function
has finished saving to it.

Note: for custom register types the sexp stored in the register contents will be saved. This may or may not have
the intended effect when loaded and executed in a new emacs session (bear this in mind when creating custom types)."
  (interactive (list one-key-regs-currently-loaded-file current-prefix-arg))
  (when (or queryp (not filename))
    (setq filename (read-file-name (if filename
                                       (format "Save to file (%s): " filename)
                                     "Save to file: ")
                                   one-key-regs-default-directory
                                   one-key-regs-currently-loaded-file)))
  (let ((old-print-level print-level)
        (old-print-length print-length)
        (print-level nil)
        (print-length nil)
        menu-alist)                     ; let us write anything
    (with-temp-file filename
      ;; first save forms that will load the registers,
      ;; and rebuild menu-alist removing any unwanted text properties (just to be safe)
      (loop for (char . contents) in register-alist
            for type = (one-key-regs-get-type contents)
            for resfunc = (nth 5 (assq type one-key-regs-reserved-register-types))
            for custype = (assq type one-key-regs-custom-register-types)
            for colour = (cdr (assq type one-key-regs-colours-alist))
            for key = (single-key-description char)
            for desc = (or (cdar (one-key-get-menu-item char one-key-menu-one-key-registers-alist))
                           "No description!")
            for desc2 = (if colour (propertize (substring-no-properties desc)
                                               'face (list :background colour
                                                           :foreground one-key-item-foreground-colour))
                          (substring-no-properties desc))
            for func = `(lambda nil (interactive) (one-key-regs-execute-register ,char))
            do (add-to-list 'menu-alist (cons (cons key desc2) func) t)
            (if resfunc (insert (format "%S\n" (funcall resfunc char contents)))
              (if custype (insert (format "(set-register %d (quote %S))\n" char contents)))))
      ;; now save a form to load `one-key-menu-one-key-registers-alist'
      (insert (format "\n(let ((menu-items-alist '%S))
  (if (not one-key-menu-one-key-registers-alist)
      (setq one-key-menu-one-key-registers-alist menu-items-alist)
    (loop for item in menu-items-alist
          for key = (caar item)
          for olditem = (find-if (lambda (x) (equal key (caar x))) one-key-menu-one-key-registers-alist)
          if olditem do (setf (cdar olditem) (cdar item) (cdr olditem) (cdr item))
          else do (add-to-list 'one-key-menu-one-key-registers-alist item))))\n\n" menu-alist))
      ;; finally save forms to load other items in `one-key-regs-save-items'
      (loop for var in one-key-regs-save-items
            do (insert (format "(setq %S '%S)\n" var (eval var)))))
    (setq one-key-regs-currently-loaded-file filename)
    (setq print-level old-print-level print-length old-print-length))
  (if (> (length one-key-regs-save-items) 0)
      (message "Registers, menu items, %s saved to %S"
               (mapconcat (lambda (x) (substitute 32 45 (substring (symbol-name x) 13)))
                          one-key-regs-save-items ", ")
               filename)
    (message "Registers and menu items saved to %S" filename)))

(defun one-key-regs-change-key (reg menuitem &optional prompt key)
  "Change the key associated with register REG and `one-key' menu item MENUITEM.
The key will be changed to an unassigned key, or KEY if supplied and not already used.
If PROMPT is non-nil prompt the user for an unused key.
REG should be an element of a `register-alist' and MENUITEM an element of `one-key-menu-one-key-registers-alist'."
  (let ((unused-keys (set-difference one-key-regs-quick-keys (mapcar 'car register-alist))))
    (while (not (memq key unused-keys))
      (setq key (if prompt (string-to-char (read-key-sequence (format "Enter new key for \"%s\"
Available keys: %s" (cdr label) (mapcar 'char-to-string unused-keys))))
                  (car unused-keys)))))
  (setcar reg key)
  (setf (caar menuitem) (single-key-description key)))

(defun one-key-regs-merge-registers (file mergemethod)
  "Load registers stored in a one-key-regs file.
If there are any conflicts between the chars used in the new registers with those used in the already loaded
registers the conflicts will be resolved according to the value of MERGEMETHOD which is set equal
to `one-key-regs-merge-conflicts' if called interactively, unless a prefix arg is supplied, 
 (see the documentation for `one-key-regs-merge-conflicts' to see the possible values of MERGEMETHOD and their meanings).
If a prefix arg is supplied and `one-key-regs-merge-conflicts' is set to 'prompt then MERGEMETHOD will be
set to 'replace, otherwise it will be set to 'prompt."
  (interactive
   (list (one-key-regs-prompt-for-file)
         (if (not current-prefix-arg) one-key-regs-merge-conflicts
           (if (eq one-key-regs-merge-conflicts 'prompt) 'replace 'prompt))))
  ;; Before merging we backup the old registers and menu items to `old-register-alist' and `old-one-key-menu-one-key-registers-alist'
  ;; and then copy the new ones into `register-alist' and `one-key-menu-one-key-registers-alist'.
  (let ((old-register-alist (copy-alist register-alist))
        (old-one-key-menu-one-key-registers-alist (copy-alist one-key-menu-one-key-registers-alist)))
    (setq register-alist nil one-key-menu-one-key-registers-alist nil)
    (load-file file)
    (if (eq mergemethod 'replace) (setq one-key-regs-currently-loaded-file file))
    ;; Here follows some macros to make the code shorter and more readable.
    (macrolet (;; for keepold we alter the new register and menu item to match the old one
               (keepold nil '(progn (setcdr newreg (cdr oldreg))
                                    (one-key-regs-add-menu-item oldreg (cdar oldmenuitem) (cdr oldmenuitem))))
               ;; for addold we add the old register and menu items
               (addold nil '(progn (add-to-list 'register-alist oldreg)
                                   (one-key-regs-add-menu-item oldreg (cdar oldmenuitem) (cdr oldmenuitem))))
               ;; for addnew we just add the new menu item since the register is already added
               (addnew nil '(one-key-regs-add-menu-item newreg (cdar newmenuitem) (cdr newmenuitem)))
               ;; changekey changes the key of register r and menu item i, prompting the user if p is non-nil
               (changekey (r i p) `(one-key-regs-change-key ,r ,i ,p)))
      (loop named regloop for oldreg in old-register-alist
            for char = (car oldreg)
            for oldmenuitem = (one-key-get-menu-item char old-one-key-menu-one-key-registers-alist)
            for newreg = (assq char register-alist)
            for newmenuitem = (one-key-get-menu-item char one-key-menu-one-key-registers-alist)
            if (and newreg (not (equal newreg oldreg))) ; only need to merge if newreg doesn't match oldreg
            do (case mergemethod
                 (prompt (while (not (case (string-to-char
                                            ;; prompt the user till they enter a valid key or press "C-g" (i.e. event 7)
                                            (read-key-sequence (format "\"%c\" key is already in use! Press one of the following keys:
1 - keep old register: \"%s\"
2 - use new register: \"%s\"
3 - change key of new register
4 - change key of old register
5 - overwrite all further conflicting old registers
6 - discard all further conflicting new registers
7 - change key of all conflicting new registers (to next unassigned key)
8 - change key of all conflicting old registers (to next unassigned key)" char (cdar oldmenuitem) (cdar newmenuitem))))
                                       (49 (keepold) t)
                                       (50 t)
                                       (51 (changekey newreg newmenuitem t) (addnew) (addold) t)
                                       (52 (changekey oldreg oldmenuitem t) (addnew) (addold) t)
                                       (53 (setq mergemethod 'overwriteold) t)
                                       (54 (keepold) (setq mergemethod 'discardnew) t)
                                       (55 (changekey newreg newmenuitem nil) (addnew) (addold)
                                           (setq mergemethod 'changenew) t)
                                       (56 (changekey oldreg oldmenuitem nil) (addnew) (addold)
                                           (setq mergemethod 'changeold) t)
                                       (7 (setq register-alist old-register-alist)
                                          (setq one-key-menu-one-key-registers-alist old-one-key-menu-one-key-registers-alist)
                                          (return-from regloop))))))
                 (overwriteold t)
                 (discardnew (keepold))
                 (changeold (changekey oldreg oldmenuitem nil) (addnew) (addold))
                 (changenew (changekey newreg newmenuitem nil) (addnew) (addold)))
            ;; if newreg and oldreg are different add oldreg unless mergemethod is replace
            else if (not (eq mergemethod 'replace)) do (addold))))
  ;; short hack to clear echo area (need both lines - not sure why)
  (message " ")
  (message nil))

(defun one-key-regs-open-register-sets-menu (mergetype)
  "Returns a menu-alist of register set items.
The items correspond to the register set files stored in `one-key-regs-default-directory', and pressing the key for an
item will load the corresponding register set."
  (let* ((pair (one-key-get-menus-for-type (concat "register sets (merge=" mergetype ")"))))
    (one-key-open-submenu (car pair) (cdr pair))))

(defun one-key-regs-prompt-for-file nil
  "Prompt the user for a file.
The default file is `one-key-regs-currently-loaded-file' if that is non-nil."
  (if (featurep 'ido)
      (ido-read-file-name "one-key-regs file: "
                          (and one-key-regs-currently-loaded-file
                               (file-name-directory one-key-regs-currently-loaded-file))
                          defaultfile t
                          (and one-key-regs-currently-loaded-file
                               (file-name-nondirectory one-key-regs-currently-loaded-file)))
    (read-file-name "one-key-regs file: "
                    (and one-key-regs-currently-loaded-file
                         (file-name-directory one-key-regs-currently-loaded-file))
                    defaultfile t
                    (and one-key-regs-currently-loaded-file
                         (file-name-nondirectory one-key-regs-currently-loaded-file)))))


(defun one-key-regs-clear-registers (&optional noprompt)
  "Delete all registers from memory.
Unless NOPROMPT is non-nil the user will be prompted to check if they want to continue."
  (interactive "P")
  (if (or noprompt (y-or-n-p "Are you sure you want to clear all registers? "))
      (progn (setq register-alist nil one-key-regs-currently-loaded-file nil)
             (one-key-regs-update-menu-alist))))

(defun one-key-regs-prompt-to-add-menu-item (info-alist full-list)
  "Prompt the user for item details and add it to FULL-LIST, then update INFO-ALIST and redisplay the `one-key' menu."
  (let* ((isref (symbolp info-alist))
         (key (read-event "Enter the key for the new item")))
    (one-key-regs-function key '(4))
    (one-key-regs-update-menu-alist)
    (setq one-key-menu-call-first-time t)
    (one-key-set-window-state 'close)))

(defun one-key-regs-swap-menu-items (full-list)
  "Prompt user for a pair of items from FULL-LIST and swap the corresponding keys."
  (let* ((keya (read-event "Press key for first item"))
         (keyastr (one-key-key-description keya))
         (itema (one-key-get-menu-item keyastr full-list))
         (keyb (read-event "Press key for second item"))
         (keybstr (one-key-key-description keyb))
         (itemb (one-key-get-menu-item keybstr full-list)))
    (if (not (and itema itemb)) (message "Invalid key!")
      (setf (caar itema) keybstr (caar itemb) keyastr)
      (let ((rega (assq keya register-alist))
            (regb (assq keyb register-alist)))
        (setf (car rega) keyb (car regb) keya)))
    (setq one-key-menu-call-first-time t)
    (one-key-set-window-state 'close)))

(defun one-key-regs-edit-menu-item (info-alist full-list)
  "Prompt user for details of item in FULL-LIST to edit, make changes and then reopen `one-key' menu."
  (let* ((oldkey (read-event "Press the key of the item you want to edit"))
         (item (one-key-get-menu-item oldkey full-list))
         (newkey (let ((key (read-event "Enter new key for the item")))
                   (while (and (one-key-get-menu-item key full-list)
                               (not (eq key oldkey))
                               (not (y-or-n-p "That key is already used! Use it anyway?")))
                     (setq key (read-event "Enter new key for the item")))
                   key))
         (desc (read-string "Item description: " (cdar item) nil nil))
         (oldcontents (get-register oldkey))
         (contents (read-from-minibuffer "Register contents: " (format "%S" oldcontents) nil t))
         (isref (symbolp info-alist))
         (reg (cons newkey contents))
         (type (one-key-regs-get-type contents))
         (colour (cdr (assq type one-key-regs-colours-alist)))
         (desc2 (if (and one-key-regs-colourize-menu colour)
                    (propertize desc 'face
                                (list :background colour :foreground one-key-item-foreground-colour))
                  desc)))
    (setf (caar item) (single-key-description newkey))
    (setf (cdar item) desc)
    (setf (cdr item) `(lambda nil (interactive) (one-key-regs-execute-register ,newkey)))
    (setq register-alist (assq-delete-all oldkey register-alist))
    (set-register newkey contents))
    (setq one-key-menu-call-first-time t)
    (one-key-set-window-state 'close))

(defun one-key-regs-delete-menu-item (info-alist full-list)
  "Prompt the user for an item to delete from FULL-LIST, delete it, and then redisplay the `one-key' menu."
  (let* ((isref (symbolp info-alist))
         (key (read-event "Press the key of the item you want to delete"))
         (item (one-key-get-menu-item key full-list)))
    (if (and item (y-or-n-p (format "Delete item \"%s\"?" (cdar item))))
        (if isref (set info-alist (delete item full-list))
          (setq info-alist (delete item full-list))))
    (setq register-alist (assq-delete-all key register-alist))
    (setq one-key-menu-call-first-time t)
    (one-key-set-window-state 'close)))

;; Set the value of `one-key-regs-legend-string' to match the current value of `one-key-regs-colours-alist'
(setq one-key-regs-legend-string
      (loop for (sym . colour) in one-key-regs-colours-alist 
            for (h s v) = (hexrgb-hex-to-hsv colour)
            for newcolour = (hexrgb-hsv-to-hex h s 0.5) 
            for name = (symbol-name sym)
            concat (propertize name 'face
                               (list :background (if one-key-auto-brighten-used-keys newcolour colour)
                                     :foreground one-key-item-foreground-colour))))

;; Set the menu-alist, title string format and special keybindings for `register sets' menus
(one-key-add-to-alist 'one-key-types-of-menu
                      (list "register sets"
                            (lambda (name) (string-match "register sets" name))
                            (lambda (name)
                              (let* ((choices '("prompt" "overwriteold" "discardnew" "changeold" "changenew" "replace"))
                                     (mergename (if (string-match "(merge=\\(.*\\))" name)
                                                    (match-string 1 name)
                                                  (if (featurep 'ido)
                                                      (ido-completing-read "Merge method: " choices)
                                                    (completing-read "Merge method: " choices))))
                                     (mergetype (intern mergename))
                                     (merge-func
                                      `(lambda (file)
                                         (interactive)
                                         (one-key-regs-merge-registers file ',mergetype)
                                         (setq one-key-regs-currently-loaded-file file)
                                         (let ((old-val one-key-submenus-replace-parents)
                                               (one-key-submenus-replace-parents t))
                                           (one-key-open-submenu "registers" 'one-key-menu-one-key-registers-alist)
                                           (setq one-key-submenus-replace-parents old-val))))
                                     (menu-alists (one-key-dir-build-menu-alist
                                                   one-key-regs-default-directory
                                                   :filefunc merge-func
                                                   :topdir one-key-regs-default-directory))
                                     (name (concat "register sets (merge=" mergename ")"))
                                     (names (one-key-append-numbers-to-menu-name name (length menu-alists))))
                                (cons names menu-alists)))
                            (lambda nil
                              (format "Currently loaded registers file: %s\n"
                                      (if one-key-regs-currently-loaded-file
                                          (file-name-nondirectory one-key-regs-currently-loaded-file)
                                        "Unsaved registers")))
                            'one-key-regs-special-keybindings) t)

;; Set the menu-alist, title string format and special keybindings for `one-key-regs' menus
(one-key-add-to-alist 'one-key-types-of-menu
                      (list "registers"
                            (lambda (name) (string-match "^registers:?" name))
                            (lambda (name) (if (equal name "registers")
                                               (cons "registers" 'one-key-menu-one-key-registers-alist)
                                             (let* ((filename (and (string-match "^registers:\\(.*\\)" name)
                                                                   (match-string 1 name)))
                                                    (filedir (file-name-directory filename))
                                                    (filepath (if filedir filename
                                                                (concat one-key-regs-default-directory filename))))
                                               (if (and (file-readable-p filepath)
                                                        (not (file-directory-p filepath)))
                                                   (progn (one-key-regs-merge-registers filepath 'replace)
                                                          (cons name 'one-key-menu-one-key-registers-alist))))))
                            (lambda nil
                              (if one-key-regs-show-legend
                                  (format "Currently loaded registers file: %s\n%s\n"
                                          (if one-key-regs-currently-loaded-file
                                              (file-name-nondirectory one-key-regs-currently-loaded-file)
                                            "Unsaved registers")
                                          one-key-regs-legend-string)
                                (format "Currently loaded registers file: %s\n"
                                          (if one-key-regs-currently-loaded-file
                                              (file-name-nondirectory one-key-regs-currently-loaded-file)
                                            "Unsaved registers"))))
                              'one-key-regs-special-keybindings) t)

(add-to-list 'one-key-exclude-from-save "^registers")
;; Load the default register set
(if (file-readable-p one-key-regs-default-file)
    (progn (load one-key-regs-default-file)
           (setq one-key-regs-currently-loaded-file one-key-regs-default-file)))

;; Make sure registers are saved on exit
(add-hook 'kill-emacs-hook 'one-key-regs-save-on-exit-hook)

(provide 'one-key-regs)

;;; one-key-regs.el ends here

;; (magit-push)
;; (yaoddmuse-post "EmacsWiki" "one-key-regs.el" (buffer-name) (buffer-string) "update")
