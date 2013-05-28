;;; one-key.el --- Easy access configurable popup menus to display keybindings and other things.

;; Filename: one-key.el
;; Description: One key
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;;         rubikitch <rubikitch@ruby-lang.org>
;; Maintainer: Joe Bloggs <vapniks@yahoo.com>
;; Copyleft (Ↄ) 2012, Joe Bloggs, all rites reversed.
;; Copyright (C) 2008, 2009, 2010 Andy Stewart, all rights reserved.
;; Copyright (C) 2009, rubikitch, all rights reserved.
;; Created: 2008-12-22 21:54:30
;; Version: 1.1
;; Last-Updated: 7/12/2012 01:00:00
;;           By: Joe Bloggs
;; URL: http://www.emacswiki.org/emacs/download/one-key.el
;; Keywords: one-key
;; Compatibility: GNU Emacs 22 ~ 23
;;
;; Features that might be required by this library:
;;
;; `cl' `hexrgb' `reporter' `browse-url'
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
;; along with this program.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Bitcoin donations gratefully accepted: 1HnSqGHrVenb1t2V2aijyocWyZcd7qt1k

;; With so many Emacs extensions, you have a lot of keystrokes to remember, and you probably forget most of them.
;;
;; This package fixes that problem, and helps new users to learn the common keybindings.
;;
;; One Key provides a single keystroke that when pressed presents you with a menu of choices in a popup window
;; for commands to execute with a further keystroke. By default menus for common prefix keys and commands are defined.
;;
;; Just type one of the listed keystrokes to execute the corresponding command.
;;
;; You can delete, edit, sort, highlight and filter the menu items, add new menu items and even add new menus.
;; You can have access to several different menus from the same window which can be navigated with the arrow keys.
;; Such a collection of menus is called a menu set and you can define several different menu sets containing different
;; types of menus.
;; Several different types of menus are defined (and more may be added) for holding different types of menu items.
;; For example the "major-mode" type opens the menu corresponding to the current major-mode.
;; More different types are defined by one-key extension libraries (e.g. `one-key-dir' for fast directory
;; tree navigation), or you can create your own types. See "Creating menus" below.

;;; The *One-Key* buffer:
;;
;; Running the command `one-key-open-associated-menu-set' or `one-key-open-menu-set' opens the *One-Key* buffer.
;; (these commands may be bound to keys - see "Installation" below).
;;
;; Within the *One-Key* buffer you will see a list of command descriptions each with a corresponding key in square
;; brackets to its left. Pressing the key executes the command.
;; Along the top of the buffer in the header line you will see a list of menu names. One of these names will be
;; highlighted and indicates the current menu. You can navigate between the different menus by pressing the left/right
;; arrow keys (unless these have been redefined to other keys in `one-key-special-keybindings', see below).
;;
;; You can toggle the size of the window holding the *One-Key* buffer by pressing the appropriate special key (see below).
;; The window size is toggled between default size, large size (so that all items fit in the window), and hidden (window
;; is closed, but one-key is still active). This is useful if there are a large number of items in the menu, or when you
;; need to see part of the buffer that is obscured by the *One-Key* window.
;;
;; By default one-key will quit and the *One-Key* window will close after pressing a key corresponding to one of the
;; menu items. If you want one-key to stay active after pressing an item key you should toggle the menu persistence
;; by pressing the appropriate special key (C-menu by default, see "Special keybindings" below).
;; To quit one-key and close the *One-Key* window press ESC. If you want to quit but keep the window open (e.g. to see
;; the keybindings for a major mode), press C-ESC.

;;; Special keybindings:
;;
;; For each different type of menu certain "special" keybindings are defined which activate menu specific commands,
;; such as sorting or editing the menu items, adding new menus, etc.
;; These special keybindings are specific to each menu type, though many of them will be the same for all menu types.
;; For example the arrow keys are defined as special keybindings for navigating around menus.
;;
;; Pressing the f1 key displays a help message listing all the special keybindings for the current menu.
;;
;; By default, when a menu is created one-key will ensure that the keys corresponding to menu items do not clash with
;; the special keybindings for that menu type. However, if for some reason there is a clash then the menu item gets
;; priority over the special keybinding unless the help window is displayed (by pressing f1), in which case the special
;; keybinding gets priority.
;;
;; You can alter or add new special keybindings by customizing `one-key-special-keybindings',
;; and `one-key-default-special-keybindings'.
;; Extension libraries (such as `one-key-dir' or `one-key-regs') may also define customizable special keys specific
;; to the menu type defined in the library.
;;
;; By default the following special keybindings are defined:
;;
;; ESC        : Quit and close menu window                       
;; <C-escape> : Quit, but keep menu window open                  
;; <C-menu>   : Toggle menu persistence                          
;; <menu>     : Toggle menu display                              
;; <left>     : Change to next menu                              
;; <right>    : Change to previous menu                          
;; <up>       : Scroll/move up one line                          
;; <down>     : Scroll/move down one line                        
;; <prior>    : Scroll menu down one page                        
;; <next>     : Scroll menu up one page                          
;; C-h        : Show help for next item chosen                   
;; C-s        : Save current state of menu                       
;; <f1>       : Toggle this help buffer                          
;; <f2>       : Toggle column/row ordering of items              
;; <f3>       : Sort items by next method
;; <C-f3>     : Sort items by previous method          
;; <f4>       : Reverse order of items                 
;; /          : Limit items to those matching regexp   
;; C-/        : Highlight items matching regexp        
;; <f5>       : Edit a menu item                       
;; <f6>       : Delete a menu item                     
;; <f7>       : Copy/kill coloured items               
;; <C-f7>     : Yank copied items                      
;; <f8>       : Swap menu item keys                    
;; <f9>       : Add a menu item                        
;; <C-f9>     : Add a menu                             
;; <C-S-f9>   : Remove this menu                       
;; <f10>      : Reposition item (with arrow keys)      
;; <f11>      : Donate to support further development  
;; <C-f11>    : Report a bug                           

;;; Creating menus:
;; 
;; All of the menus are stored in the file `one-key-menus-save-file' (customizable), but you should never need to
;; edit this file. Instead you can create and edit menus from within the *One-Key* buffer.
;; If you press the special key corresponding to "Add a menu" you will be prompted for the type of menu to add.
;; By default the following menu types are defined, but more may be added with extension libraries, or by creating
;; them yourself (see `one-key-types-of-menu'):

;;; Default menu types:
;;
;; top-level         : contains items defined in `one-key-menu-toplevel-alist', which by default contains common prefix key
;;                     menus, and menus for common commands to help new users learn emacs
;; blank menu        : creates a blank menu with no items
;; major-mode        : contains items corresponding to the current major mode (keybindings and menu-bar items)
;; existing menu     : prompts for an existing menu to use
;; existing keymap   : contains items in a given keymap (prompted for)
;; prefix key keymap : contains items whose usual keybindings begin with a given prefix key (prompted for)
;; menu-sets         : contains items for opening menu sets (see below)

;;; Saving menus:

;; If `one-key-autosave-menus' is non-nil then any new menus or menus that have been changed will be saved
;; on Emacs exit, unless they are listed in `one-key-exclude-from-save'
;; Alternatively you may save individual menus by pressing the special key for "Save the current state of menu"
;; (C-s by default).

;;; Menu sets:

;; A menu set is a collection of menu names. When you open the *One-Key* buffer with `one-key-open-associated-menu-set'
;; it opens a collection of menus associated with the current major-mode or buffer. By default this is the set of menus
;; in `one-key-default-menu-set'. You can define other menu sets by customizing `one-key-sets-of-menus-alist', and
;; associate them with different major-modes or buffers by customizing `one-key-associations-for-menu-sets'.
;; Each menu set consists of a name for the menu set, and a list of menu names.
;; one-key reconstructs a menu from its name by searching `one-key-types-of-menu' for a matching entry, and applying
;; the associated function to create the menu.
;; With the "menu-sets" menu you can see what menu sets are currently defined, switch menu sets, and save the current
;; menus as a menu set (C-s).
;; See "Creating menus" above for info on how to add the "menu-sets" menu to the *One-Key* buffer.

;;; Other features:
;;
;; Item help: to get help on a particular menu item press C-h followed by the key for the item. The help page
;; for the associated command will be displayed. If part of the *Help* buffer is obscured by the *One-Key* buffer,
;; you can hide the *One-Key* buffer by pressing the special key to toggle the display (<menu> by default).
;;
;; Item brightening: By default the background colour of a menu item is increased relative to the brightness
;; of other items each time the item is executed. This means you can quickly find the most frequently used items
;; in a large menu. To turn this feature off set `one-key-auto-brighten-used-keys' to nil.
;;
;; Sorting/ordering: you can sort the items in a menu by pressing the appropriate special key, and add new sort
;; methods by customizing `one-key-default-sort-method-alist'. The current sort method is displayed in the mode-line,
;; however this information is not guaranteed to be correct when the *One-Key* menu is initially opened.
;; You can also toggle between row/column ordering of items, and reverse the order of items.
;; By trying out different sort and ordering combinations you can find a configuration which is most readable,
;; or fits most items on screen. The order of menu items is persistent between sessions if the menus are saved
;; (see "Saving menus").
;;
;; Filtering and colouring items: you can filter the items displayed to match a regular expression, or specify
;; the background colour of items that match a given regular expression (press f1 to see which key to press).
;; This can make the menus more readable. The background colours will be saved with the menu.
;;
;; Editing menus: you can add, delete and edit menu items, and also copy or kill (i.e. cut) and yank (i.e. paste)
;; menu items from one menu to another. To copy or kill a bunch of items first make sure they are all highlighted
;; with the same background colour (doesn't matter if they have different brightness levels), and then press the
;; appropriate special key (press f1 for help). You will be prompted for an item in the group, and whether or not
;; you want to also kill (cut) the items from the current menu. The items will be saved in `one-key-copied-items' and
;; can be then be yanked (pasted) into another menu.
;; Any further copy/kills will overwrite the value of `one-key-copied-items', and you cannot retrieve previous kills
;; so take care.
;; You can also reposition items in a menu:
;;    1) press the appropriate specialkey
;;    2) press the key of the item to be moved
;;    3) use the up/down arrow keys to move the item
;;    4) exit one-key to fix the item
;;
;; Support further development: writing this code required a significant amount of unpaid labour on my part.
;; Please consider donating to help support further development by pressing f11 in the *One-Key* menu.
;; To report a bug press C-f11. Please report the circumstances in which the bug occured (where you creating a new
;; menu? what major-mode was in use at the time? etc.).


;;; Installation:
;;
;; Put one-key.el in a directory in your load-path, e.g. ~/.emacs.d/
;; You can add a directory to your load-path with the following line in ~/.emacs
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;; where ~/elisp is the directory you want to add
;; (you don't need to do this for ~/.emacs.d - it's added by default).
;;
;; Make sure that you also have hexrgb.el in your load-path.
;; At the time of writing it can be obtained from here: http://emacswiki.org/emacs/hexrgb.el

;; Add the following to your ~/.emacs startup file, replacing <menu> with whatever key you
;; want to use to open the *One-Key* buffer.
;;
;; (require 'one-key)
;; (global-set-key (kbd "<menu>") 'one-key-open-associated-menu-set)
;;
;; Because this library uses a special implementation,
;; sometimes a `max-lisp-eval-depth' or `max-specpdl-size' error can occur.
;; So making the above two variables larger will reduce the probability that an error occurs.
;; E.g:
;;
;; (setq max-lisp-eval-depth 10000)
;; (setq max-specpdl-size 10000)
;;

;;; Bug reporting
;;
;; To report a bug: M-x one-key-submit-bug-report, or press the appropriate special key 

;;; Customize:

;; `one-key-default-menu-keys' : A list of chars which may be used as the default keys in automatically generated 
;;                               `one-key' menus.
;; `one-key-min-keymap-submenu-size' : The minimum number of elements allowed in a submenu when creating menus from 
;;                                     keymaps.
;; `one-key-popup-window' : Whether to popup window when `one-key-menu' is run for the first time.
;; `one-key-buffer-name' : The buffer name of the popup menu window.
;; `one-key-column-major-order' : If true then menu items are displayed in column major order, otherwise row major order.
;; `one-key-min-number-of-columns' : An integer greater than 0 indicating the minimum number of columns to create for 
;;                                   one-key menus.
;; `one-key-menu-window-max-height' : The max height of popup menu window.
;; `one-key-menus-save-file' : The file where `one-key' menus are saved.
;; `one-key-autosave-menus' : If non-nil then one-key menus will automatically be saved when created or changed.
;; `one-key-exclude-from-save' : List of regular expressions matching names of menus which should not be autosaved.
;; `one-key-include-menubar-items' : Whether or not to include menu items with no keybinding when creating one-key menus 
;;                                   from keymaps.
;; `one-key-item-foreground-colour' : Foreground colour of highlighted items in `one-key' menus.
;; `one-key-auto-brighten-used-keys' : If non-nil then set brightness of menu items colours according to how often the 
;;                                     keys are pressed.
;; `one-key-submenus-replace-parents' : If non-nil then when a submenu of a `one-key' menu is opened it will replace the 
;;                                      parent menu.
;; `one-key-major-mode-remap-alist' : A list of cons cells mapping major modes to one-key-menus.
;; `one-key-menu-toplevel-alist' : The `one-key' top-level alist.
;; `one-key-sets-of-menus-alist' : Saved menu sets (sets of menus).
;; `one-key-default-menu-set' : The default menu set. It's value should be the car of one of the items in 
;;                              `one-key-sets-of-menus-alist'.
;; `one-key-associations-for-menu-sets' : An alist indicating which menu sets should be used with which 
;;                                        buffers/major-modes.
;; `one-key-default-sort-method-alist' : An alist of sorting methods to use on the `one-key' menu items.
;; `one-key-special-keybindings' : An list of special keys; labels, keybindings, descriptions and associated functions.
;; `one-key-default-special-keybindings' : List of special keys to be used if no other set of special keys is defined for 
;;                                         a given one-key menu type.
;; `one-key-menu-sets-special-keybindings' : List of special keys to be used for menu-sets menus (see 
;;                                           `one-key-default-special-keybindings' for more info).
;; `one-key-disallowed-keymap-menu-keys' : List of keys that should be excluded from one-key menus created from keymaps.
;; `one-key-types-of-menu' : A list of names of different types of `one-key' menu, and associated functions.
;; `one-key-persistent-menu-number' : If non-nil then when the default menu set is opened it will start with the same 
;;                                    menu as when previously opened.
;; `one-key-mode-line-message' : Form that when evaluated should produce a string for the mode-line in the *One-Key* 
;;                               buffer.

;; All above options can be customized through:
;;      M-x customize-group RET one-key RET
;;

;;; Change log:
;;
;; 2012/07/04
;;    * Joe Bloggs
;;       * This Change log is not being maintained anymore. Refer to the git log instead.
;; 2012/04/05
;;    * Joe Bloggs
;;       * Lots of changes! I have not been keeping track of them all.
;;       * Removed `one-key-items-per-line'
;;
;; 2012/3/01
;;    * Joe Bloggs
;;       * Lots of changes! Improved menu layout (can fit in more items), different menu sorting options,
;;       * colourization of menu items, limit items to those matching regexp, edit menu items in place,
;;       * manual repositioning of menu items in place.
;; 2010/12/07
;;    * Joe Bloggs
;;       * Added key-binding ("C-/" by default) to jump to source file of current one-key menu for editing.
;;       * Made fixed menu keys configurable with variables `one-key-key-hide' `one-key-key-quit' `one-key-key-up'
;;         `one-key-key-down' `one-key-key-pgup' `one-key-key-pgdown' `one-key-key-help' `one-key-key-edit'
;;         (they are called one-key-key-??? instead of one-key-???-key so that they will group together in the
;;          customization buffer).
;;       * Deleted `one-key-highlight-prompt' function since this is not used anywhere.
;;       * Added new variable `one-key-column-major-order', and altered `one-key-menu-format' function so that
;;         now you can choose whether items should be listed column first or row first.
;;
;; 2010/11/27
;;    * Joe Bloggs
;;       * Quick fix to one-key-template-write so that it remains in one-key-template-mode after writing
;;       
;; 2010/11/23
;;    * Joe Bloggs
;;       * Added `one-key-template-group-key-items-by-regexps', `one-key-template-describe-command',
;;         and associated keybindings and menu items.
;;
;; 2010/11/20
;;    * Joe Bloggs
;;       * Added `one-key-template-write' function for saving *One-Key-Template* buffer in `one-key-menus-location',
;;         and added keybinding `one-key-template-mode' and item to `one-key-menu-one-key-template-alist'.
;;       
;; 2010/11/18
;;    * Joe Bloggs
;;       * Added new major mode for editing one-key-menus in *One-Key-Template* buffer
;;       * Added following functions to aid editing menus in *One-Key-Template* buffer:
;;          `one-key-template-mode', `one-key-template-move-line-region', `one-key-template-move-line-region-up'
;;          `one-key-template-move-line-region-down', `one-key-template-test-menu', `one-key-template-mark-key-items'
;;          `one-key-template-sort-key-items-by-command-alphabetically',
;;          `one-key-template-sort-key-items-by-description-alphabetically',
;;          `one-key-template-sort-key-items-by-key-alphabetically',
;;          `one-key-menu-one-key-template', `one-key-menu-one-key'
;;       * Added keybindings for `one-key-template-mode'.
;;       * Altered `one-key-menu-format' function so that the keys are ordered by column instead of by row.
;;       * Added `one-key-toplevel-alist' customizable variable and `one-key-menu-toplevel' function.
;;       * Added `one-key-mode-alist' customizable variable and `one-key-get-menu' function.
;;       * Alterend `one-key-insert-template' and `one-key-show-template' functions so that they also add
;;         optional (commented) code to add items to `one-key-mode-alist' and `one-key-toplevel-alist'
;;       * Added customization variables `one-key-menus-location', `one-key-menus-regexp' and
;;         `one-key-auto-load-menus', and function `one-key-load-files'.
;;         Added code to automatically load menus if `one-key-auto-load-menus' is set to t.
;;       * Fixed spelling mistakes in documentation and added documentation for new features.
;;
;; 2010/09/27
;;    * Joe Bloggs
;;       * Altered one-key-make-template so that it adds the original keys to the descriptions of each item.
;;       
;; 2010/09/21
;;    * Joe Bloggs
;;       * Fixed a problems with one-key-make-template so it should work with more keymaps
;;       * Added ability to get help on one-key-menu items by pressing C-? followed by item key
;;       * Altered header text of menu
;;       * Fixed bug in one-key-menu so that window pops up if one-key-popup-window is t
;;         (this was also fixed independently by Andy, but I'm keeping my fix since it works fine)
;;
;; 2009/03/09
;;   * Andy Stewart:
;;      * Add `char-valid-p' for compatibility Emacs 22.
;;
;; 2009/02/25
;;   * Andy Stewart:
;;      * Fix a bug of `one-key-menu'.
;;
;; 2009/02/19
;;   * Andy Stewart:
;;      * Just show help message when first call function `one-key-menu',
;;        don't overwritten message from command.
;;      * Remove function `one-key-menu-quit' and
;;        option `one-key-show-quit-message', unnecessary now.
;;
;; 2009/02/10
;;   * rubikitch
;;      * Fix bug.
;;      * PageUp and PageDown are scroll page keys now.
;;      * Add new option `one-key-show-quit-message'.
;;
;; 2009/01/28
;;   * Andy Stewart:
;;      * Capitalize describe in variable `one-key-menu-*-alist'.
;;
;; 2009/01/27
;;   * rubikitch
;;      * Fix doc.
;;
;; 2009/01/26
;;   * rubikitch
;;      * Improve code.
;;
;; 2009/01/25
;;   * Andy Stewart:
;;      * Applied rubikitch's patch for generate
;;        template code automatically, very nice!
;;
;; 2009/01/22
;;   * rubikitch:
;;      * Add new option `one-key-items-per-line'.
;;      * Refactory code make it more clear.
;;      * Fix bug.
;;   * Andy Stewart:
;;      * Applied rubikitch's patch. Thanks!
;;      * Modified code make build-in keystroke
;;        can be overridden.
;;      * Fix doc.
;;
;; 2009/01/20
;;   * Andy Stewart:
;;      * Add new option `execute-last-command-when-miss-match'
;;        to function `one-key-menu', make user can execute
;;        last input command when miss match key alist.
;;
;; 2009/01/15
;;   * rubikitch:
;;      * Fix bug of `one-key-menu'.
;;      * Add recursion execute support for `one-key-menu'.*
;;        Thanks rubikitch patched for this! ;)
;;
;; 2009/01/04
;;   * Andy Stewart:
;;      * Add `okm-alternate-function' argument with function `one-key-menu'.
;;
;; 2008/12/22
;;   * Andy Stewart:
;;      * First released.
;;

;;; Acknowledgements:
;;
;;      rubikitch <rubikitch@ruby-lang.org>
;;              For send many patches.
;;

;;; TODO
;;
;; New special keybinding for limiting by regexp all items in current menu and all submenus?
;; Make functions autoloadable.
;; Prompt to save submenus when saving menu. Special keybinding to save all altered menus?
;; Autohighlighting of menu items using regexp associations?
;; Add to marmalade and elpa repos.
;; Handle mouse clicks so that item is executed when clicked on, and clicks outside the window close it. Make these customizable.
;; Applications of one-key-read-logical-formula : emms, org-mode header filtering, dired filtering?, gnus?, bbdb?
;; Allow background colour of menu items to be different.
;; Change `one-key-types-of-menu' to include sorting methods, and add buffer local variable to store current sort methods for each
;; menu.
;; one-key-read-logical-formula : change recursive algorithm to iterative one and allow more flexible editing of logical formula
;; (left/right to move to previous/next clause, and <insert>/<delete> to insert/delete clause. Need cursor to show position).
;; Also have special key to show items filtered by current state of formula.
;; Utilize 'booldnf' in linux for converting a logical formula into a DNF.
;;
;; one-key menus listing all commands in a given elisp library. Prompt user for library name first.
;; one-key-navigate - for navigating org-files and call trees (also have a look at org-favtable: http://orgmode.org/worg/org-contrib/org-favtable.html)
;;
;; Could use this https://github.com/nicferrier/emacs-kv for handling nested one-key menus?
;; (download from marmalade)
;;
;; Show filter regexp in mode-line
;;
;; one-key menu for showing ido keybindings (I always forget them). Needs to revert focus to minibuffer after closing.
;; In fact we should have menus for all minibuffer modes since there are quite a few keybindings that I always forget
;; (e.g. M-n in the prompt for dired-mark).

;;; Require
(eval-when-compile (require 'cl))
(require 'dired)
(require 'hexrgb)
;;; Code:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;; UTILITY FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; (these need to go first since customizable variables depend on them) ;;;;

(defun one-key-add-elements-to-list (list-var newelts)
  "Add elements in list NEWELTS to LIST-VAR if they are not already present and return the resulting list."
  (let ((listval (symbol-value list-var)))
    (append listval (loop for elt in newelts
                          unless (member elt listval)
                          collect elt))))

(defun one-key-add-to-alist (alist-var elt-cons &optional no-replace)
  "Add to the value of ALIST-VAR an element ELT-CONS if it isn't there yet, and return the new list.
If an element with the same car as the car of ELT-CONS is already present,
replace it with ELT-CONS unless NO-REPLACE is non-nil; if a matching
element is not already present, add ELT-CONS to the front of the alist.
The test for presence of the car of ELT-CONS is done with `equal'."
  (let ((existing-element (assoc (car elt-cons) (symbol-value alist-var))))
    (if existing-element
        (or no-replace
            (setcdr existing-element (cdr elt-cons)))
      (set alist-var (cons elt-cons (symbol-value alist-var)))))
  (symbol-value alist-var))

(defun one-key-add-elements-to-alist (alist-var newelts &optional no-replace)
  "Use `one-key-add-to-alist' to add each element of NEWELTS to ALIST-VAR.
NO-REPLACE has the same meaning as in `one-key-add-to-alist'."
  (loop for elt in newelts do
        (one-key-add-to-alist alist-var elt no-replace))
  (symbol-value alist-var))

(defun one-key-assq-list (symlist alist)
  "Return a list of the cdr's of elements of ALIST whose car's match a symbol in SYMLIST.
The matching is performed with assq so that only the first element of alist matching a symbol in SYMLIST is returned.
The elements returned will be in the same order as the elements of SYMLIST."
  (mapcar 'cdr (loop for symbol in symlist collect (assq symbol alist))))

(defun one-key-eval-if-symbol (sexp)
  "If SEXP is a symbol return the result of eval'ing it, otherwise return SEXP."
  (if (symbolp sexp) (eval sexp) sexp))

(defun one-key-get-special-key-contents (specialkeys)
  "Given a symbol or list of symbols from `one-key-special-keybindings', return the corresponding contents for each symbol.
The first element of the contents of each item will be replaced by a key description string by following symbol references
in `one-key-special-keybindings'.
In other words if `one-key-special-keybindings' contains the items (symba symbb \"descriptiona\" commanda), and
 (symbb \"a\" \"descriptionb\" commandb), then (one-key-get-special-key-contents '(symba symbb)) will return '((\"a\" \"descriptiona\" commanda) (\"b\" \"descriptionb\" commandb)). Notice that symbb is replaced by \"a\" in the returned list since
this is the key description for symbb. At most 5 symbolic links will be followed before setting the key to nil."
  (let* ((symbs (if (listp specialkeys) specialkeys
                  (if (symbolp specialkeys) (list specialkeys)
                    (error "Invalid argument"))))
         (items (one-key-assq-list symbs one-key-special-keybindings)))
    (loop for (key . rest) in items
          for x = 1
          do (while (and key (symbolp key))
               (setq key (cadr (assoc key one-key-special-keybindings)))
               (if (> x 4) (setq key nil) (setq x (1+ x))))
          collect (cons key (if key rest (list "undefined key!"))))))

(defun one-key-get-special-key-descriptions (specialkeys)
  "Given a symbol or list of symbols from `one-key-special-keybindings', return the corresponding key descriptions.
This can be used to find out which special keys are used for a particular one-key menu type.
If `specialkeys' is a single symbol then a single string will be returned.
If `specialkeys' is a list then a list of strings will be returned."
  (let* ((keys (mapcar 'car (one-key-get-special-key-contents specialkeys)))
         (len (length keys)))
    (if (> len 1) keys (car keys))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; CUSTOMIZABLE VARIABLES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup one-key nil
  "One key - easy access, refactorable menus."
  :group 'editing
  :link '(url-link "http://www.emacswiki.org/OneKey"))

(defgroup one-key-menu-sets nil
  "One key menu sets - sets of one-key menus."
  :group 'one-key)

(defcustom one-key-default-menu-keys
  (let (letters-and-numbers)
    (dotimes (i 26)
      (push (- ?Z i) letters-and-numbers))
    (dotimes (i 26)
      (push (- ?z i) letters-and-numbers))
    (dotimes (i 10)
      (push (- ?9 i) letters-and-numbers))
    letters-and-numbers)
  "A list of chars which may be used as the default keys in automatically generated `one-key' menus.
This list will be used for generating keys by the `one-key-generate-key' function."
  :group 'one-key
  :type '(repeat character))

(defcustom one-key-min-keymap-submenu-size 4
  "The minimum number of elements allowed in a submenu when creating menus from keymaps.
When creating menus from keymaps with `one-key-create-menus-from-keymap', submenus will be created for any prefix keys in
the keymap. These submenus contain the commands whose keybindings start with the corresponding prefix key.
If the number of items in a submenu would be less than `one-key-min-keymap-submenu-size' then instead of creating a submenu,
those items will be merged with the parent menu instead."
  :type 'integer
  :group 'one-key)

(defcustom one-key-window-toggle-sequence '(0.3 1.0 close)
  "List of numbers & symbols indicating the sequence of actions to perform on successive calls to `one-key-menu-window-toggle'.
Possible values in this list are:
A positive integer : indicates maximum size of one-key window in lines.
Floating point number between 0.0 & 1.0 : indicates maximum size of one-key window as fraction of the frame (or whole screen
if it has a frame to itself).
Symbol 'close : indicates to close the one-key window (but not kill the buffer).
Symbol 'deselect : indicates to keep the one-key window open but move focus to the associated window.
Symbol 'ownframe : indicates to place the one-key window in its own frame. Once one-key has it's own frame the window cannot be placed back in another frame without closing it first.
Symbol 'showhelp : show the *Help* buffer (see `one-key-window-help-buffer-own-frame').
Symbol 'hidehelp : hide the *Help* buffer.
A cons cell whose car is a symbol indicating the state to switch to, and whose cdr is an integer or float indicating the
size of the window after switching (interpreted in the same way as above).
At the moment the car is fixed to ownframe, but more states will be added soon."
  :type '(repeat (choice (integer :tag "Max number of lines")
                         (float :tag "Max height as fraction of frame/screen")
                         (const :tag "Return focus to previous window/buffer" deselect)
                         (const :tag "Open window in own frame" ownframe)
                         (const :tag "Show the one-key help buffer" showhelp)
                         (const :tag "Hide the one-key help buffer" hidehelp)
                         (const :tag "Close window" close)
                         (cons :tag "Pair" 
                                 (choice (const :tag "Open window in own frame" ownframe))
                                 (choice (integer :tag "Max number of lines")
                                         (float :tag "Max height as fraction of frame/screen")))))
  :group 'one-key)

(defvar one-key-window-toggle-pos 0
  "The position in `one-key-window-toggle-sequence' indicating the current one-key window state.")

(defcustom one-key-buffer-name "*One-Key*"
  "The buffer name of the one-key menu window."
  :type 'string
  :group 'one-key)

(defcustom one-key-help-buffer-name "*One-Key Help*"
  "The name of the help buffer for one-key."
  :type 'string
  :group 'one-key)

(defcustom one-key-column-major-order t
  "If true then menu items are displayed in column major order, otherwise row major order.
In column major order items will fill first column, then second, etc.
In row major order the rows are filled one at a time."
  :type 'boolean
  :group 'one-key)

(defcustom one-key-min-number-of-columns 2
  "An integer greater than 0 indicating the minimum number of columns to create for one-key menus.
Items in one-key menus will have their descriptions shortened (in the `one-key-format' function) so that the indicated
number of columns can be created."
  :type '(choice (const :tag "One column" 1)
                 (const :tag "Two columns" 2)
                 (const :tag "Three columns" 3)
                 (const :tag "Four columns" 4))
  :group 'one-key)

(defcustom one-key-menu-window-max-height nil
  "The max height of popup menu window."
  :type 'int
  :set (lambda (symbol value)
         (set symbol value)
         ;; Default is half height of frame.
         (unless value
           (set symbol (/ (frame-height) 2))))
  :group 'one-key)

(defcustom one-key-menus-save-file "~/.emacs.d/one-key-menus-save-file.el"
  "The file where `one-key' menus are saved."
  :type 'file
  :group 'one-key)

(defcustom one-key-autosave-menus nil
  "If non-nil then one-key menus will automatically be saved when created or changed."
  :type 'boolean
  :group 'one-key)

(defcustom one-key-exclude-from-save '("^prefix-key" "^major-mode")
  "List of regular expressions matching names of menus which should not be autosaved."
  :type '(repeat (regexp :tag "Regexp" :help-echo "Regular expression matching menu names to exclude from autosave." ))
  :group 'one-key)

(defcustom one-key-include-menubar-items t
  "Whether or not to include menu items with no keybinding when creating one-key menus from keymaps."
  :type '(choice (const :tag "Yes" t)
                 (const :tag "No" nil)
                 (const :tag "Prompt each time" 'prompt))
  :group 'one-key)

(defcustom one-key-item-foreground-colour "black"
  "Foreground colour of highlighted items in `one-key' menus."
  :type 'color
  :group 'one-key)

(defcustom one-key-auto-brighten-used-keys t
  "If non-nil then set brightness of menu items colours according to how often the keys are pressed."
  :type 'boolean
  :group 'one-key)

(defcustom one-key-submenus-replace-parents nil
  "If non-nil then when a submenu of a `one-key' menu is opened it will replace the parent menu.
Otherwise a new menu is created to hold the submenu and added to the current menu set."
  :type 'boolean
  :group 'one-key)

(defcustom one-key-major-mode-remap-alist '((Custom-mode . "custom-mode")
                                            (latex-mode ."LaTeX-mode"))
  "A list of cons cells mapping major modes to one-key-menus.
The car of each cell is the symbol of a major mode function (e.g. 'emacs-lisp-mode), and the cdr is the name of
a `one-key' menu associated with the major mode.
When a menu of type \"major-mode\" is opened this alist is checked, and if the current major mode is listed then the
associated menu will be used, otherwise the menu alist with name one-key-menu-???-alist (where ??? is the name of the
current major mode) will be used (and created if necessary)."
  :type '(alist :key-type (function :tag "Major mode" :help-echo "A major mode function") :value-type (string :tag "Name of associated menu" :help-echo "The name of the menu to be associated with the major mode"))
  :group 'one-key)

(defcustom one-key-menu-toplevel-alist '((("M" . "Cursor motion commands") .
                                          (lambda nil (interactive)
                                            (one-key-open-submenu "Cursor motion commands"
                                                                  one-key-menu-cursor-motion-commands-alist)))
                                         (("B" . "Buffer and file commands") .
                                          (lambda nil (interactive)
                                            (one-key-open-submenu "Buffer and file commands"
                                                                  one-key-menu-buffer-and-file-commands-alist)))
                                         (("E" . "Editing commands") .
                                          (lambda nil (interactive)
                                            (one-key-open-submenu "Editing commands"
                                                                  one-key-menu-editing-commands-alist)))
                                         (("s" . "Search/replace/grep commands") .
                                          (lambda nil (interactive)
                                            (one-key-open-submenu "Search/replace/grep commands"
                                                                  one-key-menu-searching-commands-alist)))
                                         (("S" . "Sorting commands") .
                                          (lambda nil (interactive)
                                            (one-key-open-submenu "Sorting commands"
                                                                  one-key-menu-sorting-commands-alist)))
                                         (("W" . "Window commands") .
                                          (lambda nil (interactive)
                                            (one-key-open-submenu "Window commands"
                                                                  one-key-menu-window-commands-alist)))
                                         (("C-h" . "prefix-key:C-h (help commands)") .
                                          (lambda nil (interactive)
                                            (funcall 'one-key-prefix-key-menu-command "C-h" t)))
                                         (("<C-escape>" . "prefix-key:ESC (all meta key keybindings)") .
                                          (lambda nil (interactive)
                                            (funcall 'one-key-prefix-key-menu-command "ESC" t)))
                                         (("M-g" . "prefix-key:M-g (error commands)") .
                                          (lambda nil (interactive)
                                            (funcall 'one-key-prefix-key-menu-command "M-g" t)))
                                         (("M-o" . "prefix-key:M-o (font-lock/centering commands)") .
                                          (lambda nil (interactive)
                                            (funcall 'one-key-prefix-key-menu-command "M-o" t)))
                                         (("M-s" . "prefix-key:M-s (occur/highlight commands)") .
                                          (lambda nil (interactive)
                                            (funcall 'one-key-prefix-key-menu-command "M-s" t)))
                                         (("C-x" . "prefix-key:C-x (all C-x keybindings)") .
                                          (lambda nil (interactive)
                                            (funcall 'one-key-prefix-key-menu-command "C-x" t)))
                                         (("r" . "prefix-key:C-x r (bookmark, rectangle and register commands)") .
                                          (lambda nil (interactive)
                                            (funcall 'one-key-prefix-key-menu-command "C-x r" t)))
                                         (("v" . "prefix-key:C-x v (version control commands)") .
                                          (lambda nil (interactive)
                                            (funcall 'one-key-prefix-key-menu-command "C-x v" t)))
                                         (("a" . "prefix-key:C-x a (abbrev commands)") .
                                          (lambda nil (interactive)
                                            (funcall 'one-key-prefix-key-menu-command "C-x a" t)))
                                         (("n" . "prefix-key:C-x n (narrow/widen commands)") .
                                          (lambda nil (interactive)
                                            (funcall 'one-key-prefix-key-menu-command "C-x n" t)))
                                         (("C-k" . "prefix-key:C-x C-k (keyboard macro commands)") .
                                          (lambda nil (interactive)
                                            (funcall 'one-key-prefix-key-menu-command "C-x C-k" t)))
                                         (("w" . "prefix-key:C-x w (highlight commands)") .
                                          (lambda nil (interactive)
                                            (funcall 'one-key-prefix-key-menu-command "C-x w" t)))
                                         (("RET" . "prefix-key:C-x RET (input/coding commands)") .
                                          (lambda nil (interactive)
                                            (funcall 'one-key-prefix-key-menu-command "C-x RET" t)))
                                         (("4" . "prefix-key:C-x 4 (other-window commands)") .
                                          (lambda nil (interactive)
                                            (funcall 'one-key-prefix-key-menu-command "C-x 4" t)))
                                         (("5" . "prefix-key:C-x 5 (other-frame commands)") .
                                          (lambda nil (interactive)
                                            (funcall 'one-key-prefix-key-menu-command "C-x 5" t)))
                                         (("6" . "prefix-key:C-x 6 (2 column mode commands)") .
                                          (lambda nil (interactive)
                                            (funcall 'one-key-prefix-key-menu-command "C-x 6" t)))
                                         (("C-c" . "prefix-key:C-c (mode specific bindings)") .
                                          (lambda nil (interactive)
                                            (funcall 'one-key-prefix-key-menu-command "C-c" t)))
                                         (("@" . "prefix-key:C-c @ (outline commands)") .
                                          (lambda nil (interactive)
                                            (funcall 'one-key-prefix-key-menu-command "C-c @" t)))
                                         (("," . "prefix-key:C-c , (senator/semantic commands)") .
                                          (lambda nil (interactive)
                                            (funcall 'one-key-prefix-key-menu-command "C-c ," t)))
                                         (("." . "prefix-key:C-c . (Ede commands)") .
                                          (lambda nil (interactive)
                                            (funcall 'one-key-prefix-key-menu-command "C-c ." t)))
                                         (("/" . "prefix-key:C-c / (Srecode commands)") .
                                          (lambda nil (interactive)
                                            (funcall 'one-key-prefix-key-menu-command "C-c /" t)))
                                         )
  "The `one-key' top-level alist.
Contains list of key items for toplevel one-key menu.
Each item contains a key, description and command, in that order.
The key should be entered in the same format as that returned by `describe-key'."
  :type '(alist :key-type (cons string string) :value-type function)
  :group 'one-key)

(if (featurep 'bookmark+)
    (one-key-add-elements-to-alist 'one-key-menu-toplevel-alist
                                   '((("p" . "prefix-key:C-x p (bookmark+ commands)") .
                                      (lambda nil (interactive)
                                        (funcall 'one-key-prefix-key-menu-command "C-x p" t)))
                                     (("j" . "prefix-key:C-x j (bookmark+ jump commands)") .
                                      (lambda nil (interactive)
                                        (funcall 'one-key-prefix-key-menu-command "C-x j" t))))
                                   t))

(defcustom one-key-sets-of-menus-alist (list '("default" "major-mode" "top-level" "menu-sets"))
  "Saved menu sets (sets of menus).
Each element in this list is a cons cell whose car is a name or description for the set, and whose cdr is a list of names
of menus which make up the set. Each menu name must correspond to a type in `one-key-types-of-menu' (which see),
and `one-key' must be able to reconstruct the menu from the name (which it will be able to if the corresponding entry
in `one-key-types-of-menu' is complete.
These menu sets may be opened from the \"menu-sets\" menu, and you may want to create different sets for different
projects."
  :type '(alist :key-type (string :tag "Set description/name" :help-echo "A name or description for this collection of menus")
                :value-type (repeat (string :tag "Menu" :help-echo "The name of the menu. Must correspond to a type in `one-key-types-of-menu'.")))
  :group 'one-key-menu-sets)

(defcustom one-key-default-menu-set "default"
  "The default menu set. It's value should be the car of one of the items in `one-key-sets-of-menus-alist'.
It may be changed by the user from the menu-sets `one-key' menu.
This is only meaningful if it is used with `one-key-open-menu-set' bound to a key so that the key can open a different
menu set if the user has altered its value."
  :type 'string
  :group 'one-key-menu-sets)

(defcustom one-key-associations-for-menu-sets nil
  "An alist indicating which menu sets should be used with which buffers/major-modes.
Each element is a cons cell whose car is either the symbol for a major-mode or a regular expression, and whose cdr is the
name of a menu set (i.e. the car of an element of `one-key-sets-of-menus-alist').
The `one-key-open-associated-menu-set' command uses this alist to determine which menu set to open.
It will open the first menu set in the list whose car matches either the current major-mode or the name of the current
buffer."
  :type '(alist :key-type (choice (symbol :tag "Major mode") (regexp :tag "Regular expression"))
                :value-type (string :tag "Name of menu set"))
  :group 'one-key-menu-sets)

(defcustom one-key-default-sort-method-alist
  '(("key" . (lambda (a b) (string< (caar a) (caar b))))
    ("description" . (lambda (a b) (string< (cdar a) (cdar b))))
    ("command" . (lambda (a b) (string< (prin1-to-string (cdr a))
                                      (prin1-to-string (cdr b)))))
    ;; ("colour name" . (lambda (a b) (string< (cadr (get-text-property 0 'face (cdar a)))
    ;;                                       (cadr (get-text-property 0 'face (cdar b))))))
    ("colour hue" . (lambda (a b)
                    (let* ((bg (cdr (assq 'background-color (frame-parameters))))
                           (cola (or (cadr (get-text-property 0 'face (cdar a))) bg))
                           (colb (or (cadr (get-text-property 0 'face (cdar b))) bg))
                           (hsva (destructuring-bind (r g b) (color-values cola)
                                   (hexrgb-rgb-to-hsv r g b)))
                           (hsvb (destructuring-bind (r g b) (color-values colb)
                                   (hexrgb-rgb-to-hsv r g b))))
                      (> (first hsva) (first hsvb)))))
    ("colour brightness" . (lambda (a b)
                           (let* ((bg (cdr (assq 'background-color (frame-parameters))))
                                  (cola
                                   (or (cadr (get-text-property 0 'face (cdar a))) bg))
                                  (colb
                                   (or (cadr (get-text-property 0 'face (cdar b))) bg))
                                  (hsva (destructuring-bind (r g b)
                                            (color-values cola)
                                          (hexrgb-rgb-to-hsv r g b)))
                                  (hsvb (destructuring-bind (r g b)
                                            (color-values colb)
                                          (hexrgb-rgb-to-hsv r g b))))
                             (> (third hsva) (third hsvb)))))
    ;; ("colour saturation" . (lambda (a b)
    ;;                        (let* ((bg (cdr (assq 'background-color (frame-parameters))))
    ;;                               (cola
    ;;                                (or (cadr (get-text-property 0 'face (cdar a))) bg))
    ;;                               (colb
    ;;                                (or (cadr (get-text-property 0 'face (cdar b))) bg))
    ;;                               (hsva (destructuring-bind (r g b)
    ;;                                         (color-values cola)
    ;;                                       (hexrgb-rgb-to-hsv r g b)))
    ;;                               (hsvb (destructuring-bind (r g b)
    ;;                                         (color-values colb)
    ;;                                       (hexrgb-rgb-to-hsv r g b))))
    ;;                          (> (second hsva) (second hsvb)))))
    ("length" . (lambda (a b) (> (length (cdar a)) (length (cdar b))))))
  "An alist of default sorting methods to use on the `one-key' menu items.
Each element is a cons cell of the form (NAME . PREDICATE) where NAME is a symbol for the name of the sort method,
and PREDICATE is a function which takes two items from the `one-key' menu alist as arguments and returns non-nil if
the first item should come before the second in the menu."
  :type '(alist :key-type (string :help-echo "Name of sort method")
                :value-type (function :help-echo "Predicate that returns non-nil if 1st item comes before 2nd"))
  :group 'one-key)

(defcustom one-key-sort-method-indices-alist nil
  "An alist indicating the current sort method to use for each menu.
Each element is in the form (NAME . INDEX) pairs where NAME is the name of a menu and INDEX is the position of the current
sort method in the sort methods alist associated with the menu (see `one-key-default-sort-method-alist').

This variable will change whenever you change the sort method for a menu, but you can save this option to fix the default
sort methods for different menus."
  :type '(alist :key-type (string :tag "Menu name" :help-echo "Name of menu")
                :value-type (integer :tag "Index" :help-echo "Index of default sort method in associated sort methods alist"))
  :group 'one-key)

(defcustom one-key-special-keybindings
  `((quit-close "q" "Quit and close menu window" (lambda nil (setq one-key-buffer-temp-action 'close)))
    (quit-open "C-q" "Quit, but keep menu window open"
               (lambda nil (setq one-key-buffer-temp-action 'deselect)))
    (toggle-persistence "<C-menu>" "Toggle menu persistence"
                        (lambda nil (if one-key-buffer-match-action
                                        (setq one-key-buffer-match-action nil
                                              one-key-buffer-miss-match-action nil)
                                      (setq one-key-buffer-match-action 'close
                                            one-key-buffer-miss-match-action 'close))))
    (toggle-display "<menu>" "Toggle menu display" one-key-menu-window-toggle)
    (next-menu "<left>" "Change to left menu"
               (lambda nil (setq one-key-buffer-menu-number
                                 (mod (1- one-key-buffer-menu-number)
                                      (length one-key-buffer-menu-alists))
                                 one-key-default-menu-number one-key-buffer-menu-number)
                 (one-key-update-buffer-contents)
                 (one-key-set-window-state
                  (nth one-key-window-toggle-pos one-key-window-toggle-sequence))))
    (prev-menu "<right>" "Change to right menu"
               (lambda nil (setq one-key-buffer-menu-number
                                 (mod (1+ one-key-buffer-menu-number)
                                      (length one-key-buffer-menu-alists))
                                 one-key-default-menu-number one-key-buffer-menu-number)
                 (one-key-update-buffer-contents)
                 (one-key-set-window-state
                  (nth one-key-window-toggle-pos one-key-window-toggle-sequence))))
    (skip-menus-left "<C-left>" "Skip menus to left"
                     (lambda nil (let* ((nummenus (length one-key-buffer-menu-alists))
                                        (skipnum (max (round (* nummenus 0.333)) 2)))
                                   (setq one-key-buffer-menu-number
                                         (mod (- one-key-buffer-menu-number skipnum) nummenus)
                                         one-key-default-menu-number one-key-buffer-menu-number)
                                   (one-key-update-buffer-contents)
                                   (one-key-set-window-state
                                    (nth one-key-window-toggle-pos one-key-window-toggle-sequence)))))
    (skip-menus-right "<C-right>" "Skip menus to right"
                      (lambda nil (let* ((nummenus (length one-key-buffer-menu-alists))
                                         (skipnum (max (round (* nummenus 0.333)) 2)))
                                    (setq one-key-buffer-menu-number
                                          (mod (+ one-key-buffer-menu-number skipnum) nummenus)
                                          one-key-default-menu-number one-key-buffer-menu-number)
                                    (one-key-update-buffer-contents)
                                    (one-key-set-window-state
                                     (nth one-key-window-toggle-pos one-key-window-toggle-sequence)))))
    (up "<up>" "Scroll/move up one line" one-key-menu-window-scroll-up-line)
    (down "<down>" "Scroll/move down one line" ,(apply-partially 'one-key-menu-window-scroll-up-line t))
    (scroll-down "<prior>" "Scroll menu down one page" ,(apply-partially 'one-key-menu-window-scroll-up t))
    (scroll-up "<next>" "Scroll menu up one page" one-key-menu-window-scroll-up)
    (documentation "<S-f1>" "Show one-key documentation"
                   (lambda nil (finder-commentary (locate-library "one-key"))))
    (save-menu "C-s" "Save state of current menu"
               (lambda nil (one-key-save-menu one-key-buffer-this-name
                                              one-key-buffer-full-list)))
    (toggle-help "<f1>" "Toggle this help buffer"
                 (lambda nil (if (get-buffer-window one-key-help-buffer-name)
                                 (one-key-set-window-state 'hidehelp)
                               (one-key-set-window-state 'showhelp)
                               (one-key-show-help one-key-buffer-special-keybindings))))
    (toggle-row/column-order "<f2>" "Toggle column/row ordering of items"
                             (lambda nil (setq one-key-column-major-order (not one-key-column-major-order))
                               (one-key-update-buffer-contents)))
    (sort-next "<f3>" "Sort items by next method"
               ,(apply-partially 'one-key-sort-items-by-next-method t))
    (sort-prev "<C-f3>" "Sort items by previous method"
               ,(apply-partially 'one-key-sort-items-by-next-method t t))
    (reverse-order "<f4>" "Reverse order of items" one-key-reverse-item-order)
    (limit-items "/" "Limit items to those matching regexp"
                 (lambda nil (setq one-key-buffer-filter-regex
                                   (read-regexp "Regular expression matching items to be filtered"))
                   (one-key-update-buffer-contents)))
    (highlight-items "C-/" "Highlight items matching regexp"
                     (lambda nil (let ((regex (read-regexp "Regular expression matching items to be coloured"))
                                       (bgcolour (read-color "Colour: ")))
                                   (one-key-highlight-matching-items
                                    bgcolour
                                    (lambda (item) (string-match regex (cdar item)))))))
    (edit-item "<f5>" "Edit a menu item" one-key-edit-menu-item)
    (delete-item "<f6>" "Delete a menu item" one-key-delete-menu-item)
    (kill-items "<f7>" "Copy/kill coloured items" one-key-copy/kill-items)
    (yank-items "<C-f7>" "Yank copied items" one-key-yank-items)
    (swap-keys "<f8>" "Swap menu item keys" one-key-swap-menu-items)
    (add-item "<f9>" "Add a menu item" one-key-prompt-to-add-menu-item)
    (add-menu "<C-f9>" "Add a menu" one-key-add-menus)
    (remove-menu "<C-S-f9>" "Remove this menu" one-key-delete-menus)
    (move-item "<f10>" "Reposition item (with arrow keys)"
               (lambda nil (let ((key (one-key-key-description (read-event "Enter key of item to be moved"))))
                             (setq one-key-current-item-being-moved key))))
    (donate "<f11>" "Donate to support further development"
            (lambda nil (browse-url "http://onekeydonate.dynalias.net")))
    (report-bug "<C-f11>" "Report a bug" one-key-submit-bug-report)
    (show-menusets "C-h" "Show menus in menu set"
                   (lambda nil
                     (let* ((key (read-event "Enter the key for the menu set"))
                            (item (one-key-get-menu-item
                                   key one-key-buffer-full-list))
                            (menuset (assoc (cdar item) one-key-sets-of-menus-alist))
                            (desc (car menuset))
                            (names (cdr menuset)))
                       (message "%S" names))))
    (customize-menusets "C-c" "Customize menu sets"
                        (lambda nil
                          (one-key-set-window-state 'close)
                          (with-selected-window (previous-window)
                            (customize-group 'one-key-menu-sets))))
    (change-default-menuset "<f5>" "Change default menu set"
                            (lambda nil
                              (let* ((key (read-event "Press the key of item to set as default"))
                                     (item (one-key-get-menu-item
                                            key one-key-buffer-full-list))
                                     (name (cdar item))
                                     (pos (position "menu-sets" one-key-buffer-menu-names :test 'equal)))
                                (if name (eval `(customize-save-variable 'one-key-default-menu-set
                                                                         ,(substring-no-properties name))))
                                (if pos (setf (nth pos one-key-buffer-menu-alists)
                                              (one-key-build-menu-sets-menu-alist))))
                              (one-key-update-buffer-contents)))
    (save-menuset save-menu "Save current menu set"
                  (lambda nil
                    (let* ((names (mapcar 'car one-key-sets-of-menus-alist)) 
                           (newname (read-string "Name for menu set: "))
                           (validnames
                            (remq nil
                                    (mapcar
                                     (lambda (name) (if (one-key-get-menu-type name) name))
                                     one-key-buffer-menu-names)))
                           newset oldsets)
                      (unless (and (member newname names)
                                   (not (y-or-n-p
                                         "A menu set with that name already exists, overwrite it?")))
                        (setq newset (if (y-or-n-p "Include \"menu-sets\" menu?")
                                         (append (list newname) validnames)
                                       (remove "menu-sets" (append (list newname) validnames))))
                        (setq oldsets (remove-if (lambda (item) (string= (car item) newname))
                                                 one-key-sets-of-menus-alist))
                        (if (y-or-n-p "Associate menu set with major-mode?")
                            (let ((mode (with-selected-window (or one-key-buffer-associated-window
                                                                  (selected-window))
                                          major-mode)))
                              (eval `(customize-save-variable 'one-key-associations-for-menu-sets
                                                              ',(one-key-add-to-alist
                                                                 'one-key-associations-for-menu-sets
                                                                 (cons mode newname)))))
                          (if (y-or-n-p "Associate menu set with current buffer?")
                              (let ((regex (with-selected-window (or one-key-buffer-associated-window
                                                                  (selected-window))
                                             (concat "^" (regexp-quote (buffer-name)) "$"))))
                                (eval `(customize-save-variable 'one-key-associations-for-menu-sets
                                                                ',(one-key-add-to-alist
                                                                   'one-key-associations-for-menu-sets
                                                                   (cons regex newname)))))))
                        (eval `(customize-save-variable 'one-key-sets-of-menus-alist
                                                        ',(append oldsets (list newset))))))
                    (one-key-update-buffer-contents)))
    (rebuild-menu "<M-f11>" "Rebuild the menu" one-key-rebuild-menu)
    (read-tree-up "RET" "Complete current list"
                  (lambda nil (setq selected-item 'goup)))
    (read-tree-up2 ")" "Complete current list"
                   (lambda nil (setq selected-item 'goup)))
    (read-tree-down "SPC" "Start new list recursively"
                    (lambda nil (setq selected-item 'godown)))
    (read-tree-down2 "(" "Start new list recursively"
                     (lambda nil (setq selected-item 'godown)))
    (read-tree-delete "<backspace>" "Remove last item from list"
                      (lambda nil (setq selected-item 'del)))
    (read-logical-negate "!" "Negate next item" (lambda nil (setq selected-item 'not))))
  "An list of special keys; labels, keybindings, descriptions and associated functions.
Each item in the list contains (in this order):

  1) A symbol to reference the keybinding in the special keybinding sets for different menu types.

  2) A string representation of the key (as returned by `one-key-key-description'), or a symbol referencing
     another item whose key description should be used instead (this allows you to keep you special keybindings
     in sync when you use different items for different menu types).
     Warning: make sure you don't end up with a circular set of key references or one-key will get stuck in a loop.

  3) A short description of the associated action. This description will be displayed in the one-key help buffer.

  4) A function for performing the action. The function takes no arguments but may use the buffer local variables
     whose names begin with one-key-buffer- (which see).

These keybindings may be referred to by other variables that contain the special keybindings for different one-key menu
types. See `one-key-default-special-keybindings' for example."
  :group 'one-key
  :type '(repeat (list (symbol :tag "Name" :help-echo "A reference name for this keybinding (no spaces).")
                       (string :tag "Keybinding" :help-echo "String representation of the keybinding for this action")
                       (string :tag "Description" :help-echo "Description to display in help buffer")
                       (function :tag "Function" :help-echo "Function for performing action. See description below for further details."))))

(defvar one-key-general-special-keybindings
  '(quit-close quit-open toggle-persistence toggle-display next-menu prev-menu skip-menus-left skip-menus-right up down
               scroll-down scroll-up toggle-help toggle-row/column-order sort-next sort-prev reverse-order)
  "Special keybindings that should be used for all menu types.")

(defcustom one-key-default-special-keybindings
  (one-key-add-elements-to-list
   'one-key-general-special-keybindings 
   '(help documentation save-menu limit-items highlight-items edit-item delete-item kill-items yank-items swap-keys
          add-item add-menu remove-menu move-item donate report-bug rebuild-menu))
  "List of special keys to be used if no other set of special keys is defined for a given one-key menu type.
These keys are for performing general tasks on the menu such as sorting items, deleting items, etc.
Each element of this list is a reference to one of the keybindings defined in `one-key-special-keybindings'.
The keys will be displayed in the one-key help buffer in the order shown when the `one-key-show-help' function is executed."
  :group 'one-key
  :type '(repeat (symbol :tag "Name" :help-echo "The name/symbol corresponding to the keybinding.")))

(defcustom one-key-menu-sets-special-keybindings
  (one-key-add-elements-to-list
   'one-key-general-special-keybindings
   '(show-menusets save-menuset customize-menusets limit-items highlight-items change-default-menuset add-menu
                   remove-menu donate report-bug))
  "List of special keys to be used for menu-sets menus (see `one-key-default-special-keybindings' for more info)."
  :group 'one-key
  :type '(repeat (symbol :tag "Name" :help-echo "The name/symbol corresponding to the keybinding.")))

(defcustom one-key-major-mode-special-keybindings
  (one-key-add-elements-to-list
   'one-key-general-special-keybindings 
   '(help documentation save-menu limit-items highlight-items edit-item delete-item kill-items yank-items swap-keys
          add-item add-menu remove-menu move-item donate report-bug rebuild-menu))
  "List of special keys to be used for major-mode menus (see `one-key-default-special-keybindings' for more info)."
  :group 'one-key
  :type '(repeat (symbol :tag "Name" :help-echo "The name/symbol corresponding to the keybinding.")))

(defcustom one-key-disallowed-keymap-menu-keys '("M-TAB")
  "List of keys that should be excluded from one-key menus created from keymaps.
Each item in this list is a key description as returned by `one-key-key-description'."
  :group 'one-key
  :type '(repeat string))

(defcustom one-key-types-of-menu nil
  "A list of names of different types of `one-key' menu, and associated functions.
Each item in the list contains (in this order):

  1) The name for this menu type.

  2) A function which takes a string as its only argument and returns non-nil if that string corresponds to the name of
     a menu of this type, otherwise it returns nil. Note: this function should only return non-nil if a menu can be
     reconstructed from the name using the next item in this list.

  3) A function which takes the menu name as its only argument and returns a cons cell whose car is the new name or list
     of names for the menus, and whose cdr is a menu alist, a symbol whose value is a menu alist, or a list of symbols
     and/or menu alists. The number of names returned in the car should be equal to the number of menu alists/symbols
     returned in the cdr. Alternatively this can be a cons cell whose car is the name/names and whose cdr is the menu
     alist/alists.

  4) An function that takes no arguments and returns a title string for the `one-key' menu.
     The function will be evaluated in the context of the `one-key-highlight-menu' function, and will be processed by
     `one-key-highlight' before display. You should look at the `one-key-highlight-menu' function to see which variables
     may be used in this format string.
     Alternatively if this item is nil then `one-key-default-title-func' will be used.

  5) Either a list of special keybindings in the same form as `one-key-default-special-keybindings', or a symbol
     whose value is such a list, or nil. If nil then `one-key-default-special-keybindings' will be used.

  6) An alist of sort methods to use for this menu type. If nil then `one-key-default-sort-method-alist' will be used.
     See the documentation for `one-key-default-sort-method-alist' to see the required format of this variable."
  :type '(repeat (list (string :tag "Name"
                               :help-echo "A name for this menu type.")
                       (function :tag "Condition"
                                 :help-echo "A function which returns the new menu name(s) when passed a name corresponding to this type, and returns nil otherwise.")
                       (choice (symbol :tag "Menu alist symbol(s)"
                                       :help-echo "A symbol whose value is a menu alist of keys for menus of this type, or a list of such menus.")
                               (function :tag "Menu alist function"
                                         :help-echo "A function which takes the menu name as its only argument and returns either a `one-key' menu alist of keys, or a symbol whose value is such a list, or a list of menus and/or symbols."))
                       (function :tag "Title string function"
                                 :help-echo "A function which returns a title string for `one-key' menus of this type.")
                       (choice (symbol :tag "Special keybindings symbol"
                                       :help-echo "A symbol whose value is a list of special keybindings for menus of this type")
                               (repeat :tag "Special keybindings"
                                       (list (string :tag "Keybinding"
                                                     :help-echo "String representation of the keybinding for this action")
                                             (string :tag "Description"
                                                     :help-echo "Description to display in help buffer")
                                             (function :tag "Function"
                                                       :help-echo "Function for performing action. See description below for further details."))))
                       (alist :tag "Sort methods"
                              :key-type (string :tag "Name" :help-echo "Name for sort method")
                              :value-type (function :tag "Function" :help-echo "Predicate that returns non-nil if 1st item comes before 2nd"))))
  :group 'one-key)

(defcustom one-key-persistent-menu-number t
  "If non-nil then when the default menu set is opened it will start with the same menu as when previously opened."
  :group 'one-key
  :type 'boolean)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FACES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defface one-key-name
  '((t (:foreground "Gold")))
  "Face for highlighting name."
  :group 'one-key)

(defface one-key-keystroke
  '((t (:foreground "DarkRed")))
  "Face for highlighting keystroke."
  :group 'one-key)

(defface one-key-prompt
  '((t (:foreground "khaki3")))
  "Face for highlighting prompt."
  :group 'one-key)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; STRUCTURES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct one-key-menu-struct
  "Information for constructing a one-key menu."
  name items specialkeys filter filtereditems title sortmethods)

(defstruct one-key-menus
  "Set of one-key-menu objects, and name of menu set, along with other relevant information."
  (name :read-only t) menus)

(defun one-key-get-slots (struct)
  "Return list of symbols for the slots in the cl-structure STRUCT."
  (mapcar 'car (cdr (get (intern-soft (substring (symbol-name (elt struct 0)) 10))
                         'cl-struct-slots))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; GLOBAL VARIABLES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar one-key-current-menus nil
  "The list of one-key menus used in the current one-key buffer.")

(defvar one-key-displayed-sort-method nil
  "The sort method displayed in the mode line.")

(defvar one-key-mode-line-message '(format "Press %s for help, %s to quit. Sorted by %s (%s first)."
                                              (cadr (assoc 'toggle-help one-key-special-keybindings))
                                              (cadr (assoc 'quit-close one-key-special-keybindings))
                                              one-key-displayed-sort-method (if one-key-column-major-order "columns" "rows"))
  "Form that when evaluated should produce a string for the mode-line in the *One-Key* buffer.
This should probably be left alone unless you remove `toggle-help' or `quit-close' from `one-key-special-keybindings'")

(defvar one-key-copied-items nil
  "List of menu items that have been killed using the `one-key-copy/kill-items' function.")

(defvar one-key-null-keys (regexp-opt '("<remap>" "mouse" "<follow-link>"))
  "Regular expression matching key descriptions of keymap items that should be excluded from `one-key' menus.")

(defvar one-key-menu-show-key-help nil
  "If true show help for function associated with next keystroke, when it is pressed in the one-key-menu.")

(defvar one-key-current-item-being-moved nil
  "The key corresponding to the item currently being moved in the `one-key' menu, or nil if none is being moved.")

(defvar one-key-altered-menus nil
  "List of menu alist variables that should be saved on exit if `one-key-autosave-menus' is true.")

(defvar one-key-default-menu-number nil
  "The default menu number to use when opening the default menu set if `one-key-persistent-menu-number' is non-nil.")

(defvar one-key-mode-line-format
  '("%e" "%e"
    #("-" 0 1
      (help-echo "mouse-1: Select (drag to resize)\nmouse-2: Make current window occupy the whole frame\nmouse-3: Remove current window from display"))
    mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification
    #(" " 0 1
      (help-echo "mouse-1: Select (drag to resize)\nmouse-2: Make current window occupy the whole frame\nmouse-3: Remove current window from display"))
    mode-line-position
    #(" " 0 1
      (help-echo "mouse-1: Select (drag to resize)\nmouse-2: Make current window occupy the whole frame\nmouse-3: Remove current window from display"))
    (:eval (eval one-key-mode-line-message))
    (global-mode-string
     ("" global-mode-string
      #(" " 0 1
        (help-echo "mouse-1: Select (drag to resize)\nmouse-2: Make current window occupy the whole frame\nmouse-3: Remove current window from display"))))
    (:eval
     (unless
         (display-graphic-p)
       #("-%-" 0 3
         (help-echo "mouse-1: Select (drag to resize)\nmouse-2: Make current window occupy the whole frame\nmouse-3: Remove current window from display")))))
  "The `mode-line-format' for the *One-Key* buffer.")

(defvar one-key-default-title-func (lambda nil
                                     (let* ((keystr (one-key-get-special-key-descriptions 'donate))
                                            (msg (concat "To support the author press "
                                                         keystr " or donate bitcoins to 1HnSqGHrVenb1t2V2aijyocWyZcd7qt1k\n"))
                                            (len (length msg))
                                            (dif (- (window-width) len)))
                                       (if (<= dif 0)
                                           msg
                                         (concat (make-string (/ dif 2) ? ) msg))))
  "Function to return default message for one-key menus, prompting for donations.")

(defvar one-key-maintainer-email "vapniks@yahoo.com"
  "Email address of current maintainer.")

(defvar one-key-version "1.0"
  "Version number of this version of one-key")

(defvar one-key-key-description-remap
  '(("<return>" . "RET")
    ("<tab>" . "TAB")
    ("<space>" . "SPC")
    ("<begin>" . "<home>")
    ("<escape>" . "ESC")
    ("<C-escape>" . "C-ESC")
    ("<delete>" . "DEL"))
  "Alist of key descriptions and their preferred versions.
This is required in order that keys such as RET (which can also be described as <return> are always described and
recognized the same way.")

;;;;;;;;;;;;;;;;;;;;;;;;;;; Some menus for the toplevel ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar one-key-menu-sorting-commands-alist
  '((("l" . "Sort lines alphabetically (M-x sort-lines)") . sort-lines)
    (("p" . "Sort paragraphs alphabetically (M-x sort-paragraphs)") . sort-paragraphs)
    (("P" . "Sort pages alphabetically (M-x sort-pages)") . sort-pages)
    (("f" . "Sort lines alphabetically by whitespace seperated fields (M-x sort-fields)") . sort-fields)
    (("n" . "Sort lines numerically by whitespace seperated fields (M-x sort-numeric-fields)") . sort-numeric-fields)
    (("c" . "Sort lines alphabetically by columns defined by point and mark (M-x sort-columns)") . sort-columns)
    (("r" . "Sort lines matching 1st regexp by field matching 2nd regexp, alphabetically (M-x sort-regexp-fields)") . sort-regexp-fields)
    (("R" . "Reverse order of lines in region (M-x reverse-region)") . reverse-region))
  "The `one-key' menu alist for sorting commands.")

(defvar one-key-menu-searching-commands-alist
  `((("C-s" . "Search forward (C-s)") . (lambda nil (interactive) (isearch-mode t nil nil t)))
    (("C-r" . "Search backward (C-r)") . (lambda nil (interactive) (isearch-mode nil nil nil t)))
    (("M-%" . "Search-replace string (M-%)") . query-replace)
    (("C-M-s" . "Search forward for regexp (C-M-s)") . isearch-forward-regexp)
    (("C-M-r" . "Search backward for regexp (C-M-r)") . isearch-backward-regexp)    
    (("C-M-%" . "Search-replace regexp (C-M-%)") . query-replace-regexp)
    (("b" . "Regular expression builder (M-x re-builder)") . re-builder)
    (("l" . "Grep files in dir (M-x lgrep)") . lgrep)
    (("r" . "Grep files in dir and subdirs (M-x rgrep)") . rgrep)
    (("g" . "Run grep via find (M-x grep-find)") . grep-find)
    ,(if (featurep 'mgrep) '(("m" . "mgrep - grep dirs stored in `mgrep-list' (M-x mgrep)") . mgrep))
    ,(if (featurep 'ack-and-a-half) '(("a" . "ack - grep source code files (M-x ack-and-a-half)") . ack-and-a-half))
    ,(if (featurep 'ack-and-a-half) '(("o" . "open source code file (M-x ack-and-a-half-find-file)") . ack-and-a-half-find-file))
    (("f" . "Find files and put them in dired buffer (M-x find-dired)") . find-dired)
    (("n" . "Find files matching by name (M-x find-name-dired)") . find-name-dired)
    (("c" . "Find files matching by contents (M-x find-grep-dired)") . find-grep-dired)
    )
  "The `one-key' menu alist for search/replace commands.")

(defvar one-key-menu-editing-commands-alist
  '((("C-SPC" . "Set mark (C-SPC)") . set-mark-command)
    (("h" . "Mark entire buffer (C-x h)") . mark-whole-buffer)
    (("C-p" . "Mark page (C-x C-p)") . mark-page)
    (("h" . "Mark paragraph (M-h)") . mark-paragraph)
    (("C-x" . "Exchange point and mark (C-x C-x)") . exchange-point-and-mark)
    (("M-w" . "Copy selected region (M-w OR C-Insert)") . kill-ring-save)
    (("C-w" . "Cut selected region (C-w OR S-DEL)") . kill-region)
    (("C-y" . "Paste/yank last copied/cut text (C-y)") . yank)
    (("M-y" . "Paste/yank ealier copied/cut text (M-y)") . yank-pop)
    (("C-d" . "Delete character forward (C-d)") . delete-char)
    (("M-DEL" . "Delete word backward (M-DEL)") . backward-kill-word)
    (("M-D" . "Delete word forward (M-D OR C-S-DEL)") . kill-word)
    (("C-k" . "Delete to end of line (C-k)") . kill-line)
    (("M-k" . "Delete to end of sentence (M-k)") . kill-sentence)
    (("M-u" . "Make word uppercase (M-u)") . upcase-word)
    (("M-l" . "Make word lowercase (M-l)") . downcase-word)
    (("M-c" . "Capitalize word (M-c)") . capitalize-word)
    (("C-t" . "Transpose/swap adjacent characters (C-t)") . transpose-chars)
    (("M-t" . "Transpose/swap adjacent words (M-t)") . transpose-words)
    (("t" . "Transpose/swap lines (C-x C-t)") . transpose-lines)
    (("M-^" . "Join previous line (M-^)") . delete-indentation)
    (("TAB" . "Indent all lines in region (C-x TAB)") . indent-rigidly)
    (("M-q" . "Fill or justify paragraph (M-q)") . fill-paragraph)
    (("C-o" . "Open a new line before this one (C-o)") . open-line)
    (("1" . "Remove all but 1 empty line (C-x C-o)") . delete-blank-lines))
  "The `one-key' menu alist for editing commands.")

(defvar one-key-menu-cursor-motion-commands-alist 
  '((("C-f" . "Forward a character (C-f)") . forward-char)
    (("C-b" . "Backward a charcter (C-b)") . backward-char)
    (("C-n" . "Forward a line (C-n)") . next-line)
    (("C-p" . "Backward a line (C-p)") . previous-line)
    (("C-a" . "Goto the beginning of line (C-a)") . move-beginning-of-line)
    (("C-e" . "Goto the end of line (C-e)") . move-end-of-line)
    (("M-f" . "Forward a word (M-f)") . forward-word)
    (("M-b" . "Backward a word (M-b)") . backward-word)
    (("M-e" . "Forward a sentence (M-e)") . forward-sentence)
    (("M-a" . "Backward a sentence (M-a)") . backward-sentence)
    (("M-<" . "Goto the beginning of buffer (M-<)") . beginning-of-buffer)
    (("M->" . "Goto the end of buffer (M->)") . end-of-buffer)
    (("{" . "Move backwards paragraph (M-{") . backward-paragraph)
    (("}" . "Move forwards paragraph (M-})") . forward-paragraph)
    (("[" . "Move backwards page (C-x [)") . backward-page)
    (("]" . "Move forwards page (C-x ])") . forward-page))
  "The `one-key' menu alist for cursor motion commands.")

(defvar one-key-menu-buffer-and-file-commands-alist 
  '((("C-f" . "Open file (C-x C-f)") . find-file)
    (("d" . "Open directory (C-x d)") . (lambda nil (interactive)
                                          (if (featurep 'ido) (call-interactively 'ido-dired)
                                            (call-interactively 'dired))))
    (("C-s" . "Save current file (C-x C-s)") . save-buffer)
    (("C-w" . "Save file as (C-x C-w)") . write-file)
    (("s" . "Prompt user for files to save (C-x s)") . save-some-buffers)
    (("i" . "Insert file contents into buffer (C-x i)") . insert-file)
    (("b" . "Switch to another buffer (C-x b)") . switch-to-buffer)
    (("4" . "Switch to another buffer in another window (C-x 4 b)") . switch-to-buffer-other-window)
    (("5" . "Switch to another buffer in another frame (C-x 5 b)") . switch-to-buffer-other-frame)
    (("<C-left>" . "Select previous buffer (C-x <left>)") . previous-buffer)
    (("<C-right>" . "Select next buffer (C-x <right>)") . next-buffer)    
    (("k" . "Kill buffer (C-x k)") . kill-buffer)
    (("K" . "Prompt to kill buffers (M-x kill-some-buffers)") . kill-some-buffers)
    (("C-v" . "Kill current buffer then open another file (C-x C-v)") . find-alternate-file)
    (("m" . "Menu for switching/killing buffers (M-x bs-show)") . bs-show)
    (("l" . "List existing buffers (C-x C-b)") . list-buffers)
    (("C-q" . "Toggle read-only status of buffer (C-x C-q)") . toggle-read-only)
    (("R" . "Rename buffer (M-x rename-buffer)") . rename-buffer)
    )
  "The `one-key' menu alist for buffer and file commands.")

(defvar one-key-menu-window-commands-alist
  '((("1" . "Make this window fill it's frame (C-x 1)") . delete-other-windows)
    (("2" . "Split this window vertically (C-x 2)") . split-window-vertically)
    (("3" . "Split this window horizontally (C-x 3)") . split-window-horizontally)
    (("o" . "Select next window (C-x o)") . other-window)
    (("0" . "Kill this buffer and window (C-x 4 0)") . kill-buffer-and-window)
    (("^" . "Make this window taller (C-x ^)") . enlarge-window)
    (("-" . "Make this window shorter (C-x -)") . shrink-window-if-larger-than-buffer)
    (("}" . "Make this window wider (C-x })") . enlarge-window-horizontally)
    (("{" . "Make this window narrower (C-x {)") . shrink-window-horizontally)
    (("+" . "Make all windows the same height (C-x +)") . balance-windows)
    (("C-M-v" . "Scroll the next window (C-M-v)") . scroll-other-window)
    (("b" . "Select buffer in another window (C-x 4 b)") . switch-to-buffer-other-window)
    (("C-o". "Display buffer in another window (C-x 4 C-o)") . display-buffer)
    (("f" . "Open a file in another window (C-x 4 f)") . find-file-other-window)
    (("d" . "Open a directory in another window (C-x 4 d)") . dired-other-window)
    (("m" . "Compose email in another window (C-x 4 m)") . mail-other-window)
    (("." . "Find tag of current tags table in another window (C-x 4 .)") . find-tag-other-window)
    (("r" . "Open a file as read-only in another window (C-x 4 r)") . find-file-read-only-other-window))
  "The `one-key' menu alist for window commands.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Buffer local variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Only declaring these vars here so that they are documented. They don't actually need to be declared globally,
;; as they are declared local to the one-key buffer when one-key-mode is run.
(defvar one-key-buffer-menu-number nil
  "The index of the current menu in `one-key-buffer-menu-alists'.

This variable is local to the one-key buffer.")

(defvar one-key-buffer-menu-names nil
  "The current list of menu names.

This variable is local to the one-key buffer.")

(defvar one-key-buffer-menu-alists nil
  "The current list of menu lists.

This variable is local to the one-key buffer.")

(defvar one-key-buffer-special-keybindings nil
  "The special keybindings for the current menu.

This variable is local to the one-key buffer.")

(defvar one-key-buffer-associated-window nil
  "The window associated with the one-key buffer.
This is set to the window that was selected when the one-key menu was opened.

This variable is local to the one-key buffer.")

(defvar one-key-buffer-temp-action nil
  "Indicates what to do after the last key was pressed in the one-key buffer. 
This variable may be set by special key or menu commands in the context of the one-key buffer.
It's value is treated in the same way as `one-key-buffer-match-action' except that it is overridden by
`one-key-buffer-match-action' after a menu command if `one-key-buffer-temp-action' is nil.
The default value is nil and it is reset to nil after performing the associated action.

This variable is local to the one-key buffer.")

(defvar one-key-buffer-match-action 'close
  "Indicates what to do after a matching (menu item) key is pressed, and the associated command is executed.
The value should be either nil which means do nothing and leave the window open, a function to be called with
no arguments, or a symbol which is interpreted in the same way as the possible values for `one-key-window-toggle-sequence'.
The default value is 'close (i.e. close the one-key window).
This variable may be temporarily overridden by the value of `one-key-buffer-temp-action' by individual commands.

This variable is local to the one-key buffer.")

(defvar one-key-buffer-miss-match-action 'close
  "Indicates what to do after a miss-match (non menu item) key is pressed.
The possible values are the same as for `one-key-buffer-match-action' or the symbols 'execute 'executeclose
which will execute the key (in the associated window) and leave the one-key window open/closed respectively.
The default value is 'close (i.e. close the one-key window).

This variable is local to the one-key buffer.")

(defvar one-key-buffer-dedicated-frame nil
  "If non-nil then this var holds the frame dedicated to the one-key menu.

This variable is local to the one-key buffer.")

(defvar one-key-buffer-filter-regex nil
  "A regular expression matching menu items to be diplayed in one-key menu.
Any menu items in the current menu alist that don't match this regexp won't be displayed
when `one-key-update-buffer-contents' is executed.
If nil then all items are displayed.

This variable is local to the one-key buffer.")

(defvar one-key-buffer-filtered-list nil
  "The list of menu items that match `one-key-buffer-filter-regex'.
These are the items that are displayed in the one-key buffer.

This variable is local to the one-key buffer.")

;;;;;;; This function is needed for defining the major modes
(defun one-key-header-line-format (names menu-number)
  "Return the preferred value of `header-line-format' for the *One-Key* buffer.
NAMES should be the current list of menu names displayed, or just a single name if there is only one menu.
MENU-NUMBER should be nil if NAMES is a single name, otherwise it should index the current menu in NAMES."
  (let* ((prenames (if (and menu-number (> menu-number 0))
                       (concat (mapconcat 'identity (subseq names 0 menu-number) " ") " ")))
         (nameslen (length names))
         (postnames (if (and menu-number (< menu-number (1- nameslen)))
                        (concat " " (mapconcat 'identity (subseq names (1+ menu-number) nameslen) " "))))
         (name (if menu-number (nth menu-number names) names))
         (name1 (propertize name 'face 'one-key-name))
         (namelen (length name))
         (prelen (length prenames))
         (postlen (length postnames))
         (winwidth (window-width))
         (namepos (/ (- winwidth namelen) 2))
         (startpos (- namepos prelen))
         (postnames2 (if postnames (substring postnames 0 (min namepos postlen)))))
    (if (>= startpos 0)
        (concat (make-string startpos ? ) prenames name1 postnames2)
      (concat (substring prenames (- startpos) prelen) name1 postnames2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MAJOR MODES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-derived-mode one-key-mode fundamental-mode "One-Key"
  "The major-mode for the one-key menu buffer."
  :group 'one-key
  ;; Make sure brackets are not highlighted by syntax table
  :syntax-table (let ((table (make-syntax-table)))
                  (dolist (char '(40 41 60 62 91 93 123 125))
                    (modify-syntax-entry char "w" table))
                  table)
  ;; Set buffer local variables
  (dolist (var '(one-key-buffer-menu-number
                 one-key-buffer-menu-names
                 one-key-buffer-menu-alists
                 one-key-buffer-special-keybindings
                 one-key-buffer-filter-regex
                 one-key-buffer-filtered-list
                 one-key-buffer-temp-action
                 one-key-buffer-miss-match-action
                 one-key-buffer-match-action
                 one-key-buffer-associated-window
                 one-key-buffer-dedicated-frame
                 one-key-buffer-this-name
                 one-key-buffer-full-list
                 one-key-buffer-this-list))
    (set (make-local-variable var) nil))
  ;; Set mode-line and header-line
  (setq mode-line-format one-key-mode-line-format
        header-line-format (one-key-header-line-format
                            (or one-key-buffer-menu-names "one-key")
                            one-key-buffer-menu-number)
        cursor-type nil
        one-key-mode-map (make-keymap)
        buffer-read-only nil)
  ;; Set keymap
  (set-char-table-range (second one-key-mode-map) t 'one-key-command)
  (define-key one-key-mode-map [t] 'one-key-command)
  (use-local-map one-key-mode-map)
  (local-unset-key (kbd "ESC")))

(define-derived-mode one-key-help-mode fundamental-mode "One-Key Help"
  "The major-mode for the one-key help buffer."
  :group 'one-key
  ;; Make sure brackets are not highlighted by syntax table
  :syntax-table (let ((table (make-syntax-table)))
                  (dolist (char '(40 41 60 62 91 93 123 125))
                    (modify-syntax-entry char "w" table))
                  table)
  ;; Set buffer local variables
  (dolist (var '(one-key-buffer-special-keybindings
                 one-key-buffer-associated-window
                 one-key-buffer-dedicated-frame))
    (set (make-local-variable var) nil))
  ;; Set mode-line and header-line
  (setq
   ;; mode-line-format one-key-mode-line-format
   ;;     header-line-format (one-key-header-line-format
   ;;                         (or one-key-buffer-menu-names "one-key")
   ;;                         one-key-buffer-menu-number)
   cursor-type nil
   one-key-help-mode-map (make-keymap)
   buffer-read-only nil)
  ;; Set keymap
  (set-char-table-range (second one-key-help-mode-map) t 'one-key-command)
  (define-key one-key-help-mode-map [t] 'one-key-command)
  (use-local-map one-key-help-mode-map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun one-key-show-help (special-keybindings)
  "Show information about `one-key-menu' special keybindings in the alist SPECIAL-KEYBINDINGS."
  (interactive)
  (let* ((maxkey (loop for elt in special-keybindings
                       maximize (1+ (length (car elt)))))
         (maxstr (loop for elt in special-keybindings
                       maximize (+ 3 maxkey (length (cadr elt)))))
         (width (/ (window-width) 2))
         (keystr (if (> maxstr width)
                     (mapconcat (lambda (elt) (format "%s\t: %s"
                                                      (one-key-remap-key-description (car elt))
                                                      (cadr elt)))
                                special-keybindings "\n")
                   (loop with colsize = (+ (/ (length special-keybindings) 2)
                                           (% (length special-keybindings) 2))
                         with finalstr
                         for n from 0 to (1- colsize)
                         for (key1 desc1) = (nth n special-keybindings)
                         for (key2 desc2) = (nth (+ n colsize) special-keybindings)
                         for key1a = (one-key-remap-key-description key1)
                         for key2a = (one-key-remap-key-description key2)
                         for keyspc1 = (make-string (- maxkey (length key1a)) ? )
                         for keyspc2 = (make-string (- maxkey (length key2a)) ? )
                         for str1 = (format "%s%s: %s" key1a keyspc1 desc1)
                         for str2 = (format "%s%s: %s" key2a keyspc2 desc2)
                         for spc = (make-string (- width (length str1)) ? ) do
                         (push (concat str1 spc (if key2a str2) "\n") finalstr)
                         finally return (mapconcat 'identity (nreverse finalstr) ""))))
         (onekeybuf (get-buffer one-key-buffer-name)))
    (with-help-window one-key-help-buffer-name
      (princ (concat "Press the highlighted key in the menu to perform the corresponding action written next to it.
The following special keys may also be used:\n"
                     keystr)))
    (with-current-buffer one-key-help-buffer-name (one-key-help-mode)
                         (setq one-key-buffer-special-keybindings special-keybindings
                               one-key-buffer-dedicated-frame
                               (if onekeybuf (with-current-buffer onekeybuf
                                               one-key-buffer-dedicated-frame))
                               one-key-buffer-associated-window
                               (if onekeybuf (with-current-buffer onekeybuf
                                               one-key-buffer-associated-window))))))


(defun one-key-show-item-help (key)
  "Show help for the item in the current menu that is associated with the key KEY."
  (let* ((this-list (nth one-key-buffer-menu-number one-key-buffer-menu-alists))
         (isref (symbolp this-list))
         (full-list (if isref (eval this-list) this-list))
         (item (one-key-get-menu-item key full-list))
         (tail (cdr item)))
    (if (listp tail)
        (if (commandp tail)
            (with-help-window one-key-help-buffer-name (princ tail))
          (let ((cmd (car tail)))
            (if (symbolp cmd)
                (if (commandp cmd)
                    (describe-function cmd)
                  (message "Unknown item!"))
              (with-help-window one-key-help-buffer-name (princ cmd)))))
      (if (commandp tail)
          (describe-function tail)
        (message "Unknown item!")))))

(defun one-key-prompt-to-add-menu-item nil
  "Prompt the user for item details and add it to the current one-key menu."
  (let* ((this-list (nth one-key-buffer-menu-number one-key-buffer-menu-alists))
         (isref (symbolp this-list))
         (full-list (if isref (eval this-list) this-list))
         (newkey (let ((key (read-event "Enter the key for the new item")))
                   (while (and (one-key-get-menu-item key full-list)
                               (not (y-or-n-p "That key is already used! Overwrite old item?")))
                     (setq key (read-event "Enter new key for the item")))
                   key))
         (desc (read-string "Item description: "))
         (contents (read-from-minibuffer "Command: " nil nil t)))
    (if isref
        (progn (add-to-list 'one-key-altered-menus (symbol-name this-list))
               (set this-list (one-key-add-menu-item newkey desc contents full-list)))
      (setq this-list (one-key-add-menu-item newkey desc contents full-list))))
  (one-key-update-buffer-contents))

(defun one-key-swap-menu-items nil
  "Prompt user for a pair of items in the current one-key menu and swap the corresponding keys."
  (let* ((this-list (nth one-key-buffer-menu-number one-key-buffer-menu-alists))
         (isref (symbolp this-list))
         (full-list (if isref (eval this-list) this-list))
         (keya (read-event "Press key for first item"))
         (keyastr (one-key-key-description keya))
         (itema (one-key-get-menu-item keyastr full-list))
         (keyb (read-event "Press key for second item"))
         (keybstr (one-key-key-description keyb))
         (itemb (one-key-get-menu-item keybstr full-list)))
    (if (not (and itema itemb)) (message "Invalid key!")
      (setf (caar itema) keybstr (caar itemb) keyastr))
    (if isref (add-to-list 'one-key-altered-menus (symbol-name this-list))))
  (one-key-update-buffer-contents))

(defun one-key-delete-menu-item nil
  "Prompt the user for an item in the current menu to delete from the `one-key' menu."
  (let* ((this-list (nth one-key-buffer-menu-number one-key-buffer-menu-alists))
         (isref (symbolp this-list))
         (full-list (if isref (eval this-list) this-list))
         (key (read-event "Press the key of the item you want to delete"))
         (item (one-key-get-menu-item key full-list)))
    (if (and item (y-or-n-p (format "Delete item \"%s\"?" (cdar item))))
        (if isref (set this-list (delete item full-list))
          (setq this-list (delete item full-list))))
    (if isref (add-to-list 'one-key-altered-menus (symbol-name this-list))))
  (one-key-update-buffer-contents))

(defun one-key-get-item-colour (item &optional fg rettype)
  "Return the background colour of menu item ITEM. If FG is non-nil return the foreground colour instead.
If RETTYPE is one of the following symbols: 'hex, 'hsv or 'rgb, then the colour will be returned in the associated format
 (a hex string, list of hsv values or list of rgb values).
By default the colour will be returned in hex string format."
  (let* ((descface (get-text-property 0 'face (cdar item)))
         (type (if fg :foreground :background))
         (colour (or (and (facep descface)
                          (not (equal (face-attribute descface type) 'unspecified))
                          (face-attribute descface type))
                     (plist-get descface type)))
         (colour2 (or colour (if fg one-key-item-foreground-colour
                               (cdr (assq 'background-color (frame-parameters)))))))
    (case rettype
      (hex (hexrgb-color-name-to-hex colour2))
      (hsv (hexrgb-hex-to-hsv colour2))
      (rgb (hexrgb-hex-to-rgb colour2))
      (t (hexrgb-color-name-to-hex colour2)))))

(defun one-key-copy/kill-items nil
  "Prompt for a colour, copy all items with that colour from the current menu, and put them in `one-key-copied-items'."
  (let* ((this-list (nth one-key-buffer-menu-number one-key-buffer-menu-alists))
         (isref (symbolp this-list))
         (full-list (if isref (eval this-list) this-list))
         (filtered-list one-key-buffer-filtered-list)
         (key (read-event "Press the key of an item in the colour group to copy: "))
         (item (one-key-get-menu-item key filtered-list))
         (kill (y-or-n-p "Delete items from current menu?")))
    (if item
        (destructuring-bind (h1 s1 v1) (one-key-get-item-colour item nil 'hsv)
          (setq one-key-copied-items
                (loop for item2 in filtered-list
                      for (h2 s2 v2) = (one-key-get-item-colour item2 nil 'hsv)
                      for huediff = (abs (- h1 h2))
                      for satdiff = (abs (- s1 s2))
                      if (and (< huediff 0.001) (< satdiff 0.001)) do
                      (if kill
                          (if isref (set this-list (delete item2 full-list))
                            (setq this-list (delete item2 full-list))))
                      and collect item2))
          (if isref (add-to-list 'one-key-altered-menus (symbol-name this-list))))))
    (one-key-update-buffer-contents))

(defun one-key-yank-items nil
  "Yank menu items in `one-key-copied-items' into current menu."
  (let* ((this-list (nth one-key-buffer-menu-number one-key-buffer-menu-alists))
         (isref (symbolp this-list))
         (full-list (if isref (eval this-list) this-list))
         (usedkeys (mapcar 'caar full-list))
         (pair (loop for ((key . desc) . rest) in one-key-copied-items
                     for newkey = (one-key-generate-key desc usedkeys nil key)
                     for newitem = (cons (cons newkey desc) rest)
                     do (add-to-list 'usedkeys newkey)
                     collect newitem into newitems
                     collect desc into descs
                     finally return (cons newitems descs)))
         (newitems (car pair))
         (descs (cdr pair)))
    (if (not isref) (setq this-list (append full-list newitems))
      (set this-list (append full-list newitems))
      (add-to-list 'one-key-altered-menus (symbol-name this-list)))
    (if one-key-buffer-filter-regex
        (setq one-key-buffer-filter-regex (concat (regexp-opt descs) "\\|" one-key-buffer-filter-regex))))
  (one-key-update-buffer-contents))

(defun one-key-edit-menu-item nil
  "Prompt user for the key of an item in the current menu to edit."
  (let* ((this-list (nth one-key-buffer-menu-number one-key-buffer-menu-alists))
         (isref (symbolp this-list))
         (full-list (if isref (eval this-list) this-list))
         (oldkey (read-event "Press the key of the item you want to edit"))
         (item (one-key-get-menu-item oldkey full-list))
         (newkey (let ((key (read-event "Enter new key for the item")))
                   (while (and (one-key-get-menu-item key full-list)
                               (not (eq key oldkey))
                               (not (y-or-n-p "That key is already used! Use it anyway?")))
                     (setq key (read-event "Enter new key for the item")))
                   key))
         (desc (read-string "Item description: " (cdar item) nil nil))
         (oldcontents (cdr item))
         (contents (read-from-minibuffer "Item contents: " (format "%S" oldcontents) nil t)))
    (setf (caar item) (one-key-key-description newkey))
    (setf (cdar item) desc)
    (setf (cdr item) contents)
    (if isref (add-to-list 'one-key-altered-menus (symbol-name this-list))))
  (one-key-update-buffer-contents))

(defun one-key-colourize-string (colour string)
  "Change background colour of STRING to COLOUR, and foreground colour to `one-key-item-foreground-colour'."
  (propertize string 'face (list :background colour :foreground one-key-item-foreground-colour)))

(defun one-key-highlight-matching-items (colour pred)
  "Highlight the items in the current menu that satisfy the predicate function PRED with colour COLOUR.
The predicate function should take a single item from the menu list as it's only argument.
If COLOUR is \"\" then all highlighting (and more generally any text properties) are removed from the item."
  (let* ((this-list (nth one-key-buffer-menu-number one-key-buffer-menu-alists))
         (isref (symbolp this-list))
         (full-list (if isref (eval this-list) this-list)))
    (loop for item in-ref full-list
          for str = (cdar item)
          if (funcall pred item) do
          (if (equal colour "")
              (setf (cdar item) (substring-no-properties str))
            (setf (cdar item) (one-key-colourize-string colour str))))
    (if isref (add-to-list 'one-key-altered-menus (symbol-name this-list))))
  (one-key-update-buffer-contents))

(defun one-key-save-menu (name menu-list)
  "Save a one-key menu to the file `one-key-menus-save-file'.
NAME is the name of the menu, and MENU-LIST is either a one-key menu list, or a symbol whose value is such a menu."
  (let* ((isref (symbolp menu-list))
         (varname (if isref (symbol-name menu-list)
                    (concat "one-key-menu-" name "-alist")))
         (full-list (if isref (eval menu-list) menu-list))
         (file one-key-menus-save-file)
         (buf (get-file-buffer file)))
    (if file
        (if (file-writable-p file)
            (with-current-buffer (find-file-noselect file)
              (goto-char (point-min))
              (if (not (search-forward (concat "(setq " varname) nil t))
                  (goto-char (point-max))
                (beginning-of-line)
                (mark-sexp)
                (kill-region (point) (marker-position (mark-marker)))
                (deactivate-mark))
              (insert (concat "(setq " varname "\n      '"
                              (replace-regexp-in-string
                               ") ((" ")\n        ((" (eval `(prin1-to-string full-list))) ")"))
              (save-buffer)
              (if (not buf) (kill-buffer (get-file-buffer file))))
          (message "Can't write to file %s" file))
      (message "`one-key-menus-save-file' not set" file))))

(defun* one-key-sort-items-by-next-method (&optional prev (multisort t))
  "Sort the items in the current one-key menu, and it's associated menus by the next sort method associated with that menu.

If PREV is non-nil then the previous method in SORT-METHODS will be used instead.
If MULTISORT is nil (default is t) then the associated menus will not be sorted, only the current menu.

The list of sort methods for the current menu are stored in the 5th element of the element of `one-key-types-of-menu' associated
with this menu. The current sort index is stored for this menu is stored in `one-key-sort-method-indices-alist', or 0 otherwise."
  (let* ((name (nth one-key-buffer-menu-number one-key-buffer-menu-names))
         (indices (if multisort (one-key-get-associated-menu-indices
                                 one-key-buffer-menu-number one-key-buffer-menu-names)
                    (list one-key-buffer-menu-number)))
         (onemenu (= (length indices) 1))
         (items (mapcan 'one-key-eval-if-symbol
                        (one-key-list-subset indices one-key-buffer-menu-alists)))
         (sortmethods (or (one-key-eval-if-symbol
                           (nth 5 (one-key-get-menu-type name)))
                          one-key-default-sort-method-alist))
         (numsortmethods (length sortmethods))
         (sortindex (let* ((pair (assoc name one-key-sort-method-indices-alist))
                           (oldindex (or (cdr pair) 0))
                           (newindex (mod (if prev (1- oldindex) (1+ oldindex)) numsortmethods)))
                      (if pair (setf (cdr pair) newindex)
                        (one-key-add-to-alist 'one-key-sort-method-indices-alist (cons name newindex)))
                      newindex))
         (sortmethod (nth sortindex sortmethods))
         (sorteditems (sort items (cdr sortmethod)))
         (keys (if (not onemenu) (mapcar (lambda (x) (caar x)) sorteditems)))
         (descs (if (not onemenu) (mapcar (lambda (x) (cdar x)) sorteditems)))
         (commands (if (not onemenu) (mapcar 'cdr sorteditems)))
         (sortedlists (if onemenu sorteditems (one-key-create-menu-lists commands descs keys nil :invalidkeys nil)))
         (basename (replace-regexp-in-string " ([0-9]+)$" "" name))
         (newnames (if onemenu name (one-key-append-numbers-to-menu-name basename (length sortedlists)))))
    (if onemenu (setf (nth one-key-buffer-menu-number one-key-buffer-menu-alists) sortedlists)
      (one-key-delete-menus indices)
      (one-key-add-menus newnames sortedlists))
    (setq one-key-displayed-sort-method (car sortmethod)))
  (one-key-update-buffer-contents))

(defun one-key-reverse-item-order nil
  "Reverse the order of items in the current one-key menu.
This function must be called within the context of the one-key buffer to work."
  (let* ((this-list (nth one-key-buffer-menu-number one-key-buffer-menu-alists))
         (isref (symbolp this-list))
         (full-list (if isref (eval this-list) this-list))
         (reversed-list (reverse full-list)))
    (if (not isref)
        (setf (nth one-key-buffer-menu-number one-key-buffer-menu-alists) reversed-list)
      (set this-list reversed-list)
      (add-to-list 'one-key-altered-menus (symbol-name this-list))))
  (one-key-update-buffer-contents))

(defun one-key-get-menu-type (name)
  "Return the element of `one-key-types-of-menu' corresponding to menu with name NAME, or nil if none exists."
  (if name (find-if (lambda (x)
                      (let ((one (first x))
                            (two (second x)))
                        (or (equal one name) 
                            (and (functionp two)
                                 (funcall two name)))))
                    one-key-types-of-menu)))

(defun* one-key-get-menus-for-type (name &optional (remapkeys t))
  "Given the name NAME of an existing menu or menu type in `one-key-types-of-menu', return associated names and menu alists.
If no such menu or menu type exists, return nil.
The optional argument REMAPKEYS is t by default and indicates whether or not to remap the keys in the menu using
the `one-key-remap-invalid-keys' function."
  (let* ((pair (if name (let* ((listname (concat "one-key-menu-" name "-alist"))
                               (type (one-key-get-menu-type name))
                               (func (or (third type)
                                         (and (not type)
                                              (loop for sym being the symbols
                                                    for symname = (symbol-name sym)
                                                    when (equal listname symname)
                                                    return (cons name sym))))))
                          (if (functionp func) (funcall func name) func))))
         (onep (stringp (car pair)))
         (names (car pair))
         (menus (if remapkeys
                    (if onep (one-key-remap-invalid-keys (cdr pair))
                      (mapcar 'one-key-remap-invalid-keys (cdr pair)))
                  (cdr pair))))
    (cons names menus)))

(defun one-key-prompt-for-menu nil
  "Prompt the user for a `one-key' menu type, and return menu name(s) and menu alist(s)."
  (let* ((alltypes (remq nil (mapcar 'car one-key-types-of-menu)))
         (type (if (featurep 'ido)
                   (ido-completing-read "Menu type: " alltypes)
                 (completing-read "Menu type: " alltypes))))
    (one-key-get-menus-for-type type)))

(defun one-key-add-menus (&optional newnames newlists)
  "Add a menu/menus to the current list of menus in the `one-key' menu function call.
If optional args NEWNAMES and NEWLISTS are supplied they should be a list of menu names and corresponding menu alists
to add. Otherwise the user will be prompted for a menu type, and the menus will be created using the functions associated
with that type.
This function only works when called within the context of the one-key buffer since it depends on buffer local variables."
  (let* ((both (if (and newnames newlists)
                   (cons newnames newlists)
                 (one-key-prompt-for-menu)))
         (newnames (car both))
         (newlists (cdr both))
         (multi (listp newnames)))
    (with-current-buffer one-key-buffer-name
      (let* ((listlen (length one-key-buffer-menu-alists)))
        (unless (= listlen (length one-key-buffer-menu-names))
          (error "Number of menu names doesn't match number of menus"))
        (setq one-key-buffer-menu-names
              (concatenate 'list
                           (subseq one-key-buffer-menu-names 0 (1+ one-key-buffer-menu-number))
                           (if (and multi newnames) newnames (list newnames))
                           (subseq one-key-buffer-menu-names (1+ one-key-buffer-menu-number) listlen))
              one-key-buffer-menu-alists
              (concatenate 'list
                           (subseq one-key-buffer-menu-alists 0 (1+ one-key-buffer-menu-number))
                           (if (and multi newlists) newlists (list newlists))
                           (subseq one-key-buffer-menu-alists (1+ one-key-buffer-menu-number) listlen))
              one-key-buffer-menu-number (1+ one-key-buffer-menu-number)))))
  (one-key-update-buffer-contents))

(defun one-key-get-indices-of-matches (regexp names)
  "Return list of indices of elements of NAMES (a list of strings) that match REGEXP (a regular expression)."
  (loop for i from 0 to (1- (length names))
        for name = (nth i names)
        if (string-match regexp name)
        collect i))

(defun one-key-get-associated-menu-indices (menunumber menunames)
  "Return indices of the names in MENUNAMES that are associated with the menu with index MENUNUMBER.
Associated menus are adjacent menus with the same base-name but different numbers appended to the end of the name (in brackets)."
  (let* ((name (nth menunumber menunames))
         (nummenus (length menunames))
         (basename (replace-regexp-in-string " ([0-9]+)$" "" name))
         (num (if (string-match " (\\([0-9]+\\))$" name)
                  (string-to-number (match-string 1 name)))))
    (if (not num) (list menunumber)
      (loop for i from 1 to nummenus
            for index = (mod (+ i (- menunumber num)) nummenus)
            for nextname = (nth index menunames)
            if (string= (concat basename " (" (number-to-string i) ")") nextname)
            collect index))))

(defmacro one-key-list-subset (indices list)
  "Return elements of LIST corresponding to INDICES."
  `(mapcar (lambda (i) (nth i ,list)) ,indices))

(defun one-key-get-menus (&optional menus)
  "Return menu names and menu alists from the currently loaded one-key menus.
A cons cell whose car is a list of menu names, and whose cdr is a list of corresponding menu alists is returned.
By default menus associated with the current menu are returned.
Associated menus are adjacent menus with the same base-name but different numbers appended to the end of the name (in brackets).
If the optional argument MENUS is supplied it should either be a number (the index of the menu to be returned),
a list of numbers (indices of menus), or a string (regular expression matching menu names).

This function only works when called within the context of the one-key buffer since it depends on buffer local variables."
  (let* ((indices (cond ((not menus) (one-key-get-associated-menu-indices
                                      one-key-buffer-menu-number one-key-buffer-menu-names))
                        ((stringp menus) (one-key-get-indices-of-matches
                                          menus one-key-buffer-menu-names))
                        ((listp menus) menus)
                        ((numberp menus) (list menus)))))
    (cons (one-key-list-subset indices one-key-buffer-menu-names)
          (one-key-list-subset indices one-key-buffer-menu-alists))))
  
(defun* one-key-delete-menus (&optional (menus one-key-buffer-menu-number))
  "Remove a menu(s) from the currently loaded one-key menus. By default the current menu is deleted.
If the optional argument MENUS is supplied it should either be a number (the index of the menu to be deleted),
a list of numbers (the indices of the menus to be deleted), or a string (a regular expression matching the names
of the menus to be deleted).
If there is currently only one loaded menu, the one-key window will be closed.
The function returns the number of menus deleted.

This function only works when called within the context of the one-key buffer since it depends on buffer local variables."
  (with-current-buffer one-key-buffer-name
    (let* ((listlen (length one-key-buffer-menu-alists))
           (indices (if (stringp menus)
                        (one-key-get-indices-of-matches menus one-key-buffer-menu-names)
                      (if (listp menus) menus (list menus)))))
      (unless (= listlen (length one-key-buffer-menu-names))
        (error "Number of menu names doesn't match number of menus"))
      (if (= listlen 1)
          (progn (one-key-set-window-state 'close) 1)
        (dolist (index indices)
          (setq one-key-buffer-menu-names
                (concatenate 'list
                             (subseq one-key-buffer-menu-names 0 index)
                             (subseq one-key-buffer-menu-names (1+ index) listlen))
                one-key-buffer-menu-alists
                (concatenate 'list
                             (subseq one-key-buffer-menu-alists 0 index)
                             (subseq one-key-buffer-menu-alists (1+ index) listlen))
                one-key-buffer-menu-number (min index (- listlen 2)))))
      (one-key-update-buffer-contents)
      (length indices))))

(defun one-key-delete-associated-menus (&optional (menunum one-key-buffer-menu-number))
  "Remove all menus (from the currently loaded menus) that are associated with the current menu.
Associated menus are those with the same base-name but different numbers appended to the end of the name (in brackets).
If MENUNUM is supplied it should be the index into `one-key-buffer-menu-alists' of a menu to be deleted.
Returns the number of menus deleted.

This function only works when called within the context of the one-key buffer since it depends on buffer local variables."
  (one-key-delete-menus (one-key-get-associated-menu-indices menunum one-key-buffer-menu-names)))

(defun one-key-open-menus (names &optional menu-number)
  "Invoke `one-key-menu' with names and corresponding menu-alists.
NAMES should be the name of a single `one-key' menu or menu type, or a list of such names.
If called interactively a single name will be prompted for."
  (let* ((names (if (stringp names) (list names) names))
         (pairs (remq nil (mapcar 'one-key-get-menus-for-type names)))
         (names (mapcan (lambda (x) (let ((y (car x))) (if (stringp y) (list y) y))) pairs))
         (alists (mapcan (lambda (x) (let ((a (car x)) (b (cdr x)))
                                       (if (stringp a) (list b) b))) pairs)))
    (if (and names alists)
        (one-key-menu names alists menu-number)
      (message "Invalid menu names!"))))

(defun one-key-open-menu-set (menuset &optional menu-number)
  "Open `one-key' menus defined by `one-key' menu set MENUSET.
MENUSET should be the car of an element of `one-key-sets-of-menus-alist'.
If called interactively, MENUSET will be prompted for."
  (interactive (list (if (featurep 'ido)
                         (ido-completing-read "Menu set: " (mapcar 'car one-key-sets-of-menus-alist))
                       (completing-read "Menu set: " (mapcar 'car one-key-sets-of-menus-alist)))))
  (let* ((names (assoc-default menuset one-key-sets-of-menus-alist)))
    (if names (one-key-open-menus names menu-number)
      (message "Invalid menu set name!"))))

(defun one-key-rebuild-menu nil
  "Rebuild the currently displayed one-key menu according to it's name.
This should only be used with menus that can be rebuilt using `one-key-get-menus-for-type'."
  (let* ((this-list (nth one-key-buffer-menu-number one-key-buffer-menu-alists))
         (this-name (nth one-key-buffer-menu-number one-key-buffer-menu-names))
         (isref (symbolp this-list))
         (full-list (if isref (eval this-list) this-list)))
    (if isref
      (let ((newlist (cdr (one-key-get-menus-for-type this-name))))
        (if newlist (unintern this-list)
          (if (get-buffer-window (help-buffer)) (kill-buffer (help-buffer)))
          (set this-list newlist)
          (one-key-update-buffer-contents))))))

(defun one-key-open-default-menu-set nil
  "Open the menu set defined by `one-key-default-menu-set'."
  (interactive)
  (one-key-open-menu-set one-key-default-menu-set
                         (if one-key-persistent-menu-number
                             one-key-default-menu-number nil)))

(defun one-key-open-associated-menu-set nil
  "Open the menu set associated with the current buffer according to `one-key-associations-for-menu-sets'.
If no menu set matches then open `one-key-default-menu-set'."
  (interactive)
  (let* ((allmenusets (mapcar 'car one-key-sets-of-menus-alist))
         (assocmenu (cdr (assoc-if (lambda (item) (or (eql item major-mode)
                                                      (and (stringp item)
                                                           (string-match item (buffer-name)))))
                                   one-key-associations-for-menu-sets)))
         (menuset (if (member assocmenu allmenusets) assocmenu one-key-default-menu-set)))
    (one-key-open-menu-set menuset
                           (if one-key-persistent-menu-number
                               one-key-default-menu-number nil))))

(defun one-key-highlight (msg msg-regexp msg-face)
  "Highlight text in string `MSG' that matches regular expression `MSG-REGEXP' with face `MSG-FACE'."
  (with-temp-buffer
    (insert msg)
    (goto-char (point-min))
    (while (re-search-forward msg-regexp nil t)
      (add-text-properties (match-beginning 0)
                           (match-end 0)
                           msg-face))
    (buffer-string)))

(defun one-key-highlight-menu (keystroke names menu-number &optional title-string)
  "Highlight items in KEYSTROKE (an alist of menu items), and return contents for insertion in *One-Key* buffer.
Also create header-line from NAMES (a list of menu names), highlighting the MENU-NUMBER'th name in that list.
MENU-NUMBER should be the number of the currently selected menu in the NAMES list, or nil if NAMES contains
a single menu name.
The optional arg TITLE-STRING is a title to place above the menu items. By default this title is obtained automatically
from the associated menu type in `one-key-types-of-menu' or using `one-key-default-title-func' if that doesn't exist."
  (let* ((name (if menu-number (nth menu-number names) names))
         (title-func (or (fourth (one-key-get-menu-type name)) one-key-default-title-func))
         (title-string2 (or title-string (and title-func (funcall title-func))))
         (infoline (if (> (length title-string2) 0)
                       (one-key-highlight (if (equal (substring title-string2 -1) "\n") title-string2
                                            (concat title-string2 "\n"))
                                          "\\(<[^<>]*>\\|'[^']*'\\)" '(face one-key-name))))
         (keystrokelist (one-key-highlight keystroke "\\[\\([^\\[\\]\\)*?\\]" '(face one-key-keystroke))))
    (setq header-line-format (one-key-header-line-format names menu-number)
          mode-line-format one-key-mode-line-format)
    (concat infoline keystrokelist)))

(defun one-key-command nil
  "Invoke command associated with last keypress in one-key buffer."
  (interactive)
  (let ((helpbufp (string= (buffer-name) one-key-help-buffer-name))
        (key (one-key-key-description (this-command-keys)))
        matchitem postaction)
    (with-current-buffer (or (get-buffer one-key-buffer-name)
                             (get-buffer one-key-help-buffer-name))
      (cond
       ;; Ignore mouse events.
       ((and (string-match "mouse" key)) t)
       ;; Make sure frame switching works.
       ((and (string= "<switch-frame>" key)) (select-frame (previous-frame)))
       ;; If this is the help buffer then there's no one-key buffer, so close the help buffer too.
       ((string= (buffer-name) one-key-help-buffer-name)
        (setq postaction 'close))
       ;; Handle special keys
       ((setq matchitem (assoc* key one-key-buffer-special-keybindings
                                :test (lambda (x y) (equal x (one-key-remap-key-description y)))))
        (funcall (caddr matchitem))
        (setq postaction one-key-buffer-temp-action))
       ;; Handle keystrokes matching menu items unless called from the help buffer.
       ((and (not helpbufp)
             (setq matchitem (assoc* key one-key-buffer-filtered-list
                                     :test (lambda (x y) (equal x (one-key-remap-key-description (car y)))))))
        (let* ((desc (cdar matchitem))
               (rest (cdr matchitem))
               (command (if (commandp rest) rest
                          (if (one-key-list-longer-than-1-p rest)
                              (car rest)
                            (lambda nil (interactive) (message "Invalid command %S" rest))))))
          ;; Update key usage statistics if necessary
          (unless (not one-key-auto-brighten-used-keys)
            (one-key-menu-increment-key-usage matchitem)
            (let* ((thislist (nth one-key-buffer-menu-number one-key-buffer-menu-alists))
                   (issymbol (symbolp thislist)))
              (if issymbol (add-to-list 'one-key-altered-menus (symbol-name thislist)))))
          ;; Execute the menu command in the associated window, (and get action to perform afterwards).
          (with-selected-window one-key-buffer-associated-window (call-interactively command))
          (setq postaction (or one-key-buffer-temp-action
                               one-key-buffer-match-action))))
       ;; Handle all other (miss-match) keys unless called from the help buffer.
       ((not helpbufp)
        (setq postaction one-key-buffer-miss-match-action)))
      ;; Set one-key window state appropriately according to value of postaction var (nil by default).
      (cond ((functionp postaction)
             (funcall postaction))
            ((eq postaction 'execute)
             (one-key-execute-binding-command key))
            ((eq postaction 'executeclose)
             (one-key-execute-binding-command key)
             (one-key-set-window-state 'close))
            (t (one-key-set-window-state postaction)))
      ;; Reset one-key-buffer-temp-action.
      (with-current-buffer one-key-buffer-name (setq one-key-buffer-temp-action nil)))))

(defun* one-key-update-buffer-contents (&optional title-string (buf one-key-buffer-name))
  "Update the contents of the one-key menu buffer.
The optional argument TITLE-STRING is a title to insert above the menu items. By default this string will be obtained
automatically from the associated menu type in `one-key-types-of-menu' or using `one-key-default-title-func' if that
doesn't exist."
  (with-current-buffer buf
    (cond ((not one-key-buffer-menu-number)
           (error "Buffer local variable one-key-buffer-menu-number is nil"))
          ((not one-key-buffer-menu-names)
           (error "Buffer local variable one-key-buffer-menu-names is nil"))
          ((not one-key-buffer-menu-alists)
           (error "Buffer local variable one-key-buffer-menu-alists is nil")))
    ;; Fill buffer with menu items.
    (setq one-key-buffer-this-list (nth one-key-buffer-menu-number one-key-buffer-menu-alists)
          one-key-buffer-full-list (one-key-eval-if-symbol one-key-buffer-this-list)
          one-key-buffer-this-name (nth one-key-buffer-menu-number one-key-buffer-menu-names)
          one-key-buffer-filtered-list
          (if (stringp one-key-buffer-filter-regex)
              (remove-if-not
               (lambda (elt) (string-match one-key-buffer-filter-regex (cdar elt)))
               (remove nil one-key-buffer-full-list))
            (remove nil one-key-buffer-full-list))
          one-key-buffer-special-keybindings
          (or (one-key-get-special-key-contents
               (one-key-eval-if-symbol
                (or (fifth (one-key-get-menu-type one-key-buffer-this-name))
                    one-key-default-special-keybindings)))))
    (erase-buffer)
    (goto-char (point-min))
    (insert (one-key-highlight-menu
             (one-key-menu-format one-key-buffer-filtered-list)
             one-key-buffer-menu-names one-key-buffer-menu-number title-string)))
  (one-key-reposition-window-contents))

(defun* one-key-menu (&optional menu-names menu-alists menu-number
                                &key special-keybinding-symbols title-string
                                (associated-window (selected-window))
                                (miss-match-action 'close) (match-action 'close))
  "Function to open `one-key' menu of commands. The commands are executed by pressing the associated keys.
MENU-NAMES is the name of the menu as displayed in the menu window, or a list of names corresponding to different menu
lists in MENU-ALISTS.
MENU-ALISTS is either a list of menu items, a list of such lists or a symbol whose value is a list or list of lists.
Each item in a menu list is of the form: ((key . description) . command).
MENU-NUMBER should be an index (starting at 0) indicating which list to display initially (default is 0).
If either of MENU-NAMES or MENU-ALISTS is nil then the values that are already stored in the one-key buffer (as buffer local
variables) will be used, or an error will be flagged if no such values exist.

The user can switch between the menu lists by pressing the appropriate keys in `one-key-default-special-keybindings'.
If SPECIAL-KEYBINDING-SYMBOLS is non-nil then it should be a list of symbols corresponding to items in `one-key-special-keybindings',
 and defines the special keys to use for this menu. Otherwise the special keys will be determined from the menu type,
or `one-key-default-special-keybindings' will be used.
TITLE-STRING is a string to display above the menu items. If TITLE-STRING is nil then the title string will be obtained
from the fourth element of the associated menu type in `one-key-types-of-menu' or using `one-key-default-title-func' if that
doesn't exist.

By default the one-key buffer will be associated with the currently selected window, and all menu commands will be executed
in that window. You can change the associated window by setting the ASSOCIATED-WINDOW arg to any other window.
If ASSOCIATED-WINDOW is explicitly set to nil then the window that was last used with one-key will be used (if it exists).

The MATCH-ACTION and MISS-MATCH-ACTION arguments indicate what to do after a matching (menu item)/non-matching key is pressed
respectively. See `one-key-buffer-match-action' and `one-key-buffer-miss-match-action' for the different values these args
can take."
  (let* ((buf (or (get-buffer one-key-buffer-name)
                  (generate-new-buffer one-key-buffer-name)))
         (menu-number (or menu-number 0)))
    ;; Setup the one-key buffer
    (set-buffer buf)
    (if (not (equal major-mode 'one-key-mode)) (one-key-mode))
    (let* ((onemenup (and (listp menu-alists)
                          (listp (car menu-alists))
                          (listp (caar menu-alists))
                          (stringp (caaar menu-alists))))
           (menu-alists2 (or (if onemenup (list menu-alists) menu-alists)
                             one-key-buffer-menu-alists
                             (error "Both menu-alists and one-key-buffer-menu-alists are nil")))
           (menu-names2 (or (if (stringp menu-names) (list menu-names) menu-names)
                            one-key-buffer-menu-names
                            (error "Both menu-names and one-key-buffer-menu-names are nil")))
           (menu-number2 (or menu-number one-key-buffer-menu-number
                             (error "Both menu-number and one-key-buffer-menu-number are nil")))
           (menu-number3 (if onemenup 0 ; make sure menu number is set properly
                           (max (min menu-number2 (1- (length menu-alists2))) 0)))
           (this-name (nth menu-number3 menu-names2))
           (special-keybindings (or (if special-keybinding-symbols
                                        (one-key-get-special-key-contents special-keybinding-symbols))
                                    (if menu-alists
                                        (one-key-get-special-key-contents
                                         (one-key-eval-if-symbol
                                          (or (fifth (one-key-get-menu-type this-name))
                                              one-key-default-special-keybindings))))
                                    one-key-buffer-special-keybindings))
           (associated-window2 (or associated-window
                                  one-key-buffer-associated-window
                                  (error "Both associated-window and one-key-buffer-associated-window are nil")))
           (miss-match-action2 (if menu-alists miss-match-action
                                 one-key-buffer-miss-match-action))
           (match-action2 (if menu-alists match-action
                            one-key-buffer-match-action)))
      (setq one-key-buffer-menu-number menu-number3
            one-key-buffer-menu-names menu-names2
            one-key-buffer-menu-alists menu-alists2
            one-key-buffer-special-keybindings special-keybindings
            one-key-buffer-associated-window associated-window2
            one-key-buffer-miss-match-action miss-match-action2
            one-key-buffer-match-action match-action2
            one-key-window-toggle-pos 0)
      (one-key-update-buffer-contents title-string))
    ;; Open the one-key window
    (one-key-set-window-state (car one-key-window-toggle-sequence))))

(defun one-key-execute-binding-command (key)
  "Execute the command bound to KEY (a string description of a key), unless this command is `keyboard-quit'.
If KEY contains shift, and there is no command bound to that key, then the same key binding with shift removed
will be tried (in accordance with normal emacs behaviour)."
  (let* ((rawkey (elt (eval (read-kbd-macro key)) 0))
         (mods (event-modifiers rawkey))
         (basic (event-basic-type rawkey))
         (func (key-binding (vector rawkey)))
         (keynoshift (append (remq 'shift mods) (list basic)))
         (func2 (or func
                    (key-binding (vector (event-convert-list keynoshift))))))
    (when (and (not (eq func2 'keyboard-quit))
               (functionp func2))
      (setq last-command-event last-input-event)
      (call-interactively func2))))

(defun one-key-menu-window-exist-p nil
  "Return non-nil if one-key menu window exists, otherwise return nil."
  (let ((onekeybuf (get-buffer one-key-buffer-name)))
    (and onekeybuf (window-live-p (get-buffer-window onekeybuf)))))

(defun one-key-reposition-window-contents nil
  "Scroll the one-key buffer contents so that the top of the buffer is shown at the top of the window."
  (let ((onekeywin (get-buffer-window one-key-buffer-name t)))
    (if onekeywin (with-selected-window onekeywin
                    (goto-char (point-min)) (recenter 0)))))

;; WARNING: spaghetti code
;; FIXME: look into using dedicated window for dedicated frame, and what about quit-window instead of delete-window ?
(defun one-key-set-window-state (state &optional noselect)
  "Set the one-key window into state STATE (a number or symbol).
For the possible values of state see `one-key-window-toggle-sequence'.
The one-key window will be selected after calling this function unless optional argument NOSELECT is non-nil."
  (let* ((onekeybuf (get-buffer one-key-buffer-name))
         (onekeywin (get-buffer-window one-key-buffer-name t))
         (onekeyframe (if onekeywin (window-frame onekeywin)))
         (helpbuf (get-buffer one-key-help-buffer-name))
         (helpwin (if helpbuf (get-buffer-window helpbuf t)))
         (helpframe (if helpwin (window-frame helpwin)))
         ;; The number of lines used by the buffer.
         (buflines (with-current-buffer (or onekeybuf (current-buffer))
                     (count-lines (point-min) (point-max))))
         ;; The number of lines currently used by the one-key window
         (winlines (if onekeywin (window-total-height onekeywin)))
         ;; Estimate maximum frame size in lines.
         (maxlines (if one-key-buffer-dedicated-frame
                       (if (display-graphic-p)
                           (- (/ (display-pixel-height) (frame-char-height))
                              (frame-parameter nil 'menu-bar-lines)
                              (* 2 (frame-parameter nil 'tool-bar-lines))
                              1)
                         (display-pixel-height))
                     (frame-parameter nil 'height)))
         ;; The number of columns to be used for the width
         (cols (frame-parameter nil 'width))
         ;; Make sure one-key-buffer-dedicated-frame is set correctly
         ;; (the frame may have been deleted, or created outside of this function).
         (dedicatedframe (if onekeybuf
                             (with-current-buffer onekeybuf
                               (if (not onekeyframe) (setq one-key-buffer-dedicated-frame nil)
                                 ;; If one-key is the only buffer displayed in it's frame, and there are other
                                 ;; frames then assume it has a dedicated frame
                                 (if (and (= (length (window-list onekeyframe)) 1)
                                          (> (length (frame-list)) 1))
                                     (setq one-key-buffer-dedicated-frame onekeyframe)))
                               one-key-buffer-dedicated-frame)))
         ;; Use following let bindings instead of symbol-macrolets to allow debugging.
         (resizeframe '(progn (set-frame-height dedicatedframe newlines)
                              (unless noselect
                                (select-frame-set-input-focus dedicatedframe)
                                (raise-frame dedicatedframe))
                              (one-key-update-buffer-contents)))
         (resizewindow '(let* ((win (or onekeywin (display-buffer onekeybuf)))
                               (frame (window-frame win)))
                          (fit-window-to-buffer win newlines)
                          (unless noselect (select-frame-set-input-focus frame)
                                  (select-window win))
                          (one-key-update-buffer-contents))))
    (cond ((not (or onekeybuf helpbuf)) (error "No one-key buffer found"))
          ((and (not onekeybuf) (equal (current-buffer) helpbuf))
           (kill-buffer helpbuf))
          ((integerp state)
           (let ((newlines (max (min state (if dedicatedframe maxlines (1- maxlines))
                                     (if dedicatedframe
                                         (+ buflines 3) (+ buflines 2)))
                                5))
                 (helpp (windowp helpwin)))
             (if helpp (one-key-set-window-state 'hidehelp))             
             (if onekeybuf
                 (if dedicatedframe (eval resizeframe) (eval resizewindow)))
             (if helpp (one-key-set-window-state 'showhelp))))
          ((floatp state)
           (let ((newlines (max (min (round (* (max (min state 1.0) 0.0)
                                               (if dedicatedframe maxlines (1- maxlines))))
                                     (if dedicatedframe
                                         (+ buflines 3) (+ buflines 2))) 5))
                 (helpp (windowp helpwin)))
             (if helpp (one-key-set-window-state 'hidehelp))
             (if onekeybuf
                 (if dedicatedframe (eval resizeframe) (eval resizewindow)))
             (if helpp (one-key-set-window-state 'showhelp))))
          ((eq state 'ownframe)
           (let ((newlines (max (min maxlines (+ buflines 3) maxlines) 5))
                 (helpp (windowp helpwin)))
             (if helpp (one-key-set-window-state 'hidehelp))
             (if dedicatedframe (eval resizeframe)
               (if onekeywin (delete-window onekeywin))
               (switch-to-buffer-other-frame onekeybuf t)
               (with-current-buffer onekeybuf (setq one-key-buffer-dedicated-frame (selected-frame)
                                                    dedicatedframe one-key-buffer-dedicated-frame))
               (set-frame-size dedicatedframe cols newlines)
               (if noselect (select-frame-set-input-focus (previous-frame dedicatedframe)))
               (one-key-update-buffer-contents))
             (if helpp (one-key-set-window-state 'showhelp))))
          ((eq state 'deselect)
           (if dedicatedframe
               (select-frame-set-input-focus (previous-frame dedicatedframe))
             (select-window (if onekeywin (previous-window onekeywin)
                              one-key-buffer-associated-window))))
          ((eq state 'showhelp)
           (with-selected-window (or onekeywin (selected-window))
             (if (and helpframe (> (length (window-list helpframe)) 1))
                 (delete-window helpwin))
             (let ((helpbuf (get-buffer-create one-key-help-buffer-name))
                   (helpbuflines (if helpbuf (with-current-buffer helpbuf
                                               (count-lines (point-min) (point-max))))))
               (if (not dedicatedframe)
                   (display-buffer helpbuf)
                 (set-frame-height dedicatedframe
                                   (min maxlines
                                        (+ (frame-height dedicatedframe)
                                           helpbuflines 2)))
                 (set-window-buffer (split-window-below winlines) helpbuf)))))
          ((eq state 'hidehelp) (if helpwin
                                    (if (or (not dedicatedframe)
                                            (= (length (window-list helpframe)) 1))
                                        (switch-to-prev-buffer helpwin)
                                      (delete-window helpwin)
                                      (set-frame-height dedicatedframe (+ winlines 1)))))
          ((eq state 'close)
           (if dedicatedframe
               (progn (unless (= (length (visible-frame-list)) 1)
                        (delete-frame dedicatedframe))
                      (setq dedicatedframe nil))
             (if onekeywin
                 (if (> (length (visible-frame-list)) 1)
                     (delete-window onekeywin)
                   (if (> (length (window-list onekeyframe)) 1)
                       (delete-window onekeywin)
                     (switch-to-prev-buffer onekeywin))))
             (if helpwin
                 (if (> (length (visible-frame-list)) 1)
                     (delete-window helpwin)
                   (if (> (length (window-list onekeyframe)) 1)
                       (delete-window helpwin)
                     (switch-to-prev-buffer helpwin))))))))
  (one-key-reposition-window-contents))

(defun one-key-menu-window-toggle nil
  "Toggle the one-key menu window to the next state in `one-key-window-toggle-sequence'."
  (interactive)
  (setq one-key-window-toggle-pos (mod (1+ one-key-window-toggle-pos)
                                       (length one-key-window-toggle-sequence)))
  (let* ((nextstate (nth one-key-window-toggle-pos
                         one-key-window-toggle-sequence))
         (pair (consp nextstate)))
    (if pair
        (progn (one-key-set-window-state (car nextstate))
               (one-key-set-window-state (cdr nextstate)))
      (one-key-set-window-state nextstate))))

(defun one-key-menu-window-scroll-up (&optional down)
  "Scroll up one screen of the `one-key' menu window.
If DOWN is non-nil scroll down instead of up."
  (if (one-key-menu-window-exist-p)
      (ignore-errors
        (with-current-buffer one-key-buffer-name
          (if down (scroll-down) (scroll-up))))))

(defun one-key-menu-window-scroll-up-line (&optional down)
  "Scroll up one line of the `one-key' menu window."
  (if (one-key-menu-window-exist-p)
      (ignore-errors
        (with-current-buffer one-key-buffer-name
          (if down (scroll-up 1) (scroll-down 1))))))

(defun one-key-menu-increment-key-usage (item)
  "Increment the key usage statistic for ITEM."
  (destructuring-bind ((key . desc) . rest) item
    (if (commandp rest)
        (setf (cdr item) (list rest 1))
      (if (and (one-key-list-longer-than-1-p rest)
               (numberp (second rest)))
          (progn (incf (second rest))
                 (setf (cdr item) rest))))))

(defsubst one-key-list-longer-than-1-p (x)
  "Return t if x is a list of length > 1, and is not a command."
  (and (not (commandp x))
       (listp x)
       (listp (cdr x))
       (> (length x) 1)))

(defun one-key-menu-brighten-most-used (this-list)
  "Set values of menu item colours proportionally according to how often they have been used.
Argument THIS-LIST is the alist of keys and associated decriptions and functions, or a symbol referencing the list."
  ;; first get min and max keypress values
  (if this-list
      (let ((menu-list (one-key-eval-if-symbol this-list)))
        (if menu-list
            (let* ((minmaxvals (loop for ((key . desc) . rest) in menu-list
                                     for val = (if (one-key-list-longer-than-1-p rest)
                                                   (second rest) 0)
                                     maximize val into max
                                     minimize val into min
                                     finally return (list min max)))
                   (minval (first minmaxvals))
                   (maxval (second minmaxvals))
                   (range (- maxval minval)))
              ;; update the colour value (from HSV) of each item in menu-list
              (loop for item in menu-list
                    for ((key . desc) . rest) = item
                    ;; get current keypress value (indicating number of times key has been pressed)
                    for val = (if (one-key-list-longer-than-1-p rest)
                                  (second rest) 0)
                    ;; calculate colour value from keypress value
                    for vval = (if (= range 0)
                                   0.5
                                 (+ (/ (- val minval) (* 2.0 range)) 0.5))
                    ;; get current background and foreground colour
                    for (h s v) = (one-key-get-item-colour item nil 'hsv)
                    ;; update value of background colour
                    for newbgcol = (hexrgb-hsv-to-hex h s vval) do
                    (setf (cdar item) (one-key-colourize-string newbgcol desc))))))))

(defun one-key-optimize-col-widths (lengths maxlength)
  "Given a list of the lengths of the menu items, work out the maximum possible number of columns and return their widths.
Actually the function returns a list of cons cells in the form (numrows . width) each of which corresponds to a column in
the optimal assignment and indicates the number of rows and width of that column.
LENGTHS is the list of menu item lengths, and MAXLENGTH is the maximum width over which the columns may span."
  (let ((nitems (length lengths))
        (ncols 1)
        bestcols)
    (if (< (apply '+ lengths) maxlength)
        (mapcar (lambda (len) (cons 1 len)) lengths)
      (while (progn
               (setq ncols (1+ ncols))
               (let ((itemspercol (make-list ncols (/ nitems ncols)))
                     colspecs)
                 (loop for n to (1- (% nitems ncols)) do (incf (nth n itemspercol) 1))
                 (setq colspecs (loop for colnum to (1- ncols) with sofar = 0
                                      for nrows = (nth colnum itemspercol)
                                      for colwidth = (loop for rownum to (1- nrows)
                                                           for itemnum = (if one-key-column-major-order
                                                                             (+ sofar rownum)
                                                                           (+ colnum (* rownum ncols)))
                                                           maximize (nth itemnum lengths))
                                      summing colwidth into width
                                      collecting (cons nrows colwidth) into specs
                                      do (setq sofar (+ sofar nrows))
                                      finally (return (list width specs))))
                 (if (< (car colspecs) maxlength) (setq bestcols (cadr colspecs)) nil))))
      (or bestcols (list (cons nitems (1- maxlength)))))))

(defun one-key-menu-format (this-list)
  "Format `one-key' menu window key description text (as displayed by the `one-key-menu' function).
Argument THIS-LIST is an alist of keys and corresponding descriptions and functions, or a symbol referencing that list.
Each element of this list is in the form: ((key . describe) . command)."
  (let* ((menu-list (one-key-eval-if-symbol this-list))
         (winwidth (- (window-width) 1))
         (maxlen (/ winwidth one-key-min-number-of-columns)))
    (if (> (length menu-list) 0)
        (let* (menu-list2
               (item-lengths (mapcar (lambda (item)
                                       (let* ((desc (cdar item))
                                              (desclen (length desc))
                                              (keydesc (one-key-remap-key-description (caar item)))
                                              (keydesclen (length keydesc))
                                              (rest (cdr item))
                                              (newdesclen (min (- maxlen keydesclen 4) desclen))
                                              (newdesc (substring desc 0 newdesclen))
                                              (newitem (cons (cons keydesc newdesc) rest)))
                                         (push newitem menu-list2)
                                         (+ newdesclen keydesclen 4)))
                                     menu-list))
               (colspecs (one-key-optimize-col-widths item-lengths winwidth))
               (numitems (length menu-list2))
               (maxcols (length colspecs))
               (maxrow (caar (last colspecs)))
               (extras (% numitems maxcols))
               keystroke-msg)
          (setq menu-list (nreverse menu-list2))
          (loop for row from 0 to maxrow
                for ncols = (if (= row maxrow) extras maxcols) do
                (loop for col from 0 to (1- ncols) with sofar = 0
                      for (colsize . width) = (nth col colspecs)
                      for itemnum = (if one-key-column-major-order
                                        (+ sofar row)
                                      (+ (* row maxcols) col))
                      for item = (nth itemnum menu-list)
                      for (key . desc) = (car item)
                      for keytext = (format "[%s] %s " (one-key-remap-key-description key) desc)
                      if item do
                      (push keytext keystroke-msg)
                      (push (make-string (- width (length keytext)) ? ) keystroke-msg)
                      (setq sofar (+ sofar colsize)))
                (push "\n" keystroke-msg))
          (mapconcat 'identity (nreverse keystroke-msg) ""))
      "No menu items!")))

(defun one-key-get-menu-item (key this-list)
  "Return the member of MENU-ALIST corresponding to key KEY, or nil if no such item exists.
KEY may be a char or the string representation of a char.
MENU-ALIST is a list of `one-key' menu items."
  (let* ((menu-list (one-key-eval-if-symbol this-list))
         (thekey (one-key-key-description key)))
    (find-if (lambda (x) (equal (one-key-remap-key-description (caar x)) thekey))
             menu-list)))

(defun one-key-add-menu-item (key desc contents menu-alist)
  "Add a new item to MENU-ALIST in the form ((KEY . DESC) . CONTENTS), overwriting any item with the same key.
Return the new value of MENU-ALIST after adding the item.
KEY may be a char or the string representation of a char.
DESC must be a string (the description to display in the menu).
CONTENTS may be a command or a list whose first element is a command (it will be executed when KEY is pressed in the menu)."
  (let* ((thekey (one-key-key-description key))
         (item (one-key-get-menu-item thekey menu-alist)))
    (if item (progn (setf (cdar item) desc (cdr item) contents) menu-alist)
      (add-to-list 'menu-alist (cons (cons thekey desc) contents)))))

(defun one-key-open-submenu (names vars)
  "Open menu/menus named NAMES with menu alist variables VARS as a submenu of the current menu, replacing it if necessary.
NAMES can be either a single string or a list of strings. VARS can be a single menu alist or a list of menu alists.
If `one-key-submenus-replace-parents' is non-nil then the current menu will be replaced with the submenu, otherwise
a new menu will be added to the current menu set.

This function only works when called within the context of the one-key buffer since it depends on buffer local variables."
  (one-key-add-menus names vars)
  (with-current-buffer one-key-buffer-name
    (if one-key-submenus-replace-parents
        (one-key-delete-menus (1- one-key-buffer-menu-number)))
    (setq one-key-buffer-temp-action
          (nth one-key-window-toggle-pos one-key-window-toggle-sequence))))

(defun one-key-merge-menu-lists (lista listb)
  "Given two one-key menu lists, merge them and return the result.
Any items in LISTB that have the same command as an item in LISTA will be disgarded, and any keys in LISTB that
also occur in LISTA will be exchanged for a new key that doesn't occur in either list."
  (loop for ((key . desc) . cmd) in (nconc lista listb) with usedkeys with usedcmds
        unless (memq cmd usedcmds)
        collect (cons (cons (let ((newkey (one-key-generate-key desc usedkeys nil key)))
                              (push newkey usedkeys)
                              newkey)
                            desc)
                      (progn (push cmd usedcmds) cmd))))

(defun* one-key-create-menus-from-menubar-keymap (keymap &optional (name (number-to-string (random)))
                                                         &key (invalidkeys
                                                               (append one-key-disallowed-keymap-menu-keys
                                                                       (one-key-get-special-key-descriptions
                                                                        one-key-default-special-keybindings))))
  "Create menu alists for a menu-bar keymap KEYMAP and all sub menus.
Submenus will always be assigned to variables whose names are formed by concatenating NAME with the name of the menu-bar
submenu. If NAME is not supplied then a random number will be used instead.
Keys will be assigned to the items using the `one-key-generate-key' function. To exclude certain keys from being used
set INVALIDKEYS to a list of keys (as key-description strings or chars) to be excluded.
These keys will only be excluded from the toplevel menu, not the submenus."
  (let* (
         ;; if the keymap has only one item which is itself a keymap then use that instead
         (keymap2 (if (= (length keymap) 2)
                      (let* ((item (cadr keymap))
                             (pos (position 'keymap item)))
                        (if pos (nthcdr pos item)
                          (find-if 'keymapp item)
                          keymap))
                    keymap))
         ;; keep a track of which keys have been used
         (usedkeys invalidkeys)
         ;; global variable for the main menu
         (mainvarname (concat "one-key-menu-" name "-alist"))
         (mainvar (intern mainvarname))
         ;; local variable for the one-key menu items
         menu-alist)
    ;; loop over the items in the keymap which have keybindings
    (loop for key being the key-codes of keymap2 using (key-bindings bind)
	  for itemislist = (listp bind)
          for itemiscons = (and itemislist (not (listp (cdr bind))))
          ;; if the item is a keymap (to be used as a submenu), extract it
          for itemkeymap = (if (and itemislist (not itemiscons))
                               (let ((pos (position 'keymap bind)))
                                 (if pos (nthcdr pos bind)
                                   (find-if 'keymapp bind))))
          ;; if the item is a command, extract it
          for itemcmd = (if itemiscons (cdr bind)
			  (if itemislist
			      (and (position 'menu-item bind)
				   (find-if (lambda (x) (and (not (stringp x)) (commandp x))) bind))))
          ;; get a description for the command or submenu
          for desc = (if itemiscons (let ((bind0 (car bind))
                                          (bind1 (cdr bind)))
                                      (if (stringp bind0) bind0
                                        (replace-regexp-in-string "-" " " (symbol-name bind1))))
		       (if itemislist
			   (let* ((bind0 (car bind))
				  ;; use either the first string in the item, or the keyname or the command name
				  (str (or (find-if 'stringp bind)
					   (and bind0 (symbolp bind0) (symbol-name bind0))
					   (if itemcmd
					       (capitalize
						(replace-regexp-in-string "-" " " (symbol-name itemcmd)))
					     "unknown"))))
			     ;; if the item is a submenu highlight it, otherwise don't
			     (cond (itemkeymap (one-key-colourize-string "cyan" str))
				   (t str)))))
          ;; get a unique key for the item
          for keystr = (if itemislist (one-key-generate-key desc usedkeys))
          ;; get the command for the one-key menu item
          for cmd = (cond (itemcmd itemcmd) ;if the keymap item is a command then use that
                          (itemkeymap ;if the item is a keymap then create a submenu item for the one-key menu
                           (let* ((desc1 (substring-no-properties desc))
                                  (desc2 (replace-regexp-in-string " " "_" desc1))
                                  (submenuname (concat name "_" desc2))
                                  ;; create an appropriate variable name to hold the submenu
                                  (varname (concat "one-key-menu-" submenuname "-alist"))
                                  ;; create the submenu
                                  (submenuvar (one-key-create-menus-from-menubar-keymap
                                               itemkeymap submenuname))
                                  (submenuitems (eval submenuvar)))
                             (if (> (length submenuitems) 1)
                                 ;; if the submenu contains more than one item create the command to open the submenu
                                 `(lambda nil (interactive)
                                    (one-key-open-submenu ,submenuname ,submenuvar))
                               ;; if the submenu only contains one item, add that item to the current menu,
                               ;; delete the submenu variable, and set cmd to nil so it won't be added later
                               (push (car submenuitems) menu-alist)
                               (unintern varname)
                               nil))))
          for skip = (if desc (string-match "^--" desc))
          unless (or skip (not cmd)) do ; skip menu divider and null items
          ;; add the item to the one-key menu list
          (push (cons (cons keystr desc) cmd) menu-alist)
          ;; mark this key as used
          (push keystr usedkeys))
    ;; set the global variable to hold the one-key menu items,
    ;; mark it to be saved on exit, and return it
    (set mainvar menu-alist)
    (add-to-list 'one-key-altered-menus mainvarname)
    mainvar))

(defun* one-key-create-menus-from-keymap (keymap &optional
                                                 (name (if (symbolp keymap)
                                                           (replace-regexp-in-string
                                                            "-map$" "" (symbol-name keymap))
                                                         "unknown"))
                                                 prefix
                                                 &key
                                                 (invalidkeys
                                                  (append one-key-disallowed-keymap-menu-keys
                                                          (one-key-get-special-key-descriptions
                                                           one-key-default-special-keybindings))))
  "Create menu alists for a keymap and all sub keymaps.
KEYMAP is the keymap or keymap symbol to use, NAME is a name for the keymap (e.g. \"emacs-lisp-mode\") and will
be used to remove common prefix words from the item descriptions.
If a symbol is supplied for keymap then by default NAME will be set to the symbols name but with \"-map\" removed from
the end (if present).
Variables will be created for storing the menus, and the variable for the main menu will be returned.
The main variable will be named one-key-menu-NAME-alist, and the submenus variables will be named
 one-key-menu-NAME-???-alist (where ??? is the name of the submenu).
If the PREFIX arg is present then key descriptions in the menu will be prefixed by this arg (this is used when the
function is called recursively).

Any submenus that have fewer than `one-key-min-keymap-submenu-size' items will be merged with their parent menu,
unless this would create a menu of more than (length one-key-default-menu-keys) items.

Also any items whose commands have keybindings that are in INVALIDKEYS will have new keys created for them.
By default this is set to the keys in `one-key-disallowed-keymap-menu-keys' and the keys used by the special keys
in `one-key-default-special-keybindings'."
  (let* ((name (or name  ; make sure the name variable is set properly
                   (if (symbolp keymap)
                       (replace-regexp-in-string
                        "-map$" "" (symbol-name keymap))
                     "unknown")))
         (keymap1 (if (functionp keymap) (symbol-function keymap)
                    (one-key-eval-if-symbol keymap)))         ;get the keymap value
         (menubar (lookup-key keymap1 [menu-bar])) ;get any menu-bar items
         (mainvar (intern (concat "one-key-menu-" name "-alist"))) ;variable to hold the main menu
         usedkeys usedcmds menu-alist)
    ;; loop over the keys in the keymap
    (loop for key being the key-codes of keymap1 using (key-bindings cmd)
          ;; get the key description
          for keystr = (if (consp key) ; could be a cons cell representing a range of keys if we have a char-table
                           (one-key-key-description (car key))
                         (one-key-key-description key))
          for keydesc = (if prefix
                            (if (equal prefix "ESC")
                                (concat "M-" keystr)
                              (concat prefix " " keystr))
                          keystr)
          ;; skip null keys, menu-bar items (these will be added later), duplicate commands,
          ;; and shadowed keys (possible if keymap inherits from another keymap) 
          unless (or (string-match one-key-null-keys keystr)
                     (eq key 'menu-bar)
                     (memq cmd usedcmds)
                     (member keystr usedkeys))
          ;; If the item is a command...
          do (cond ((commandp cmd)
                    (let* (
                           ;; create the item description from the command name or sexp,
                           ;; and add the key description to the end
                           (keymapname (replace-regexp-in-string "mode$\\|mode-map$\\|map$" "" name))
                           (keymapnameregex (regexp-opt (list keymapname (capitalize keymapname))))
                           (desc1 (if (symbolp cmd) (symbol-name cmd) (format "%S" cmd)))
                           (desc1a (replace-regexp-in-string keymapnameregex "" desc1))
                           (desc1b (capitalize (replace-regexp-in-string "-" " " desc1a)))
                           (desc2 (concat desc1b " (" keydesc ")"))
                           ;; if the key is invalid, generate a new one
                           (keystr2 (if (member keystr invalidkeys)
                                        (one-key-generate-key desc2 usedkeys)
                                      keystr)))
                      ;; mark this command as used
                      (push cmd usedcmds)
                      ;; mark the key as used and add the item to the menu list
                      (push keystr2 usedkeys)
                      (push (cons (cons keystr2 desc2) cmd) menu-alist)))
                   ;; If the item is a keymap..
                   ((keymapp cmd)
                    (let* (
                           ;; create an appropriate name and description for the submenu
                           (submenuname (concat name "_" keystr))
                           (desc2 (concat "Prefix key (" keydesc ")"))
                           (desc3 (one-key-colourize-string "cyan" desc2))
                           ;; if the key is invalid, generate a new one
                           (keystr2 (if (member keystr invalidkeys)
                                        (one-key-generate-key desc2 usedkeys)
                                      keystr))
                           (existingvar (intern-soft (concat "one-key-menu-" submenuname "-alist"))))
                      ;; if a submenu for this prefix key has already been created then merge this one with it
                      (if (and existingvar (member keystr usedkeys))
                          (let ((menuvar (one-key-create-menus-from-keymap
                                          cmd (concat submenuname "temp") keydesc)))
                            (set existingvar (one-key-merge-menu-lists (eval existingvar) (eval menuvar)))
                            ;; delete the temporary variable
                            (unintern (symbol-name menuvar)))
                        ;; otherwise call this function recursively to create a new submenu
                        (let* ((menuvar (one-key-create-menus-from-keymap cmd submenuname keydesc))
                               ;; number of items in the submenu
                               (numnewitems (length (eval menuvar))))
                          ;; if the number of items in the submenu is small then merge it with the parent menu
                          (if (and (< numnewitems one-key-min-keymap-submenu-size)
                                   (< (+ (length menu-alist) numnewitems) (length one-key-default-menu-keys)))
                              (progn (setq menu-alist (one-key-merge-menu-lists menu-alist (eval menuvar)))
                                     (unintern (symbol-name menuvar)))
                            ;; otherwise create a link to it in the parent menu
                            (let ((cmd2 `(lambda nil (interactive) (one-key-open-submenu ,submenuname ,menuvar))))
                              (push keystr usedkeys)
                              (push keystr2 usedkeys) ; mark key as used
                              (push (cons (cons keystr2 desc3) cmd2) menu-alist)))))))))
    ;; if there are menu-bar items, add them if user agrees
    (if (and menubar
             (or (eq one-key-include-menubar-items t)
                 (and (eq one-key-include-menubar-items 'prompt)
                      (y-or-n-p "Include menu-bar items?"))))
        ;; we use no name for the menubar menu since the symbol will subsequently be uninterned anyway
        (let ((menuvar (one-key-create-menus-from-menubar-keymap menubar (concat name "-menubar")
                                                                 :invalidkeys (append usedkeys invalidkeys))))
          (setq menu-alist (one-key-merge-menu-lists menu-alist (eval menuvar)))
          (unintern (symbol-name menuvar))))
    ;; set the value of the variable to hold the main menu, and make sure it will be saved if necessary
    (set mainvar menu-alist)
    (add-to-list 'one-key-altered-menus (symbol-name mainvar))
    ;; return the main menu variable
    mainvar))

(defun one-key-generate-key (desc &optional usedkeys elements trykey)
  "Return a key for the menu item whose description string is DESC.
The generated key can be used in a `one-key' menu, and this function can be used to help automatic creation
of `one-key' menus.
The function will try to choose a key corresponding to a char appearing in DESC, first choosing lowercase letters,
then uppercase, then key presses with control then with the meta key.
USEDKEYS should be a list of keys (as string descriptions, e.g. as returned by `kbd') which cannot be used.
If the ELEMENTS arg (a list of keys) is provided the normal key selection method will not be used and instead a key
from ELEMENTS that is not in USEDKEYS will be chosen instead.
If TRYKEY is provided it should be a key and will be returned if it is not in USEDKEYS (otherwise another key will be
found)."
  (let ((trykey2 (one-key-key-description trykey)))
    (flet ((findmatch (transformer keys)
                      (dolist (key keys)
                        (let ((key2 (funcall transformer key)))
                          (if (not (member key2 usedkeys))
                              (return key2)))))
           (uptransformer (prefix) `(lambda (char) (concat ,prefix (upcase (char-to-string char)))))
           (downtransformer (prefix) `(lambda (char) (concat ,prefix (downcase (char-to-string char)))))
           (findall (keys) (or (findmatch (downtransformer "") keys)
                               (findmatch (uptransformer "") keys)
                               (findmatch (downtransformer "C-") keys)
                               (findmatch (uptransformer "C-") keys)
                               (findmatch (downtransformer "M-") keys)                               
                               (findmatch (uptransformer "M-") keys))))
      (or (and trykey2 (not (member trykey2 usedkeys))
               trykey2)
          (if elements (loop for element in elements
                             for keystr = (one-key-key-description element)
                             if (not (member keystr usedkeys))
                             return keystr))
          (findall (reverse (intersection one-key-default-menu-keys (mapcar 'identity desc))))
          (findall one-key-default-menu-keys)
          (error "Can not generate a unique key for item : %s" desc)))))

(defun one-key-remap-key-description (keydesc)
  "Remap key description string KEYDESC according to it's entry in `one-key-key-description-remap' if it has one."
  (or (assoc-default keydesc one-key-key-description-remap) keydesc))

(defun* one-key-remap-invalid-keys (menu-alist &key
                                               (invalidkeys
                                                (append one-key-disallowed-keymap-menu-keys
                                                        (one-key-get-special-key-descriptions
                                                         one-key-default-special-keybindings)))
                                               (keyfunc 'one-key-generate-key))
  "Remap any keys in the one-key menu MENULIST that are in INVALIDKEYS to new unused keys.
INVALIDKEYS should be a list of string descriptions of keys (e.g. as returned by the `kbd' macro).
New keys are selected using KEYFUNC which should be a function taking two arguments - a description of the command and a list of
used keys (in that order), and return a new unused key. The default value for KEYFUNC is `one-key-generate-key'.
By default INVALIDKEYS is set to the keys in `one-key-disallowed-keymap-menu-keys' and `one-key-default-special-keybindings'"
  (let ((usedkeys invalidkeys)
        newkey)
    (loop for ((key . desc) . cmd) in (one-key-eval-if-symbol menu-alist)
          if (member key usedkeys)
          do (setq newkey (funcall keyfunc desc usedkeys))
          and collect (cons (cons newkey desc) cmd)
          and do (push newkey usedkeys)
          else collect (cons (cons key desc) cmd)
          and do (push key usedkeys))))

(defun one-key-key-description (keyseq)
  "Return the key description for the key sequence or single key KEYSEQ.
KEYSEQ may be a vector, integer, symbol or string representing a key sequence, or nil.
If KEYSEQ is nil then nil is returned, if it is non-nil and not a string, vector, symbol or number then an error is flagged."
  (one-key-remap-key-description
   (cond ((not keyseq) nil)
         ((vectorp keyseq) (key-description keyseq))
         ((numberp keyseq) (single-key-description keyseq))
         ((symbolp keyseq) (single-key-description keyseq))
         ((stringp keyseq) (if (string-match "^C-\\|^M-\\|^RET\\|^SPC\\|^TAB\\|^<[a-z0-9-]+>\\|^[a-zA-Z0-9]" keyseq)
                               keyseq
                             (key-description keyseq)))
         ((listp keyseq) (single-key-description (car keyseq))) 
         (t (error "Invalid key sequence: %S" keyseq)))))

(defun one-key-append-keys-to-descriptions (descriptions keys)
  "Append key descriptions for keys in KEYS to corresponding descriptions in DESCRIPTIONS, and return result.
DESCRIPTIONS should be a list of menu item descriptions and KEYS should be a list of keys as numbers, vectors or strings.
KEYS should be the same length as DESCRIPTIONS.
If any elements of key are nil then the corresponding description will be left alone.
If any element of descriptions is nil it will be left as nil."
  (flet ((addkeys (desc key)
                  (let ((keystr (one-key-key-description key)))
                    (if desc
                        (if keystr (concat desc " (" keystr ")")
                          desc)
                      nil))))
    (mapcar* 'addkeys descriptions keys)))

(defun one-key-append-numbers-to-menu-name (menuname nummenus)
  "Return list of menu names formed by appending numbers to MENUNAME.
The new names will be in the form \"MENUNAME (N)\" where N runs over the integers from 1 to NUMMENUS.
This is useful for creating menu types that return multiple menus."
  (loop for num from 1 to nummenus
        collect (concat menuname " (" (number-to-string num) ")")))

(defun one-key-get-menu-splits (nitems maxsize)
  "Return pairs of indices indicating how NITEMS menu items should be divided among menus of size MAXSIZE.
The return value is a list of cons cells each containing start and end indices indicating which items should go
in the associated menu."
  (let* ((nitemslast (% nitems maxsize)))
    (loop with start = 0
          with end = 0
          while (< end nitems)
          do (setq start end end (min (+ end maxsize) nitems))
          collect (cons start end))))

(defun* one-key-create-menu-lists (commands &optional descriptions keys (addkeydescs t)
                                            &key (maxsize (length one-key-default-menu-keys))
                                            (keyfunc 'one-key-generate-key)
                                            (invalidkeys
                                             (append one-key-disallowed-keymap-menu-keys
                                                     (one-key-get-special-key-descriptions
                                                      one-key-default-special-keybindings))))
  "Create list/lists of menu items for use in `one-key' menu.
COMMANDS should be a list of commands for the menu items, and KEYS an optional corresponding list of keys.
If any element in KEYS is nil, or is a repeat of a previously used key in the current menu, or if KEYS is nil,
then KEYFUNC will be used to generate a key for the item.
KEYFUNC should be a function of two arguments: the item description and a list of used keys (in that order).
DESCRIPTIONS is an optional argument which should contain a list of descriptions for the menu items.
If any of the items in DESCRIPTIONS is nil or if DESCRIPTIONS is not supplied then the item will have its description
set from the corresponding command name.
If the number of menu items is larger than MAXSIZE then several menus will be created, each of
which contains at most MAXSIZE items. By default MAXSIZE is equal to the length of `one-key-default-menu-keys',
and KEYFUNC is set to `one-key-generate-key' (which selects keys from `one-key-default-menu-keys').
If ADDKEYDESCS is non-nil (default) then key descriptions will be added to the end of the command descriptions
in the menu.
INVALIDKEYS is an optional list of keys to exclude from the menu. If any key in KEYS is also in INVALIDKEYS then
a new key will be created to replace it. By default it is set to the keys in `one-key-disallowed-keymap-menu-keys'
and `one-key-get-special-key-descriptions'.

MAXSIZE, KEYFUNC and INVALIDKEYS are keyword arguments and need to be set using, e.g. :maxsize arg1, :keyfunc arg2,
and :invalidkeys arg3."
  (let* ((nitems (length commands))
         (indices (one-key-get-menu-splits nitems maxsize))
         (nummenus (length indices))
         (menu-alists
          (loop for (start . end) in indices
                for cmds = (subseq commands start end)
                for descs = (subseq descriptions start end)
                for keys2 = (subseq keys start end)
                for usedkeys = invalidkeys
                for descs2 = (loop for desc in descs
                                   for cmd in cmds
                                   for key in keys2
                                   for desc2 = (or desc
                                                   (capitalize
                                                    (replace-regexp-in-string
                                                     "-" " " (symbol-name cmd))))
                                   collect (if (and key addkeydescs)
                                               (concat desc2 " ("
                                                       (one-key-key-description key)
                                                       ")")
                                             desc2))
                for keystrs = (loop for key in keys2
                                    for desc in descs2
                                    for keydesc = (one-key-key-description key)
                                    if (member* keydesc usedkeys
                                                :test (lambda (a b) (equal a (one-key-key-description b))))
                                    collect (let ((newkey (funcall keyfunc desc usedkeys)))
                                              (push newkey usedkeys)
                                              newkey)
                                    else
                                    collect (or (and key (push keydesc usedkeys) keydesc)
                                                (let ((newkey (funcall keyfunc desc usedkeys)))
                                                  (push newkey usedkeys)
                                                  newkey)))
                collect (loop for cmd in cmds
                              for desc in descs2
                              for key in keystrs
                              collect (cons (cons key desc) cmd)))))
    menu-alists))

(defun one-key-build-menu-sets-menu-alist nil
  "Build menu-alist for opening menu sets defined in `one-key-sets-of-menus-alist'."
  (let* ((descriptions (mapcar (lambda (item)
                                 (let ((str (car item)))
                                   (if (equal str one-key-default-menu-set)
                                       (one-key-colourize-string "red" str)
                                     str))) one-key-sets-of-menus-alist))
         (commands (mapcar (lambda (item)
                             `(lambda nil (interactive)
                                (one-key-open-menu-set ,(car item))))
                           one-key-sets-of-menus-alist)))
    (car (one-key-create-menu-lists commands descriptions))))

(defun one-key-save-altered-menus nil
  "Save the menus listed in `one-key-altered-menus' into the file `one-key-menus-save-file'.
Any menu names that match the regular expressions in `one-key-exclude-from-save' will not be saved."
  (loop for x in one-key-altered-menus
        for varname = (if (stringp x) x (if (symbolp x) (symbol-name x)))
        for name = (if (string-match "one-key-menu-.*-alist" varname)
                       (substring varname 13 -6))
        for var = (intern-soft varname)
        for menulist = (eval var)
        for exclude = (loop for regex in one-key-exclude-from-save
                            if (string-match regex varname) return t)
        if (and name var (not exclude)) do (one-key-save-menu name var)))

(defun one-key-get-major-mode-menu (name)
  "Return a menu name and menu alist for the current major mode.
This function is used by `one-key-types-of-menu' and the NAME argument is redundant.
A cons cell in the form (menu-name . menu-alist) will be returned.
If there is an element of `one-key-major-mode-remap-alist' associated with the current major mode then that will be used,
otherwise the name of the current major mode will be used.
In both cases if the variable `one-key-menu-<menu-name>-alist' (where <menu-name> is the menu name associated with this
major mode) exists then it will be used, otherwise it will be created."
  (let* ((modename (or (assoc-default major-mode one-key-major-mode-remap-alist)
                       (with-selected-window
                           (previous-window)
                         (symbol-name major-mode))))
         (menuname (concat "major-mode:" (replace-regexp-in-string "-mode$" "" modename)))
         (symname (concat "one-key-menu-" modename "-alist"))
         (menusym (intern-soft symname)))
    (if (or (not menusym) (not (boundp menusym)))
        (let* ((mapname (concat modename "-map"))
               (mapsym (intern-soft mapname)))
          (if (and mapsym (boundp mapsym))
              (progn (one-key-create-menus-from-keymap mapsym modename)
                     (setq menusym (intern-soft symname)))
            (message "Can't create menu for %S" major-mode)
            (setq menusym nil))))
    (cons menuname menusym)))

(defun one-key-create-blank-menu (name)
  "Prompt user for a name, create a blank one-key menu and return a cons cell containing the name and the menu."
  (let* ((name (read-string "Menu name: "))
         (symname (concat "one-key-menu-" name "-alist"))
         (menusym (intern-soft symname)))
    (while menusym
      (if (not (y-or-n-p "Menu with that name already exists, overwrite?"))
          (progn (setq name (read-string "Menu name: "))
                 (setq symname (concat "one-key-menu-" name "-alist"))
                 (setq menusym (intern-soft symname)))
        (setq menusym nil)))
    (setq menusym (intern symname))
    (set menusym nil)
    (add-to-list 'one-key-altered-menus (symbol-name menusym))
    (cons name menusym)))

(defun one-key-retrieve-existing-menu (x)
  "Prompt for an existing one-key menu and return it in a cons cell along with its name."
  (let* ((names (loop for sym being the symbols
                      for name = (symbol-name sym)
                      when (string-match "one-key-menu-\\(.+\\)-alist" name)
                      collect (match-string 1 name)))
         (name (if (featurep 'ido)
                   (ido-completing-read "Menu: " names)
                 (completing-read "Menu: " names))))
    (cons name (intern-soft (concat "one-key-menu-" name "-alist")))))

(defun one-key-create-menu-from-existing-keymap (name)
  "Prompt the user for a keymap and return a one-key menu for it along with it's name, in a cons cell."
  (let (partname)
    (if (string-match "^keymap:" name)
        (setq partname (replace-regexp-in-string "^keymap:" "" name)
              name (concat partname "-map"))
      (let* ((names (loop for sym being the symbols
                          when (or (keymapp sym)
                                   (and (boundp sym)
                                        (keymapp (symbol-value sym))))
                          collect (symbol-name sym))))
        (setq name (if (featurep 'ido)
                       (ido-completing-read "Keymap: " names)
                     (completing-read "Keymap: " names))
              partname (replace-regexp-in-string "-map$" "" name))))
    (let ((kmap (intern-soft name)))
      (one-key-create-menus-from-keymap kmap partname)
      (cons (concat "keymap:" partname)
            (intern-soft (concat "one-key-menu-" partname "-alist"))))))

(defun one-key-create-menu-from-prefix-key-keymap (keystr)
  "Prompt the user for a prefix key and return a one-key menu for it along with it's name, in a cons cell."
  (interactive (let ((keystr (read-string "Enter the emacs string representation of the required prefix keys: ")))
                 (while (not (ignore-errors (read-kbd-macro keystr)))
                   (setq keystr (read-string "Invalid key sequence! Try again: ")))
                 (list keystr)))
  (let (keysequence kmap)
    (setq keysequence (read-kbd-macro keystr))
    (setq kmap (key-binding keysequence))
    (if (keymapp kmap)
        (let* ((kmap2 (if (functionp kmap) (symbol-function kmap)
                        (one-key-eval-if-symbol kmap)))
               (desc1 (replace-regexp-in-string " " "_" keystr))
               (desc2 (replace-regexp-in-string "#" "\\\\#" desc1))
               (desc3 (concat "prefix-key:" desc2)))
          (cons desc3 (one-key-create-menus-from-keymap kmap2 desc3 desc2)))
      (error "No keymap is currently associated with that prefix key!"))))

(defun one-key-prefix-key-menu-command (keystr &optional submenup)
  "Given prefix key description KEYSTR open a one-key menu containing the commands associated with that prefix key.
If SUBMENUP is non-nil then the `one-key-open-submenu' command is used to add/replace a menu in the current menu set."
  (interactive (let ((keystr (read-string "Enter the emacs string representation of the required prefix keys: ")))
                 (while (not (ignore-errors (read-kbd-macro keystr)))
                   (setq keystr (read-string "Invalid key sequence! Try again: ")))
                 (list keystr)))
  (let* ((pair (one-key-create-menu-from-prefix-key-keymap keystr))
         (name (car pair))
         (menu (cdr pair)))
    (if submenup
        (one-key-open-submenu name menu)
      (one-key-menu name menu))))

(defun one-key-create-menu-sets-title-format-string nil
  "Return a title format string for menu sets one-key menus."
  (let* ((col1 (if one-key-auto-brighten-used-keys "#7FFF00000000" "red"))
         (col2 (cdr (assq 'background-color (frame-parameters))))
         (hsv2 (hexrgb-hex-to-hsv col2))
         (col2a (hexrgb-hsv-to-hex (first hsv2) (second hsv2) 0.5))
         (col2b (if one-key-auto-brighten-used-keys col2a col2))
         (keystr1 (one-key-colourize-string col1 "default menu set"))
         (keystr2 (one-key-colourize-string col2b "normal menu set"))
         (menuname (nth one-key-buffer-menu-number one-key-buffer-menu-names))
         (sortindex (or (assoc-default menuname one-key-sort-method-indices-alist) 0))
         (sortmethods (or (nth 5 (one-key-get-menu-type menuname))
                          one-key-default-sort-method-alist)))
    (concat keystr1 keystr2 "\n"
            (format "Sorted by %s (%s first). Press <f1> for help.\n"
                    (car (nth sortindex sortmethods))
                    (if one-key-column-major-order "columns" "rows")))))

(defun one-key-submit-bug-report nil
  "Submit a bug report for one-key via mail."
  (interactive)
  (require 'reporter)
  (one-key-set-window-state 'close)
  (let ((reporter-prompt-for-summary-p "Bug report subject: "))
    (reporter-submit-bug-report
     one-key-maintainer-email
     (concat "one-key version " one-key-version)
     (list 'one-key-altered-menus
           'one-key-auto-brighten-used-keys
           'one-key-autosave-menus
           'one-key-buffer-name
           'one-key-column-major-order
           'one-key-copied-items
           'one-key-current-item-being-moved
           'one-key-default-menu-keys
           'one-key-default-menu-number
           'one-key-default-menu-set
           'one-key-default-sort-method-alist
           'one-key-default-special-keybindings
           'one-key-default-title-func
           'one-key-disallowed-keymap-menu-keys
           'one-key-exclude-from-save
           'one-key-include-menubar-items
           'one-key-item-foreground-colour
           'one-key-major-mode-remap-alist
           'one-key-menu-sets-special-keybindings
           'one-key-menu-show-key-help
           'one-key-menu-window-max-height
           'one-key-menus-save-file
           'one-key-min-keymap-submenu-size
           'one-key-mode-line-format
           'one-key-mode-line-message
           'one-key-null-keys
           'one-key-persistent-menu-number
           'one-key-popup-window
           'one-key-sets-of-menus-alist
           'one-key-special-keybindings
           'one-key-submenus-replace-parents
           'one-key-menu-toplevel-alist
           'one-key-types-of-menu)
     nil nil
     "Remember to cover the basics, that is, what you expected to happen and
what in fact did happen.
Please mention which major mode was active and what keys were pressed at the time of the fault.

To read how to make a good bug report see:

http://www.gnu.org/software/emacs/manual/html_node/emacs/Understanding-Bug-Reporting.html
------------------------------------------------------------------------")))

(defun* one-key-center-string (string &optional (width (window-width)) shorten)
  "Center STRING in space padded string of length WIDTH, and return padded string.
If length of STRING is greater than WIDTH, and shorten is non-nil then return middle section of STRING of length WIDTH.
By default WIDTH is set to the current window width (as returned by the `window-width' function)."
  (let* ((strlen (length string))
         (lpadlen (/ (- width strlen) 2))
         (rpadlen (- width strlen lpadlen)))
    (if (> width strlen)
        (concat (make-string lpadlen ? ) string (make-string rpadlen ? ))
      (if shorten (substring string (- lpadlen) rpadlen) string))))

;; FIXME: make this (and related functions) iterative instead of recursive, and make it work with new
;; one-key buffer system.
(defun* one-key-read (prompt collection
                             &optional predicate require-match def title-string
                             (special-keys '(quit-close quit-open toggle-display next-menu prev-menu up down
                                                        scroll-down scroll-up toggle-help documentation
                                                        toggle-row/column-order sort-next sort-prev
                                                        reverse-order limit-items donate report-bug)))
  "one-key replacement for the built-in `completing-read' function.
PROMPT is the title string for the *One-Key* buffer.
COLLECTION can be a list of strings, an alist, an obarray or a hash table.
If non-nil PREDICATE is a predicate function used to filter the items in COLLECTION before placing them in the menu
 (see `try-completion' for more details).
If REQUIRE-MATCH is non-nil then the *One-Key* window will not disappear until an item key is pressed or the menu is quit
using the appropriate special key.
If REQUIRE-MATCH is nil then the *One-Key* window will close if a key is pressed which doesn't correspond to a menu item
or a special key, and the value of DEF will be returned.
The optional argument SPECIAL-KEYS is a list of symbols refering to the special keys to use for this menu,
and they must be defined in `one-key-special-keybindings'. You can usually leave the SPECIAL-KEYS argument alone as it uses
sensible defaults."
  (flet ((tostring (x) (cond ((stringp x) x)
                             ((symbolp x) (symbol-name x))
                             ((numberp x) (number-to-string x))
                             ((listp x) (tostring (car x))))))
    (let* (selected-item
           (collection2 (if (hash-table-p collection)
                            (loop for key being the hash-keys of collection using (hash-values val)
                                  if (or (not predicate)
                                         (funcall predicate key val))
                                  collect key)
                          (if predicate (remove-if-not predicate collection) collection)))
           (collection3 (remq nil (mapcar 'tostring collection2)))
           (commands (mapcar (lambda (choice)
                               `(lambda nil
                                  (interactive)
                                  (setq selected-item ,choice)))
                             collection3))
           (menu-alists (one-key-create-menu-lists commands collection3))
           (nummenus (length menu-alists))
           (names (if (> nummenus 1)
                      (one-key-append-numbers-to-menu-name prompt nummenus)
                    (list prompt))))
      (one-key-menu names menu-alists
                    :miss-match-action (if require-match nil 'close)
                    :title-string (or title-string "")
                    :special-keybinding-symbols special-keys)
      (or selected-item def))))

(defun* one-key-read-tree-1 (prompt collection
                                    &optional maxdepth (depth 0)
                                    (displayfunc 'one-key-read-tree-display-func)
                                    (special-keys '(quit-close quit-open toggle-display next-menu prev-menu up down scroll-down
                                                               scroll-up toggle-help documentation toggle-row/column-order
                                                               sort-next sort-prev reverse-order limit-items donate
                                                               report-bug read-tree-down read-tree-down2 read-tree-up read-tree-up2
                                                               read-tree-delete)))
  "Recursive function for reading trees/recursive lists from the user.
NOTE: This function is called by `one-key-read-tree', `one-key-read-multiple', and `one-key-read-dnf'.
It assumes dynamic binding of okr-title-string which is used by displayfunc to set the title string for the one-key menu.

The PROMPT and COLLECTION arguments are as in `one-key-read'.
MAXDEPTH is the maximum depth allowed for the tree (toplevel has depth 0), and DEPTH is the current depth of the tree.

DISPLAYFUNC should be a function that takes four arguments, choice, depth, maxdepth and tree, where choice is the last item chosen,
depth and maxdepth are the previously mentioned arguments of the same name, and tree is the current subtree. It is called every
time a choice is selected from the one-key menu and should return a value to be added to the tree (the return value of
`one-key-read-tree-1'). It may also make alterations to the dynamically bound variable okr-title-string, a string which is
displayed at the top of the one-key menu.
If the user presses a special key to complete a sublist, start a new sublist, or delete the previous item in the list then the
DISPLAYFUNC function will be called with the choice arg set to the symbol 'goup, 'godown or 'del respectively, and the return
value will not be used. The DISPLAYFUNC function will also be called when a sublist is completed, with the choice arg set to
that list. In this case you may want to make alterations to the sublist before returning it, but not necessarily update the title
string. See `one-key-read-tree-display-func' and `one-key-read-dnf-display-func' for examples."
  (let* ((choice t) tree)
    (while choice
      (setq choice (one-key-read prompt collection nil nil nil okr-title-string special-keys))
      (setq newchoice (funcall displayfunc choice depth maxdepth tree))
      (case choice
        ('godown (if (and maxdepth (>= depth maxdepth))
                     (message "Can't go down any further!")
                   (setq tree (append tree (list (funcall displayfunc 
                                                          (one-key-read-tree-1 prompt collection maxdepth
                                                                               (1+ depth) displayfunc special-keys)
                                                          depth maxdepth tree))))))
        ('goup (setq choice nil))
        ('del (setq tree (butlast tree)))
        (t (if choice (setq tree (append tree (list newchoice)))
             (setq choice t)
             (message "Invalid choice")))))
    tree))

(defun* one-key-read-tree (prompt collection &optional maxdepth actionfunc returntitle)
  "Function for reading recursive lists from the user with a one-key menu.
PROMPT and COLLECTION are as in `one-key-read'.
The optional argument MAXDEPTH is the maximum depth allowed for the tree (toplevel has depth 0).
By default no bound is placed on the maximum depth.

If ACTIONFUNC is supplied it should be a function that takes three arguments: the currently selected item (a string or a symbol,
see below), the current tree depth, and maxdepth (in that order). The function may perform some actions and should return an item
to be added to the tree. The function is called each time an item is selected/deleted, or when the user goes up/down a level.
For its first argument the function is passed the name of an item when an item is selected, the symbol 'del when an item is deleted,
'goup when the user goes up a level and 'godown when the user goes down a level.

If RETURNTITLE is non-nil then return a cons cell whose car is the final title string used in the one-key menu, and whose cdr is
the tree. Otherwise just return the tree."
  (let* ((okr-title-string (concat (one-key-center-string
                                    (concat
                                     "Press "
                                     (caar (one-key-get-special-key-contents 'read-tree-up))
                                     " to complete a list, "
                                     (caar (one-key-get-special-key-contents 'read-tree-down))
                                     " to start a new list, and "
                                     (caar (one-key-get-special-key-contents 'read-tree-delete))
                                     " to remove the last element.")) "\n"
                                     (one-key-center-string "Current depth=0, Current selection:") "\n"))
         (tree (one-key-read-tree-1 prompt collection maxdepth 0
                                    (apply-partially 'one-key-read-tree-display-func actionfunc))))
    (string-match ".*\n.*\n *\\(.*[^ ]\\) *) *$" okr-title-string)
    (if returntitle (cons (match-string 1 okr-title-string) tree) tree)))

(defun* one-key-read-list (prompt collection &optional actionfunc returntitle)
  "one-key replacement for the built-in `completing-read-multiple' function.
See `one-key-read-tree' for a description of the arguments."
  (let* ((okr-title-string (concat (one-key-center-string
                                    (concat
                                     "Press "
                                     (caar (one-key-get-special-key-contents 'read-tree-up))
                                     " to finish selecting, and "
                                     (caar (one-key-get-special-key-contents 'read-tree-delete))
                                     " to remove the last element.")) "\n"
                                     (one-key-center-string "Current selection:") "\n"))
         (list1 (one-key-read-tree-1 prompt collection 0 0
                                     (apply-partially 'one-key-read-tree-display-func actionfunc))))
    (string-match ".*\n.*\n *\\(.*[^ ]\\) *) *$" okr-title-string)
    (if returntitle (cons (match-string 1 okr-title-string) list1) list1)))

(defun* one-key-read-logical-formula (prompt collection &optional actionfunc returntitle)
  "Function for reading logical formula from the user with a one-key menu.
The result is returned as a recursive list, with 'and' or 'or' symbols as the first element of each list depending on the depth.
See `one-key-read-tree' for a description of the arguments."
  (let* ((okr-title-string (concat (one-key-center-string
                                    (concat
                                     "Press "
                                     (caar (one-key-get-special-key-contents 'read-tree-up))
                                     " to close bracket, "
                                     (caar (one-key-get-special-key-contents 'read-tree-down))
                                     " to open bracket, "
                                     (caar (one-key-get-special-key-contents 'read-tree-delete))
                                     " to remove last element, and "
                                     (caar (one-key-get-special-key-contents 'read-logical-negate))
                                     " to negate next element.")) "\n"
                                     (one-key-center-string "Current selection:") "\n"))
         (form1 (one-key-read-tree-1 prompt collection nil 0
                                     (apply-partially 'one-key-read-logical-formula-display-func actionfunc)
                                     '(quit-close quit-open toggle-display next-menu prev-menu up down scroll-down
                                                  scroll-up toggle-help documentation toggle-row/column-order
                                                  sort-next sort-prev reverse-order limit-items donate
                                                  report-bug read-tree-down read-tree-down2 read-tree-up read-tree-up2
                                                  read-logical-negate read-tree-delete)))
         (form2 (if form1 (cons 'or form1))))
    (string-match ".*\n.*\n *\\(.*[^ ]\\) *) *$" okr-title-string)
    (if returntitle
        (cons (substring-no-properties (match-string 1 okr-title-string)) form2)
      form2)))

(defun one-key-read-tree-display-func (actionfunc choice depth maxdepth tree)
  "Default function used for displaying information in calls to `one-key-read-tree' (which see).
This function assumes dynamic binding of okr-title-string to the current title string of the one-key menu."
  (let* ((newstring (replace-regexp-in-string
                     "Current depth=\\([0-9]+\\)" (number-to-string depth)
                     (case choice
                       ('goup (concat okr-title-string " )"))
                       ('godown (if (or (not maxdepth) (< depth maxdepth)) (concat okr-title-string " (") okr-title-string))
                       ('del (replace-regexp-in-string ".*\n.*\n.*\\(([^()]*) *$\\|[^ ()]+ *$\\)" "" okr-title-string nil nil 1))
                       ;; if choice is a list then it has already been displayed in a recursive call
                       (t (if (and choice (not (listp choice)))
                              (concat okr-title-string " " choice) okr-title-string)))
                     nil nil 1)))
    (string-match "\\(.*\n.*\n\\) *\\(.*[^ ]+\\)" newstring)
    (setq okr-title-string (concat (match-string 1 newstring)
                                   (replace-regexp-in-string " *$" ""
                                                             (one-key-center-string (match-string 2 newstring))))))
  (if actionfunc (funcall actionfunc choice depth maxdepth) choice))

(defun one-key-read-logical-formula-display-func (actionfunc choice depth maxdepth tree)
  "Default function used for displaying information in calls to `one-key-read-logical-formula'.
This function assumes dynamic binding of okr-title-string to the current title string of the one-key menu."
  (let* ((isor (= (% depth 2) 0))
         (negated (eq (car (last tree)) 'not))
         (seperator (unless (string-match ".*\n.*\n\\(.*(\\|.*!\\)? *$" okr-title-string)
                      (if isor " | " " & ")))
         (newstring (case choice
                      ('goup (concat okr-title-string ")"))
                      ('godown (concat okr-title-string seperator "("))
                      ;; lots of regexp trickery required here
                      ('del (with-temp-buffer
                              (insert okr-title-string)
                              (if (= (char-before) 41)
                                  (backward-sexp))
                              (if (string-match "^.*\n.*\n *$" (buffer-substring-no-properties 1 (point)))
                                  okr-title-string
                                (re-search-backward "\n *\\| *|\\| *&\\|(")
                                (if (or (= (char-after) 40)
                                        (= (char-after) 10))
                                    (forward-char) (backward-char))
                                (buffer-substring 1 (point)))))
                      ('not (concat okr-title-string seperator "!"))
                      ;; if choice is a list then it has already been displayed in a recursive call                      
                      (t (if (and choice (not (listp choice)))
                             (concat okr-title-string seperator choice) okr-title-string))))
         (retchoice (if (not choice) (if isor nil t)
                      (if (listp choice)
                          (cons (if isor 'and 'or) choice)
                        (if actionfunc (funcall actionfunc choice depth maxdepth) choice)))))
    (string-match "\\(.*\n.*\n\\) *\\(.*[^ ]+\\)" newstring)
    (setq okr-title-string (if (match-string 2 newstring)
                               (concat (match-string 1 newstring)
                                       (replace-regexp-in-string
                                        " *$" "" (one-key-center-string (match-string 2 newstring))))
                             newstring))
    (if (not negated) retchoice
      (unless (eq choice 'godown)
        (if (equal (length tree) 1) (setcar tree (if isor nil t))
          (setf (nthcdr (1- (length tree)) tree) nil)))
      (list 'not retchoice))))

;; Set one-key menu types
(one-key-add-to-alist 'one-key-types-of-menu
                      (list "top-level"
                            (lambda (name) (equal name "top-level"))
                            (cons "top-level" 'one-key-menu-toplevel-alist)
                            nil nil) t)
(one-key-add-to-alist 'one-key-types-of-menu
                      (list "blank menu"
                            (lambda (name) (equal name "blank menu"))
                            'one-key-create-blank-menu
                            nil nil) t)
(one-key-add-to-alist 'one-key-types-of-menu
                      (list "major-mode"
                            (lambda (name) (string-match "^major-mode" name))
                            'one-key-get-major-mode-menu
                            nil 'one-key-major-mode-special-keybindings) t)
(one-key-add-to-alist 'one-key-types-of-menu
                      (list "existing menu"
                            (lambda (name) (equal name "existing menu"))
                            'one-key-retrieve-existing-menu
                            nil nil) t)
(one-key-add-to-alist 'one-key-types-of-menu
                      (list "keymap"
                            (lambda (name) (string-match "^keymap" name))
                            'one-key-create-menu-from-existing-keymap
                            nil nil) t)
(one-key-add-to-alist 'one-key-types-of-menu
                      (list "prefix-key"
                            (lambda (name) (string-match "^prefix-key" name))
                            (lambda (name)
                              (let* ((keystr1 (if (> (length name) 11) (substring name 11) nil))
                                     (keystr2 (if keystr1 (replace-regexp-in-string "_" "" keystr1))))
                                (if keystr2
                                    (one-key-create-menu-from-prefix-key-keymap keystr2)
                                  (call-interactively 'one-key-create-menu-from-prefix-key-keymap))))
                            nil nil) t)
(one-key-add-to-alist 'one-key-types-of-menu
                      (list "menu-sets"
                            (lambda (name) (equal name "menu-sets"))
                            (lambda (name)
                              (cons name (one-key-build-menu-sets-menu-alist)))
                            'one-key-create-menu-sets-title-format-string
                            'one-key-menu-sets-special-keybindings) t)

;; add function for autosaving menus to kill-emacs-hook
(add-hook 'kill-emacs-hook (lambda nil (if one-key-autosave-menus (one-key-save-altered-menus))))

;; Load the saved one-key menus.
(if one-key-menus-save-file
    (if (file-readable-p one-key-menus-save-file)
        (load-file one-key-menus-save-file)
      (message "Can't read file %s" one-key-menus-save-file))
  (message "`one-key-menus-save-file' is not set, no menus loaded"))


(provide 'one-key)

;;; one-key.el ends here

;; (magit-push)
;; (yaoddmuse-post "EmacsWiki" "one-key.el" (buffer-name) (buffer-string) "update")

