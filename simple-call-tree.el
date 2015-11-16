;;; simple-call-tree.el --- analyze source code based on font-lock text-properties

;; Filename: simple-call-tree.el
;; Description: analyze source code based on font-lock text-properties
;; Author: Joe Bloggs <vapniks@yahoo.com>
;; Maintainer: Joe Bloggs <vapniks@yahoo.com>
;; Copyleft (Ↄ) 2012, Joe Bloggs, all rites reversed.
;; Created: 2012-11-01 21:28:07
;; Version: 20151116.1603
;; Last-Updated: Mon Nov 16 16:03:18 2015
;;           By: Joe Bloggs
;; URL: http://www.emacswiki.org/emacs/download/simple-call-tree.el
;;      https://github.com/vapniks/simple-call-tree
;; Keywords: programming
;; Compatibility: GNU Emacs 24.2.1
;;
;; Features that might be required by this library:
;;
;; `anaphora' `thingatpt' `outshine' `fm' `org' `cl' `ido' `newcomment'
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
;; along with this program; see the file COPYING.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary: 
;;
;; Bitcoin donations gratefully accepted: 1AmWPmshr6i9gajMi1yqHgx7BYzpPKuzMz

;;;; Introduction:
;; This library is based on simple-call-tree.el by Alex Schroeder, but you
;; do not need that library to use it (this is a replacement).
;; It displays a buffer containing a call tree for functions in source
;; code files. You can easily & quickly navigate the call tree, displaying
;; the code in another window, and apply query-replace or other commands
;; to individual functions.

;; When the command `simple-call-tree-display-buffer' is executed
;; a call tree for the functions in the current buffer will be created.
;; The user is also prompted for other files to include in the call tree.
;; The call tree is displayed in a buffer called *Simple Call Tree*,
;; which has a dedicated menu in the menu-bar showing various commands
;; and their keybindings. Most of these commands are self explanatory
;; so try them out.

;;;; Navigation:
;; You can navigate the call tree either by moving through consecutive
;; headers (n/p or N/P keys) or by jumping to main branches (j for branch
;; corresponding to function at point, and J to prompt for a function).
;; When you jump to a branch, it is added to `simple-call-tree-jump-ring',
;; and you can navigate your jump history using the </> keys.
;; You can also add the function under point to the jump-ring with the . key.
;; If you use a negative prefix (e.g. C--) before pressing j then the branch
;; jumped to will not be added to the jump-ring.
;; If you have fm.el (available here: http://www.damtp.cam.ac.uk/user/sje30/emacs/fm.el)
;; you can press f to toggle follow mode on/off.

;;;; Display
;; Normally child branches correspond to functions/variables called by the parent
;; branch. However, if you invert the tree by pressing i then the child branches
;; will correspond to functions that call the parent branch.
;; You can sort the tree in various different ways, and change the depth of the tree.
;; You can also narrow the tree to the function at point by pressing /

;;;; Exporting:
;; The tree can be exported in its current state with the `simple-call-tree-export-org-tree'
;; command, and you can alter the types of links with the `simple-call-tree-org-link-style' option.
;; This may be useful for project management.

;;;; Refactoring
;; You can perform `query-replace' or `query-replace-regexp' on the function at
;; point by pressing % or C-%, or any other arbitrary command by pressing !
;; This may be useful when refactoring.

;;; Commands:
;;
;; Below is a complete list of commands:
;;
;;  `simple-call-tree-mode'
;;    The major-mode for the one-key menu buffer.
;;    Keybinding: M-x simple-call-tree-mode
;;  `simple-call-tree-next-todo'
;;    Move to next todo state for current function.
;;    Keybinding: <S-right>
;;  `simple-call-tree-prev-todo'
;;    Move to previous todo state for current function.
;;    Keybinding: <S-left>
;;  `simple-call-tree-up-priority'
;;    Change current function to the next priority level.
;;    Keybinding: <S-up>
;;  `simple-call-tree-down-priority'
;;    Change current function to the previous priority level.
;;    Keybinding: <S-down>
;;  `simple-call-tree-add-tags'
;;    Add tags in VALUE to the function(s) FUNCS.
;;    Keybinding: C-c C-a
;;  `simple-call-tree-build-tree'
;;    Build the simple-call-tree and display it in the "*Simple Call Tree*" buffer.
;;    Keybinding: R
;;  `simple-call-tree-export-org-tree'
;;    Create an org-tree from the currently visible items, and put it in an org buffer.
;;    Keybinding: M-x simple-call-tree-export-org-tree
;;  `simple-call-tree-export-items'
;;    Export the currently visible items into a buffer.
;;    Keybinding: M-x simple-call-tree-export-items
;;  `simple-call-tree-reverse'
;;    Reverse the order of the branches & sub-branches in `simple-call-tree-alist' and `simple-call-tree-inverted-alist'.
;;    Keybinding: r
;;  `simple-call-tree-sort-by-num-descendants'
;;    Sort the branches in the *Simple Call Tree* buffer by the number of descendants to depth DEPTH.
;;    Keybinding: d
;;  `simple-call-tree-sort-by-name'
;;    Sort the functions in the *Simple Call Tree* buffer alphabetically.
;;    Keybinding: n
;;  `simple-call-tree-sort-by-position'
;;    Sort the functions in the *Simple Call Tree* buffer by position.
;;    Keybinding: p
;;  `simple-call-tree-sort-by-face'
;;    Sort the items in the *Simple Call Tree* buffer according to the display face name.
;;    Keybinding: f
;;  `simple-call-tree-sort-by-todo'
;;    Sort the items in the *Simple Call Tree* buffer by TODO state.
;;    Keybinding: T
;;  `simple-call-tree-sort-by-priority'
;;    Sort the items in the *Simple Call Tree* buffer by priority level.
;;    Keybinding: P
;;  `simple-call-tree-sort-by-size'
;;    Sort the items in the *Simple Call Tree* buffer by size.
;;    Keybinding: s
;;  `simple-call-tree-sort-by-mark'
;;    Sort the marked items in the *Simple Call Tree* buffer before the unmarked ones.
;;    Keybinding: *
;;  `simple-call-tree-quit'
;;    Quit the *Simple Call Tree* buffer.
;;    Keybinding: q
;;  `simple-call-tree-invert-buffer'
;;    Invert the tree in *Simple Call Tree* buffer.
;;    Keybinding: i
;;  `simple-call-tree-change-maxdepth'
;;    Alter the maximum tree depth in the *Simple Call Tree* buffer.
;;    Keybinding: M-x simple-call-tree-change-maxdepth
;;  `simple-call-tree-view-function'
;;    Display the source code corresponding to current header.
;;    Keybinding: C-o
;;  `simple-call-tree-jump-prev'
;;    Jump to the previous function in the `simple-call-tree-jump-ring'.
;;    Keybinding: <
;;  `simple-call-tree-jump-next'
;;    Jump to the next function in the `simple-call-tree-jump-ring'.
;;    Keybinding: >
;;  `simple-call-tree-jump-ring-add'
;;    Add the function at point to the jump-ring.
;;    Keybinding: .
;;  `simple-call-tree-move-top'
;;    Move cursor to the parent of this function.
;;    Keybinding: ^
;;  `simple-call-tree-move-next'
;;    Move cursor to the next item.
;;    Keybinding: M-x simple-call-tree-move-next
;;  `simple-call-tree-move-prev'
;;    Move cursor to the previous item.
;;    Keybinding: M-x simple-call-tree-move-prev
;;  `simple-call-tree-move-next-samelevel'
;;    Move cursor to the next item at the same level as the current one.
;;    Keybinding: C-f
;;  `simple-call-tree-move-prev-samelevel'
;;    Move cursor to the previous item at the same level as the current one.
;;    Keybinding: C-b
;;  `simple-call-tree-move-next-marked'
;;    Move cursor to the next marked item.
;;    Keybinding: M-n
;;  `simple-call-tree-move-prev-marked'
;;    Move cursor to the next marked item.
;;    Keybinding: M-p
;;  `simple-call-tree-toggle-narrowing'
;;    Toggle narrowing of *Simple Call Tree* buffer.
;;    Keybinding: /
;;  `simple-call-tree-toggle-duplicates'
;;    Toggle the inclusion of duplicate sub-branches in the call tree.
;;    Keybinding: D
;;  `simple-call-tree-query-replace'
;;    Perform query-replace on the marked items or the item at point in the *Simple Call Tree* buffer.
;;    Keybinding: %
;;  `simple-call-tree-query-replace-regexp'
;;    Perform `query-replace-regexp' on the marked items or the item at point in the *Simple Call Tree* buffer.
;;    Keybinding: C-%
;;  `simple-call-tree-bookmark'
;;    Set bookmarks the marked items or the item at point in the *Simple Call Tree* buffer.
;;    Keybinding: M-x simple-call-tree-bookmark
;;  `simple-call-tree-delete-other-windows'
;;    Make the *Simple Call Tree* buffer fill the frame.
;;    Keybinding: 1
;;  `simple-call-tree-mark'
;;    Mark the item named FUNC.
;;    Keybinding: m
;;  `simple-call-tree-unmark'
;;    Unmark the item named FUNC.
;;    Keybinding: u
;;  `simple-call-tree-unmark-all'
;;    Unmark all items.
;;    Keybinding: U
;;  `simple-call-tree-toggle-marks'
;;    Toggle marks (unmarked become marked and marked become unmarked).
;;    Keybinding: M-x simple-call-tree-toggle-marks
;;  `simple-call-tree-mark-by-name'
;;    Mark all items with names matching regular expression REGEX.
;;    Keybinding: M-x simple-call-tree-mark-by-name
;;  `simple-call-tree-mark-by-source'
;;    Mark all items with source code matching regular expression REGEX.
;;    Keybinding: M-x simple-call-tree-mark-by-source
;;  `simple-call-tree-mark-by-tag-match'
;;    Mark all items with code matching regular expression REGEX.
;;    Keybinding: t
;;  `simple-call-tree-mark-by-priority'
;;    Mark all items with priority VALUE.
;;    Keybinding: M-x simple-call-tree-mark-by-priority
;;  `simple-call-tree-mark-by-todo'
;;    Mark all items with TODO state matching regular expression REGEX.
;;    Keybinding: M-x simple-call-tree-mark-by-todo
;;  `simple-call-tree-mark-by-face'
;;    Mark all items with display face FACE.
;;    Keybinding: M-x simple-call-tree-mark-by-face
;;  `simple-call-tree-mark-by-buffer'
;;    Mark all items corresponding to source code in buffer BUF.
;;    Keybinding: b
;;  `simple-call-tree-kill-marked'
;;    Remove all marked items from the *Simple Call Tree* buffer.
;;    Keybinding: k
;;  `simple-call-tree-revert'
;;    Redisplay the *Simple Call Tree* buffer.
;;    Keybinding: g
;;
;;; Customizable Options:
;;
;; Below is a list of customizable options:
;;
;;  `simple-call-tree-default-recenter'
;;    How to recenter the window after moving to another function in the "*Simple Call Tree*" buffer.
;;    default = (quote middle)
;;  `simple-call-tree-default-valid-fonts'
;;    List of fonts to use for finding objects to include in the call tree.
;;    default = (quote (font-lock-function-name-face font-lock-variable-name-face))
;;  `simple-call-tree-default-invalid-fonts'
;;    List of fonts that should not be in the text property of any valid token.
;;    default = (quote (font-lock-comment-face font-lock-string-face font-lock-doc-face font-lock-keyword-face font-lock-warning-face ...))
;;  `simple-call-tree-default-sort-method'
;;    The default sort method to use when a call tree is newly created.
;;    default = (quote position)
;;  `simple-call-tree-default-maxdepth'
;;    The depth at which new call trees should be displayed.
;;    default = 2
;;  `simple-call-tree-major-mode-alist'
;;    Alist of major modes, and information to use for identifying objects for the simple call tree.
;;    default = (quote ((emacs-lisp-mode ... nil ... nil ...) (cperl-mode nil nil ... nil ...) (haskell-mode nil ... ... ... ...) (perl-mode nil nil ... nil ...) (python-mode ... nil ... nil ...) ...))
;;  `simple-call-tree-org-link-style'
;;    Style used for links of child headers when exporting org tree using `simple-call-tree-export-org-tree'.
;;    default = (quote radio)
;;  `simple-call-tree-org-todo-keywords'
;;    List of different TODO keywords, if nil then the keywords in `org-todo-keywords' will be used.
;;    default = nil
;;  `simple-call-tree-org-not-done-keywords'
;;    List of TODO keywords representing not done states.
;;    default = (quote ("TODO" "STARTED" "WAITING" "CHECK"))
;;  `simple-call-tree-org-highest-priority'
;;    See `org-highest-priority'.
;;    default = org-highest-priority
;;  `simple-call-tree-org-lowest-priority'
;;    See `org-lowest-priority'.
;;    default = org-lowest-priority
;;  `simple-call-tree-org-tag-alist'
;;    See `org-tag-alist'.
;;    default = org-tag-alist
;;  `simple-call-tree-mark-face'
;;    Face to use for marked items in the *Simple Call Tree* buffer.
;;    default = (if (featurep (quote dired+)) diredp-flag-mark-line (quote highlight))
;;  `simple-call-tree-jump-ring-max'
;;    Maximum number of elements in `simple-call-tree-jump-ring', before old elements are removed.
;;    default = 20

;;; Installation:
;;
;; Put simple-call-tree.el in a directory in your load-path, e.g. ~/.emacs.d/
;; You can add a directory to your load-path with the following line in ~/.emacs
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;; where ~/elisp is the directory you want to add 
;; (you don't need to do this for ~/.emacs.d - it's added by default).
;;
;; Add the following to your ~/.emacs startup file.
;;
;; (require 'simple-call-tree+)
;;
;; You might also want to define a key for creating the call tree, 
;; e.g. like this:
;;
;; (global-set-key (kbd "C-c S") 'simple-call-tree-display-buffer)

;;; Acknowledgements:
;;
;; Alex Schroeder - the creator of the original simple-call-tree.el
;;                  
;;

;;; TODO
;;
;; If anyone wants to implement the following ideas, please do:
;; More reliable code for building tree (handle duplicate function names properly).
;; Fix code so that we can have several calls to the same function in the same tree.
;; Code for managing refactoring commands (to be applied to marked functions).
;; Multiple query replace command for substituting appropriate lisp macros -
;; e.g. replace (setq list (remove x list)) with (callf2 remove x list)
;; Code for finding (and highlighting?) duplicate code and other code smells (difficult).
;; Idea: break each function down into a list of keywords for the language in question,
;; and look for common subsequences (not too short) among functions.

;;; Require
(require 'thingatpt)
(require 'outshine nil t)
(require 'fm nil t)
(require 'hl-line nil t)
(require 'anaphora)
(eval-when-compile (require 'cl))
(require 'org)
(require 'ido)
(require 'newcomment)
(comment-normalize-vars)
;;; Code:

;; simple-call-tree-info: DONE  
(defgroup simple-call-tree nil
  "Simple call tree - display a simple call tree for functions in a buffer."
  :group 'tools
  :link '(url-link "http://www.emacswiki.org/SimpleCallTree"))

;; simple-call-tree-info: DONE  
(defcustom simple-call-tree-default-recenter 'middle
  "How to recenter the window after moving to another function in the \"*Simple Call Tree*\" buffer.
Can be one of the following symbols: 'top 'middle 'bottom.
This variable is used by the `simple-call-tree-jump-to-function' function when no prefix arg is given,
and by `simple-call-tree-visit-function' and `simple-call-tree-view-function'."
  :group 'simple-call-tree
  :type '(choice (const :tag "Top" top)
                 (const :tag "Middle" middle)
                 (const :tag "Bottom" bottom)))

;; simple-call-tree-info: DONE  
(defcustom simple-call-tree-default-valid-fonts '(font-lock-function-name-face
                                                  font-lock-variable-name-face)
  "List of fonts to use for finding objects to include in the call tree."
  :group 'simple-call-tree
  :type '(repeat face))

;; simple-call-tree-info: DONE  
(defcustom simple-call-tree-default-invalid-fonts '(font-lock-comment-face
                                                    font-lock-string-face
                                                    font-lock-doc-face
                                                    font-lock-keyword-face
                                                    font-lock-warning-face
                                                    font-lock-preprocessor-face)
  "List of fonts that should not be in the text property of any valid token."
  :group 'simple-call-tree
  :type '(repeat face))

;; simple-call-tree-info: DONE  
(defcustom simple-call-tree-default-sort-method 'position
  "The default sort method to use when a call tree is newly created.
The children of each header will be sorted separately."
  :group 'simple-call-tree
  :type '(choice (const :tag "Sort by position" position)
                 (const :tag "Sort by name" name)
                 (const :tag "Sort by number of descendants" numdescend)
                 (const :tag "Sort by size" size)
                 (const :tag "Sort by face" face)
                 (const :tag "Sort by TODO state" todo)
                 (const :tag "Sort by priority" priority)
                 (const :tag "Sort by mark" mark)))

;; simple-call-tree-info: DONE  
(defcustom simple-call-tree-default-maxdepth 2
  "The depth at which new call trees should be displayed."
  :group 'simple-call-tree
  :type 'integer)

;; simple-call-tree-info: DONE  
(defcustom simple-call-tree-major-mode-alist
  '((emacs-lisp-mode (font-lock-function-name-face
                      font-lock-variable-name-face
                      font-lock-type-face)
                     nil
                     (lambda (pos)
                       (goto-char pos)
                       (backward-word)
                       (eq (get-text-property (point) 'face)
                           'font-lock-keyword-face))
                     nil end-of-defun)
    (cperl-mode nil nil (lambda (pos)
                          (goto-char pos)
                          (beginning-of-line)
                          (looking-at "sub")) nil nil)
    (haskell-mode nil (font-lock-function-name-face
                       font-lock-comment-face
                       font-lock-string-face
                       font-lock-doc-face
                       font-lock-keyword-face
                       font-lock-warning-face
                       font-lock-preprocessor-face)
                  (lambda (pos)
                    (goto-char pos)
                    (beginning-of-line)
                    (let ((thistoken (symbol-at-point)))
                      (previous-line)
                      (not (string= (symbol-at-point) thistoken))))
                  (lambda nil (haskell-ds-forward-decl)
                    (unless (= (point) (point-max)) (point)))
                  haskell-ds-forward-decl
                  "\\(:\\|\\_<\\)"
                  "\\s-")
    (perl-mode nil nil (lambda (pos)
                         (goto-char pos)
                         (beginning-of-line)
                         (looking-at "sub"))
               nil nil)
    (python-mode (font-lock-function-name-face
                  font-lock-variable-name-face
                  font-lock-type-face)
                 nil (lambda (pos)
                       (goto-char pos)
                       (beginning-of-line)
                       (re-search-forward "\\<")
                       (or (looking-at "def\\|class")
                           (eq (get-text-property (point) 'face)
                               font-lock-variable-name-face)))
                 nil
                 (lambda nil
                   (backward-char)
                   (if (eq (get-text-property (point) 'face)
                           font-lock-variable-name-face)
                       (end-of-line)
                     (end-of-defun))))
    (matlab-mode (font-lock-function-name-face)
                 nil
                 (lambda (pos)
                   (re-search-backward "^function" (line-beginning-position) t))
                 (lambda nil
                   (if (re-search-forward "^function\\s-*\\S-+\\s-*=\\s-*" (point-max) t)
                       (point)))
                 matlab-end-of-defun))
  "Alist of major modes, and information to use for identifying objects for the simple call tree.
Each element is a list in the form '(MAJOR-MODE VALID-FONTS INVALID-FONTS START-TEST END-TEST) where:

MAJOR-MODE is the symbol for the major-mode that this items applies to.

VALID-FONTS is either nil or a list of fonts for finding objects for the call tree (functions/variables/etc).
If nil then `simple-call-tree-default-valid-fonts' will be used.

INVALID-FONTS is either nil or a list of fonts that should not be present in the text properties of
any objects to be added to the call tree. If nil then `simple-call-tree-default-invalid-fonts' will be used.
The invalid fonts will also be checked when finding function calls.

START-TEST indicates how to determing the start of the next object. Potential objects are found by checking
the fonts of tokens in the buffer (against VALID-FONTS). If START-TEST is a function then it will be used as
an additional check for potential objects. It will be passed the buffer position of the beginning of the
current token to check, and should return non-nil if it represents a valid object.

NEXT-FUNC is an alternative way of determining the locations of functions.
It is either nil, meaning the function locations will be determined by fonts and maybe START-TEST,
or a function of no args which returns the position of the next function in the buffer, or nil if
there are no further functions.

END-FUNC indicates how to find the end of the current object when parsing a buffer for the call tree.
It can be either a function taking no args which moves point to the end of the current function,
or any non-nil value which means to use the `end-of-defun' function, or nil which means that font changes
will be used to determine the end of an object.

START-REGEXP a regular expression to match the beginning of a token, you can probably leave this blank.
By default it is \"\\_<\".

END-REGEXP a regular expression to match the end of a token, by default this is \"\\_>\"."
  :group 'simple-call-tree
  :type '(repeat (list (symbol :tag "major-mode symbol")
                       (repeat :tag "Faces"
                               (face :help-echo "List of faces corresponding to items to include in the call tree."))
                       (choice :tag "Start test"
                               (const :tag "Font only" nil)
                               (function
                                :tag "Function:"
                                :help-echo "Function that evaluates to true when point is at beginning of function name."))
                       (choice :tag "End test"
                               (const :tag "Font only" nil)
                               (const :tag "end-of-defun function" t)
                               (function :tag "Other function" :help-echo "Function for finding end of object"))))
  :link '(variable-link simple-call-tree-default-valid-fonts))

;; simple-call-tree-info: DONE  
(defcustom simple-call-tree-org-link-style 'radio
  "Style used for links of child headers when exporting org tree using `simple-call-tree-export-org-tree'."
  :group 'simple-call-tree
  :type '(choice (const :tag "internal radio link" radio)
                 (const :tag "link to source code" source)))

;; simple-call-tree-info: DONE  
(defcustom simple-call-tree-org-todo-keywords nil
  "List of different TODO keywords, if nil then the keywords in `org-todo-keywords' will be used."
  :group 'simple-call-tree
  :type '(choice (repeat :tag "Choose keywords" (string :tag "Keyword"))
                 (const :tag "Use org-todo-keywords" nil)))

;; simple-call-tree-info: DONE  
(defcustom simple-call-tree-org-not-done-keywords '("TODO" "STARTED" "WAITING" "CHECK")
  "List of TODO keywords representing not done states."
  :group 'simple-call-tree
  :type '(repeat :tag "Choose keywords" (string :tag "Keyword")))

;; simple-call-tree-info: DONE  
(defcustom simple-call-tree-org-highest-priority org-highest-priority
  "See `org-highest-priority'."
  :group 'simple-call-tree
  :type 'character)

;; simple-call-tree-info: DONE  
(defcustom simple-call-tree-org-lowest-priority org-lowest-priority
  "See `org-lowest-priority'."
  :group 'simple-call-tree
  :type 'character)

;; simple-call-tree-info: DONE  
(defcustom simple-call-tree-org-tag-alist org-tag-alist
  "See `org-tag-alist'."
  :group 'simple-call-tree
  :type '(alist :key-type (string :tag "Tag name")
                :value-type (character :tag "Access char")))

;; simple-call-tree-info: DONE  
(defun simple-call-tree-org-todo-keywords nil
  "Return list of all TODO states.
If `simple-call-tree-org-todo-keywords' is nil then the states in `org-todo-keywords' are returned
as a flat list."
  (or simple-call-tree-org-todo-keywords
      (remove-if (lambda (x) (or (symbolp x)
                                 (equal x "|")))
                 (mapcan 'identity org-todo-keywords))))

;; Saves a little typing
;; simple-call-tree-info: DONE  
(defmacro whilelast (&rest forms)
  `(while (progn ,@forms)))

;; simple-call-tree-info: DONE  
(defmacro whilenotlast (&rest forms)
  `(while (not (progn ,@forms))))

;; simple-call-tree-info: DONE  
(cl-defun simple-call-tree-get-item (func &optional (alist simple-call-tree-alist))
  "Return the item in `simple-call-tree-alist' corresponding with function named FUNC."
  (assoc-if (lambda (x) (simple-call-tree-compare-items (car x) func)) alist))

;; simple-call-tree-info: DONE
(defun simple-call-tree-tags-to-string (tags)
  (if (> (length tags) 0)
      (concat ":" (mapconcat (lambda (x)
                               (propertize x 'font-lock-face (org-get-tag-face x)))
                             tags ":") ":") ""))

;; simple-call-tree-info: DONE  
(defun simple-call-tree-string-to-tags (str)
  (aif str (split-string (substring-no-properties it) "[ \f\t\n\r\v,;:]+" t)))

;; Major-mode for simple call tree
;; simple-call-tree-info: DONE  
(define-derived-mode simple-call-tree-mode outline-mode "Simple Call Tree"
  "The major-mode for the one-key menu buffer."
  :group 'simple-call-tree
  (setq simple-call-tree-mode-map (make-keymap)
        buffer-read-only nil)
  (outline-minor-mode 1)
  (setq outline-regexp "^[|*]\\([-<>]*\\)\\(\\( +\\w+\\)?\\)\\(\\( \\[#.\\]\\)?\\) "
        outline-level 'simple-call-tree-outline-level)
  ;; Define keys
  (define-key simple-call-tree-mode-map (kbd "q") 'simple-call-tree-quit)
  ;; Sorting commands
  (define-prefix-command 'simple-call-tree-sort-map)
  (define-key simple-call-tree-mode-map (kbd "s") 'simple-call-tree-sort-map)
  (define-key simple-call-tree-mode-map (kbd "s n") 'simple-call-tree-sort-by-name)
  (define-key simple-call-tree-mode-map (kbd "s p") 'simple-call-tree-sort-by-position)
  (define-key simple-call-tree-mode-map (kbd "s f") 'simple-call-tree-sort-by-face)
  (define-key simple-call-tree-mode-map (kbd "s d") 'simple-call-tree-sort-by-num-descendants)
  (define-key simple-call-tree-mode-map (kbd "s s") 'simple-call-tree-sort-by-size)
  (define-key simple-call-tree-mode-map (kbd "s T") 'simple-call-tree-sort-by-todo)
  (define-key simple-call-tree-mode-map (kbd "s P") 'simple-call-tree-sort-by-priority)
  (define-key simple-call-tree-mode-map (kbd "s *") 'simple-call-tree-sort-by-mark)  
  (define-key simple-call-tree-mode-map (kbd "s r") 'simple-call-tree-reverse)
  (define-key simple-call-tree-mode-map (kbd "<C-S-up>") 'simple-call-tree-move-item-up)
  (define-key simple-call-tree-mode-map (kbd "<C-S-down>") 'simple-call-tree-move-item-down)
  ;; Display altering commands
  (define-key simple-call-tree-mode-map (kbd "i") 'simple-call-tree-invert-buffer)
  (define-key simple-call-tree-mode-map (kbd "d") 'simple-call-tree-change-maxdepth)
  (define-key simple-call-tree-mode-map (kbd "D") 'simple-call-tree-toggle-duplicates)
  (define-key simple-call-tree-mode-map (kbd "g") 'simple-call-tree-revert)
  (define-key simple-call-tree-mode-map (kbd "R") 'simple-call-tree-build-tree)
  ;; Marking commands
  (define-prefix-command 'simple-call-tree-mark-map)
  (define-key simple-call-tree-mode-map (kbd "*") 'simple-call-tree-mark-map)
  (define-key simple-call-tree-mode-map (kbd "* n") 'simple-call-tree-mark-by-name)
  (define-key simple-call-tree-mode-map (kbd "* f") 'simple-call-tree-mark-by-face)
  (define-key simple-call-tree-mode-map (kbd "* s") 'simple-call-tree-mark-by-source)
  (define-key simple-call-tree-mode-map (kbd "* T") 'simple-call-tree-mark-by-todo)
  (define-key simple-call-tree-mode-map (kbd "* p") 'simple-call-tree-mark-by-priority)
  (define-key simple-call-tree-mode-map (kbd "* t") 'simple-call-tree-mark-by-tag-match)
  (define-key simple-call-tree-mode-map (kbd "* b") 'simple-call-tree-mark-by-buffer)
  (define-key simple-call-tree-mode-map (kbd "m") 'simple-call-tree-mark)
  (define-key simple-call-tree-mode-map (kbd "u") 'simple-call-tree-unmark)
  (define-key simple-call-tree-mode-map (kbd "U") 'simple-call-tree-unmark-all)
  (define-key simple-call-tree-mode-map (kbd "t") 'simple-call-tree-toggle-marks)
  ;; Hide/show commands
  (if (featurep 'outshine)
      (define-key simple-call-tree-mode-map (kbd "<tab>") 'outline-cycle)
    (define-key simple-call-tree-mode-map (kbd "<tab>") 'outline-toggle-children))
  (define-key simple-call-tree-mode-map (kbd "<right>") 'show-children)
  (define-key simple-call-tree-mode-map (kbd "<left>") 'hide-subtree)
  (define-key simple-call-tree-mode-map (kbd "a") 'show-all)
  (define-key simple-call-tree-mode-map (kbd "1") 'simple-call-tree-delete-other-windows)
  (define-key simple-call-tree-mode-map (kbd "h") 'hide-sublevels)
  (define-key simple-call-tree-mode-map (kbd "w") 'widen)
  (define-key simple-call-tree-mode-map (kbd "/") 'simple-call-tree-toggle-narrowing)
  ;; View/visit commands
  (define-key simple-call-tree-mode-map (kbd "SPC") 'simple-call-tree-view-function)
  (define-key simple-call-tree-mode-map (kbd "C-o") 'simple-call-tree-view-function)
  (define-key simple-call-tree-mode-map (kbd "v") 'simple-call-tree-view-function)
  (define-key simple-call-tree-mode-map (kbd "<return>") 'simple-call-tree-visit-function)
  (define-key simple-call-tree-mode-map (kbd "o") 'simple-call-tree-visit-function)
  ;; Movement commands
  (define-key simple-call-tree-mode-map (kbd "^") 'simple-call-tree-move-top)
  (define-key simple-call-tree-mode-map (kbd "n") 'simple-call-tree-move-next)
  (define-key simple-call-tree-mode-map (kbd "p") 'simple-call-tree-move-prev)
  (define-key simple-call-tree-mode-map (kbd "C-f") 'simple-call-tree-move-next-samelevel)
  (define-key simple-call-tree-mode-map (kbd "C-b") 'simple-call-tree-move-prev-samelevel)
  (define-key simple-call-tree-mode-map (kbd "N") 'simple-call-tree-move-next-samelevel)
  (define-key simple-call-tree-mode-map (kbd "P") 'simple-call-tree-move-prev-samelevel)
  (define-key simple-call-tree-mode-map (kbd "M-g n") 'simple-call-tree-move-next-marked)
  (define-key simple-call-tree-mode-map (kbd "M-g p") 'simple-call-tree-move-prev-marked)
  (define-key simple-call-tree-mode-map (kbd "M-g M-n") 'simple-call-tree-move-next-marked)
  (define-key simple-call-tree-mode-map (kbd "M-g M-p") 'simple-call-tree-move-prev-marked)
  (define-key simple-call-tree-mode-map (kbd "M-p") 'simple-call-tree-move-prev-marked)
  (define-key simple-call-tree-mode-map (kbd "M-n") 'simple-call-tree-move-next-marked)
  ;; Jump ring commands
  (define-key simple-call-tree-mode-map (kbd "j") 'simple-call-tree-jump-to-function)
  (define-key simple-call-tree-mode-map (kbd "J") '(lambda nil (interactive)
                                                     (setq current-prefix-arg 1)
                                                     (call-interactively 'simple-call-tree-jump-to-function)))
  (define-key simple-call-tree-mode-map (kbd "C-j") '(lambda nil (interactive)
                                                       (setq current-prefix-arg 1)
                                                       (call-interactively 'simple-call-tree-jump-to-function)))
  (define-key simple-call-tree-mode-map (kbd ".") 'simple-call-tree-jump-ring-add)
  (define-key simple-call-tree-mode-map (kbd "<") 'simple-call-tree-jump-prev)
  (define-key simple-call-tree-mode-map (kbd ">") 'simple-call-tree-jump-next)
  ;; Applying commands
  (define-key simple-call-tree-mode-map (kbd "b") 'simple-call-tree-bookmark)
  (define-key simple-call-tree-mode-map (kbd "%") 'simple-call-tree-query-replace)
  (define-key simple-call-tree-mode-map (kbd "C-%") 'simple-call-tree-query-replace-regexp)
  (define-key simple-call-tree-mode-map (kbd "!") 'simple-call-tree-apply-command)
  (define-key simple-call-tree-mode-map (kbd "k") 'simple-call-tree-kill-marked)
  ;; org attribute commands
  (define-key simple-call-tree-mode-map (kbd "<S-right>") 'simple-call-tree-next-todo)
  (define-key simple-call-tree-mode-map (kbd "<S-left>") 'simple-call-tree-prev-todo)
  (define-key simple-call-tree-mode-map (kbd "<S-up>") 'simple-call-tree-up-priority)
  (define-key simple-call-tree-mode-map (kbd "<S-down>") 'simple-call-tree-down-priority)
  (define-key simple-call-tree-mode-map (kbd "C-c C-c") 'simple-call-tree-set-tags)
  (define-key simple-call-tree-mode-map (kbd "C-c C-q") 'simple-call-tree-set-tags)
  (define-key simple-call-tree-mode-map (kbd "C-c C-a") 'simple-call-tree-add-tags)
  (define-key simple-call-tree-mode-map (kbd "C-c C-t") 'simple-call-tree-set-todo)
  (define-key simple-call-tree-mode-map (kbd "C-c ,") 'simple-call-tree-set-priority)
  ;; Set the keymap
  (use-local-map simple-call-tree-mode-map)
  ;; Menu definition
  (easy-menu-define nil simple-call-tree-mode-map "test"
    `("Simple Call Tree"
      ["Quit" simple-call-tree-quit
       :help "Quit and bury this buffer"]
      ["Rebuild tree" simple-call-tree-build-tree
       :help "Rebuild the call tree"]
      ["View Function At Point" simple-call-tree-view-function
       :help "View the function at point"
       :key "v"]
      ["Visit Function At Point" simple-call-tree-visit-function
       :help "Visit the function at point"]
      ["Sort items..." (keymap "Sort"
                               (name menu-item "By name" simple-call-tree-sort-by-name)
                               (position menu-item "By position" simple-call-tree-sort-by-position)
                               (numdescend menu-item "By number of descendants" simple-call-tree-sort-by-num-descendants)
                               (size menu-item "By size" simple-call-tree-sort-by-size)                       
                               (face menu-item "By face/type" simple-call-tree-sort-by-face)
                               (todo menu-item "By TODO state" simple-call-tree-sort-by-todo)
                               (priority menu-item "By priority" simple-call-tree-sort-by-priority)
                               (mark menu-item "By mark" simple-call-tree-sort-by-mark)
                               (reverse menu-item "Reverse order" simple-call-tree-reverse)
                               (moveup menu-item "Move item up" simple-call-tree-move-item-up)
                               (movedown menu-item "Move item down" simple-call-tree-move-item-down))]
      ["Mark items..."
       (keymap "Mark"
               (mark menu-item "Mark current item" simple-call-tree-mark
                     :help "Mark toplevel item at point")
               (unmark menu-item "Unmark current item" simple-call-tree-unmark
                       :help "Unmark the toplevel item at point")
               (unmarkall menu-item "Unmark all items" simple-call-tree-unmark-all
                          :help "Unmark all marked items")
               (toggle menu-item "Toggle marks" simple-call-tree-toggle-marks
                       :help "Toggle marks: marked files become unmarked, and vice versa.")
               (name menu-item "By name match (regexp)..." simple-call-tree-mark-by-name
                     :help "Mark items with names matching regexp")
               (source menu-item "By source code match (regexp)..." simple-call-tree-mark-by-source
                       :help "Mark items with source code matching regexp")
               (todo menu-item "By TODO state (regexp)..." simple-call-tree-mark-by-todo
                     :help "Mark items with matching TODO state")
               (priority menu-item "By priority..." simple-call-tree-mark-by-priority
                         :help "Mark items with matching priority level")
               (tagmatch menu-item "By tag-match..." simple-call-tree-mark-by-tag-match
                         :help "Mark items with matching tags")
               (buffer menu-item "By source buffer..." simple-call-tree-mark-by-buffer
                         :help "Mark items with source code in matching buffer"))]
      ["Operate on items..."
       (keymap "Operate"
               (kill menu-item "Kill marked items" simple-call-tree-kill
                     :help "Remove marked items from the buffer")
               (todo menu-item "Set TODO state..." simple-call-tree-set-todo
                     :help "Set TODO state of current/marked items (with prefix arg remove TODO)")
               (priority menu-item "Set priority level..." simple-call-tree-set-priority
                         :help "Set priority level of current/marked items (with prefix arg remove priority)")
               (tags menu-item "Set tags..." simple-call-tree-set-tags
                     :help "Set tags for current/marked items")
               (addtags menu-item "Add tags..." simple-call-tree-add-tags
                        :help "Add tags to current/marked items (with prefix arg remove tags)")
               (queryreplace menu-item "Replace String..." simple-call-tree-query-replace
                             :help "Perform query-replace on the function at point")
               (queryreplaceregex menu-item "Replace Regexp..." simple-call-tree-query-replace-regexp
                                  :help "Perform query-replace-regexp on the function at point")
               (bookmark menu-item "Add Bookmark" simple-call-tree-bookmark
                         :help "Create a bookmark for the position corresponding to the function at point")
               (arbitrary menu-item "Apply Arbitrary Command..." simple-call-tree-apply-command
                          :help "Apply an arbitrary elisp command to the function at point"))]
      ["Export..."
       (keymap "Export"
               (source menu-item "Export source code..." simple-call-tree-export-items
                       :help "Export source code of displayed items")
               (org menu-item "Export as org tree..." simple-call-tree-export-org-tree
                    :help "Export displayed items to an org-mode buffer"))]
      ["---" "---"]
      ["Jump To Branch At Point" simple-call-tree-jump-to-function
       :help "Goto the toplevel branch for the function at point"]
      ["Jump To Branch..." ,(lambda nil (interactive) (setq current-prefix-arg 1)
                              (call-interactively 'simple-call-tree-jump-to-function))
       :help "Prompt for a toplevel branch to jump to"
       :keys "J"]
      ["Add To Jump Ring" simple-call-tree-jump-ring-add
       :help "Add the function at point to the jump ring"]
      ["Previous Jump" simple-call-tree-jump-prev
       :help "Goto previous function in jump ring"]
      ["Next Jump" simple-call-tree-jump-next
       :help "Goto next function in jump ring"]
      ["---" "---"]
      ["Move to Top" simple-call-tree-move-top
       :help "Goto the top header of this subtree"]
      ["Next Branch" simple-call-tree-move-next
       :help "Goto the next branch"]
      ["Previous Branch" simple-call-tree-move-prev
       :help "Goto the previous branch"]
      ["Next Branch Same Level" simple-call-tree-move-next-samelevel
       :help "Goto the next branch at the same level as this one"
       :key "N"]
      ["Previous Branch Same Level" simple-call-tree-move-prev-samelevel
       :help "Goto the previous branch at the same level as this one"
       :key "P"]
      ["---" "---"]
      ["Cycle Tree Visibility" outline-cycle
       :help "Cycle through different tree visibility states"
       :visible (featurep 'outshine)
       :keys "<tab>"]
      ["Toggle Children Visibility" outline-toggle-children
       :help "Toggle the visibility of the children of this header"
       :visible (not (featurep 'outshine))
       :keys "<tab>"]
      ["Show All" show-all
       :help "Show All Branches"
       :key-sequence "a"]
      ["Hide Sublevels" hide-sublevels
       :help "Hide Lower Level Branches"
       :key-sequence "h"]
      ["Toggle duplicates" simple-call-tree-toggle-duplicates
       :help "Toggle display of duplicate sub-branches"
       :key-sequence "D"
       :style toggle
       :selected (not simple-call-tree-nodups)]
      ["Toggle Follow mode" fm-toggle
       :help "Toggle Follow Mode - auto display of function at point"
       :key-sequence "f"
       :visible (featurep 'fm)
       :style toggle
       :selected fm-working]
      ["Delete Other Windows" simple-call-tree-delete-other-windows
       :help "Make this window fill the whole frame"
       :key "1"]
      ["Invert Tree" simple-call-tree-invert-buffer
       :help "Invert the tree"
       :style toggle
       :selected simple-call-tree-inverted]
      ["Change Depth..." simple-call-tree-change-maxdepth
       :help "Change the depth of the tree"]
      ["Toggle Narrowing" simple-call-tree-toggle-narrowing
       :help "Toggle between narrow/wide buffer"
       :style toggle
       :selected (simple-call-tree-buffer-narrowed-p)]
      ["---" "---"]))
  (setq mode-line-format
        (append
         (subseq mode-line-format 0
                 (1+ (position 'mode-line-buffer-identification
                               mode-line-format)))
         (list '(:eval (format (concat "|Maxdepth=%d|"
                                       "Sorted "
                                       (case simple-call-tree-current-sort-order
                                         (position "by position|")
                                         (name "by name|")
                                         (numdescend "by number of descendants|")
                                         (size "by size|")
                                         (face "by face|")
                                         (todo "by TODO|")
                                         (priority "by priority|")
                                         (mark "by mark|")))
                               simple-call-tree-current-maxdepth)))
         (subseq mode-line-format
                 (+ 2 (position 'mode-line-buffer-identification
                                mode-line-format))))
        font-lock-defaults '((("^\\(\\*.*$\\)" 1 simple-call-tree-mark-face t)))
        org-not-done-keywords simple-call-tree-org-not-done-keywords))

;; simple-call-tree-info: DONE  
(defcustom simple-call-tree-mark-face (if (featurep 'dired+) diredp-flag-mark-line
                                        'highlight)
  "Face to use for marked items in the *Simple Call Tree* buffer."
  :group 'simple-call-tree
  :type 'face)

;; simple-call-tree-info: DONE  
(defvar simple-call-tree-alist nil
  "Alist of functions and the functions they call, and markers for their locations.
Each element is a list of lists. The first list in each element is in the form
 (FUNC START END) where FUNC is the function name and START & END are markers for the
positions of the start and end of the function. The other lists contain information
for functions called by FUNC, and are in the form (FUNC2 POS) where FUNC2 is the name
of the called function and POS is the position of the call.")

;; simple-call-tree-info: DONE
(defvar simple-call-tree-inverted-alist nil
  "Alist of functions and the functions that call them, and markers for their locations.
This is an inverted version of `simple-call-tree-alist'.")

;; simple-call-tree-info: DONE
(defvar simple-call-tree-inverted nil
  "Indicates if the *Simple Call Tree* buffer is currently inverted or not.
If non-nil then children correspond to callers of parents in the outline tree.
Otherwise it's the other way around.")

;; simple-call-tree-info: DONE
(defvar simple-call-tree-current-maxdepth nil
  "The current maximum depth of the tree in the *Simple Call Tree* buffer.
The minimum value is 0 which means show top level functions only.")

;; simple-call-tree-info: DONE
(defcustom simple-call-tree-jump-ring-max 20
  "Maximum number of elements in `simple-call-tree-jump-ring', before old elements are removed."
  :group 'simple-call-tree
  :type 'integer)

;; simple-call-tree-info: DONE
(defvar simple-call-tree-jump-ring (make-ring simple-call-tree-jump-ring-max)
  "Ring to hold history of functions jumped to in *Simple Call Tree* buffer.")

;; simple-call-tree-info: DONE  
(defvar simple-call-tree-jump-ring-index 0
  "The current position in the jump ring.")

;; simple-call-tree-info: DONE
(defvar simple-call-tree-current-sort-order simple-call-tree-default-sort-method
  "The current sort order of the call tree.
See `simple-call-tree-default-sort-method' for possible values.")

;; simple-call-tree-info: DONE
(defvar simple-call-tree-nodups nil
  "If non-nil then duplicate sub-branches will not be included in the tree.
I.e. if a function makes multiple calls to the same function then only one of these calls will
be shown in the tree.")

;; simple-call-tree-info: DONE
(defvar simple-call-tree-buffers nil
  "Buffers analyzed to create the simple-call-tree.")

(defvar simple-call-tree-buffer-name "*Simple Call Tree*"
  "Name for the simple call tree buffer.")

;; simple-call-tree-info: DONE
(defvar simple-call-tree-tags-regexp
  "simple-call-tree-info:\\s-*\\(\\w+\\)?\\s-*\\(\\[#[A-Z]\\]\\)?\\s-*\\(:[a-zA-Z0-9:,;-_]+:\\)?"
  "Regular expression to match org properties (todo, priority & tags) in source code.")

;; simple-call-tree-info: DONE
(defvar simple-call-tree-marked-items nil
  "List of names of items in the *Simple Call Tree* buffer that are or should be marked.")

(defvar simple-call-tree-regex-maxlen 10000
  "Maximum allowed length for regular expressions.")

;; simple-call-tree-info: DONE  
(cl-defun simple-call-tree-analyze (&optional (buffers (list (current-buffer))))
  "Analyze the current buffer, or the buffers in list BUFFERS.
The result is stored in `simple-call-tree-alist'.
Optional arg BUFFERS is a list of buffers to analyze together.
By default it is set to a list containing the current buffer."
  (interactive)
  (setq simple-call-tree-alist nil)
  ;; First add all the functions defined in the buffers to simple-call-tree-alist.
  (let (pos oldpos count1 pair nextfunc item endtest oldpos startmark endmark attribs)
    (dolist (buf buffers)
      (with-current-buffer buf
        (font-lock-default-fontify-buffer)
        (setq pos (point-min)
              count1 0
              endtest (sixth (assoc major-mode simple-call-tree-major-mode-alist)))
        (save-excursion
          (while (setq pair (simple-call-tree-next-func pos)
                       pos (car pair)
                       nextfunc (cdr pair))
            (goto-char pos)
            (setq attribs (simple-call-tree-get-attribs))
            (setq startmark (point-marker))
            (cond ((functionp endtest) (funcall endtest))
                  (endtest (end-of-defun))
                  ((setq pair (simple-call-tree-next-func pos))
                   (goto-char (- (car pair) (length (cdr pair)))))
                  (t (goto-char (point-max))))
            (setq endmark (point-marker))
            (add-to-list 'simple-call-tree-alist (list (list nextfunc startmark endmark
                                                             (car attribs)
                                                             (second attribs)
                                                             (third attribs))))
            (setq count1 (1+ count1))))))
    ;; Now find functions called
    ;; This is still not exactly right: it will match both abc & abc' on abc'
    ;; where ' could be any char in the expression prefix syntax class.
    ;; This happens in haskell mode for example when you have defined two functions
    ;; named func and func' for example.
    (let* ((mode (with-current-buffer
                     (marker-buffer (second (caar simple-call-tree-alist)))
                   major-mode))
           (modevals (assoc major-mode simple-call-tree-major-mode-alist))
           (symstart (or (seventh modevals) "\\_<"))
           (symend (or (eighth modevals) "\\_>"))
           (names (mapcar 'caar simple-call-tree-alist))
           ;; May need to make several regexps if there are many names
           ;; (there is a limit on the size of regexp allowed by `re-search-forward')
           (lengths (mapcar 'length names))
           (regexps (mapcar (lambda (lst) (concat symstart
                                                  (regexp-opt lst t)
                                                  symend))
                            (cl-loop for i from 0 to (1- (length lengths))
				     with sum = 0
				     with start = 0
				     if (and (< sum simple-call-tree-regex-maxlen)
					     (< i (1- (length lengths))))
				     do (setq sum (+ sum (nth i lengths)))
				     else
				     collect (subseq names start i)
				     and do (setq start (1+ i) sum 0))))
           (invalidfonts (or (third (assoc mode simple-call-tree-major-mode-alist))
                             simple-call-tree-default-invalid-fonts)))
      (cl-loop for item in simple-call-tree-alist
	       for count2 from 1
	       for buf = (marker-buffer (second (car item)))
	       for startpos = (marker-position (second (car item)))
	       for endpos = (marker-position (third (car item)))
	       do (with-current-buffer buf
		    (save-excursion
		      (goto-char startpos)
		      (while (dolist (regex regexps)
			       (if (re-search-forward regex endpos t)
				   (return t)))
			;; need to go back so that the text properties are read correctly
			(forward-word -1)
			;; check face is valid
			(let ((face (get-text-property (point) 'face)))
			  (if (listp face)
			      (if (not (intersection face invalidfonts))
				  (push (list (match-string 1) (point-marker)) (cdr item)))
			    (if (not (member face invalidfonts))
				(push (list (match-string 1) (point-marker)) (cdr item)))))
			(forward-word 1))))))
    (setq simple-call-tree-inverted-alist (simple-call-tree-invert))
    (message "simple-call-tree done")))

;; Unable to get this to go significantly faster 
;; simple-call-tree-info: DONE  
(defun simple-call-tree-invert nil
  "Invert `simple-call-tree-alist' and return the result."
  (let ((result (mapcar (lambda (item) (list (car item)))
                        simple-call-tree-alist)))
    (dolist (item simple-call-tree-alist)
      (let* ((caller (car item)))
        (dolist (callee (cdr item))
          (let ((elem (simple-call-tree-get-item (car callee) result)))
            (if elem (push (list (caar item) (second callee))
                           (cdr elem)))))))
    result))

;;; New functions (not in simple-call-tree.el)

;; simple-call-tree-info: DONE
(cl-defun simple-call-tree-get-function-at-point (&optional (buf simple-call-tree-buffer-name))
  "Return the name of the function nearest point in the *Simple Call Tree* buffer.
If optional arg BUF is supplied then use BUF instead of the *Simple Call Tree* buffer.
If there is no function on this line of the *Simple Call Tree* buffer, return nil."
  (with-current-buffer buf
    (if (equal buf simple-call-tree-buffer-name)
        (save-excursion
          (move-beginning-of-line nil)
          (if (re-search-forward (concat outline-regexp "\\(\\S-+\\)")
                                 (line-end-position) t)
              (match-string 6)
            (previous-line)
            (re-search-forward (concat outline-regexp "\\(\\S-+\\)")
                               (line-end-position) t)
            (match-string 6)))
      (symbol-name (if (functionp 'symbol-nearest-point)
                       (symbol-nearest-point)
                     (symbol-at-point))))))

;; simple-call-tree-info: DONE  
(defun simple-call-tree-next-func (start)
  "Find the next function in the current buffer after position START.
Return a cons cell whose car is the position in the buffer just after the function name,
and whose cdr is the function name, unless there are no more functions in which case return
nil."
  (let* ((modevals (assoc major-mode simple-call-tree-major-mode-alist))
         (validfonts (or (second modevals)
                         simple-call-tree-default-valid-fonts))
         (invalidfonts (or (third modevals)
                           simple-call-tree-default-invalid-fonts))
         (starttest (fourth modevals))
         (nextfunc (fifth modevals))
         end)
    (if nextfunc
        (save-excursion
          (goto-char start)
          (setq start (funcall nextfunc)))
      (while (and (or (not (memq (get-text-property start 'face) validfonts))
                      (memq (get-text-property start 'face) invalidfonts)
                      (and starttest
                           (not (save-excursion (funcall starttest start)))))
                  (callf next-single-property-change start 'face))))
    (unless (not start)
      (setq end (next-single-property-change start 'face))
      (unless (not end)
        (cons end (buffer-substring start end))))))

;; simple-call-tree-info: DONE  
(cl-defun simple-call-tree-get-attribs (&optional (lookback -2))
  "Extract org attributes from item in source code from lines previous to the current one.
The LOOKBACK argument indicates how many lines backwards to search and should be negative."
  (let ((end (point)) todo priority tags)
    (forward-line lookback)
    (if (re-search-forward
         "simple-call-tree-info:\\s-*\\(\\w+\\)?\\(\\s-*\\[#\\([A-Z]\\)\\]\\)?\\(\\s-*\\(:[a-zA-Z0-9:,;-_]+:\\)\\)?\\s-*$"
         end t)
        (progn
          (aif (match-string 1) (setq todo (substring-no-properties it)))
          (aif (match-string 3) (setq priority (string-to-char it)))
          (setq tags (simple-call-tree-string-to-tags (match-string 5)))))
    (goto-char end)
    (list todo priority tags)))

;; Need to be able to handle cases where the function starts at the beginning of the file,
;; (so there are no previous lines).
;; simple-call-tree-info: DONE  
(cl-defun simple-call-tree-set-attribute (attr value &optional func (updatesrc t))
  "Set the todo, priority, or tags for an item in `simple-call-tree-alist', and update the buffer and source code.
ATTR can be one of: 'todo, 'priority, or 'tags
VALUE is the corresponding value: a string for 'todo or 'priority (a single letter), or a list of strings for 'tags.
FUNC is the name of the function corresponding to the item to be updated.
The *Simple Call Tree* buffer and comments in the source code (just before FUNC) will be updated with the corresponding
information. If UPDATESRC is nil then don't bother updating the source code."
  (let* ((item (car (simple-call-tree-get-item func)))
         (marker (second item))
         (buf (marker-buffer marker))
         (end (marker-position marker))
         srcval)
    (case attr
      (todo (unless (or (not value) (stringp value))
              (error "Invalid TODO value"))
            (setf newval (or value "")
                  srcval (concat newval " \\2 \\3")
                  (fourth item) value))
      (priority (unless (or (not value)
                            (and (integerp value)
                                 (>= value simple-call-tree-org-highest-priority)
                                 (<= value simple-call-tree-org-lowest-priority)))
                  (error "Invalid priority value"))
                (setf newval (if value (concat "[#" (char-to-string value) "]") "")
                      srcval (concat "\\1 " (if value newval nil) " \\3")
                      (fifth item) value))
      (tags (unless (listp value) (error "Invalid tags value"))
            (setf newval (simple-call-tree-tags-to-string value)
                  srcval (concat "\\1 \\2 " newval)
                  (sixth item) value)))
    (if updatesrc
        (with-current-buffer buf
          (save-excursion
            (goto-char end)
            (forward-line -2)
            (if (re-search-forward
                 "simple-call-tree-info:\\s-*\\(\\w+\\)?\\s-*\\(\\[#[A-Z]\\]\\)?\\s-*\\(:[a-zA-Z0-9:,;-_]+:\\)?"
                 end t)
                (replace-match (concat "simple-call-tree-info: " srcval))
              (goto-char end)
              (beginning-of-line)
              (insert "simple-call-tree-info: " (or newval "") "\n")
              (forward-line -1)
              (comment-region (line-beginning-position) (line-end-position))))))
    (save-excursion
      (goto-char (point-min))
      (if (simple-call-tree-goto-func func)
          (let ((hidden (not (= (save-excursion (outline-end-of-heading) (point))
                                (save-excursion (outline-end-of-subtree) (point)))))
                (marked (simple-call-tree-marked-p func)))
            (if hidden (show-children)) ;hack! otherwise it doesn't always work properly
            (read-only-mode -1)
            (beginning-of-line)
            (kill-line)
            (simple-call-tree-insert-item item 1 nil marked)
            (read-only-mode 1)
            (if hidden (hide-subtree)))))))

;; simple-call-tree-info: DONE  
(cl-defun simple-call-tree-set-todo (value funcs &optional remove)
  "Set the TODO state for the function(s) FUNCS.
By default FUNCS is set to the list of marked items or the function at point if there are no marked items.
If a prefix arg is used (or REMOVE is non-nil) then remove the TODO state."
  (interactive (list (if current-prefix-arg nil
                       (org-icompleting-read
                        "State: " (simple-call-tree-org-todo-keywords)
                        nil t))
                     (or simple-call-tree-marked-items
                         (simple-call-tree-get-parent)
                         (simple-call-tree-get-function-at-point))))
  (dolist (func funcs)
    (simple-call-tree-set-attribute 'todo value func)))

;; simple-call-tree-info: DONE
(defun simple-call-tree-next-todo nil
  "Move to next todo state for current function."
  (interactive)
  (let* ((func (or (simple-call-tree-get-parent)
                   (simple-call-tree-get-function-at-point)))
         (curtodo (fourth (car (simple-call-tree-get-item func))))
         (states (simple-call-tree-org-todo-keywords))
         (len (length states))
         (pos (position curtodo states :test 'equal)))
    (simple-call-tree-set-attribute
     'todo
     (if pos (if (< pos (1- len)) (nth (1+ pos) states))
       (car states))     
     func t)))

;; simple-call-tree-info: DONE
(defun simple-call-tree-prev-todo nil
  "Move to previous todo state for current function."
  (interactive)
  (let* ((func (or (simple-call-tree-get-parent)
                   (simple-call-tree-get-function-at-point)))
         (curtodo (fourth (car (simple-call-tree-get-item func))))
         (states (simple-call-tree-org-todo-keywords))
         (len (length states))
         (pos (position curtodo states)))
    (simple-call-tree-set-attribute
     'todo
     (if pos (if (> pos 0) (nth (1- pos) states))
       (nth (1- len) states))
     func t)))

;; simple-call-tree-info: DONE  
(cl-defun simple-call-tree-set-priority (value funcs)
  "Set the priority level to VALUE for the function(s) FUNCS.
By default FUNCS is set to the list of marked items or the function at point if there are no marked items."
  (interactive (progn (message "Priority %c-%c, SPC to remove: "
                               simple-call-tree-org-highest-priority simple-call-tree-org-lowest-priority)
                      (list (read-char-exclusive)
                            (or simple-call-tree-marked-items
                                (list (or (simple-call-tree-get-parent)
                                          (simple-call-tree-get-function-at-point)))))))
  (cond ((not value) t)
        ((equal value ?\ ) (setq value nil))
        ((or (< (upcase value) simple-call-tree-org-highest-priority)
             (> (upcase value) simple-call-tree-org-lowest-priority))
         (user-error "Priority must be between `%c' and `%c'"
                     simple-call-tree-org-highest-priority
                     simple-call-tree-org-lowest-priority)))
  (dolist (func funcs)
    (simple-call-tree-set-attribute 'priority (and value (upcase value)) func t)))

;; simple-call-tree-info: DONE
(defun simple-call-tree-up-priority nil
  "Change current function to the next priority level."
  (interactive)
  (let* ((func (or (simple-call-tree-get-parent)
                   (simple-call-tree-get-function-at-point)))
         (curpriority (fifth (car (simple-call-tree-get-item func))))
         (nextpriority (cond ((not curpriority) simple-call-tree-org-lowest-priority)
                             ((and (<= curpriority simple-call-tree-org-lowest-priority)
                                   (> curpriority simple-call-tree-org-highest-priority))
                              (1- curpriority))
                             ((= curpriority simple-call-tree-org-highest-priority) nil))))
    (simple-call-tree-set-attribute 'priority nextpriority func t)))

;; simple-call-tree-info: DONE
(defun simple-call-tree-down-priority nil
  "Change current function to the previous priority level."
  (interactive)
  (let* ((func (or (simple-call-tree-get-parent)
                   (simple-call-tree-get-function-at-point)))
         (curpriority (fifth (car (simple-call-tree-get-item func))))
         (nextpriority (cond ((not curpriority) simple-call-tree-org-highest-priority)
                             ((and (< curpriority simple-call-tree-org-lowest-priority)
                                   (>= curpriority simple-call-tree-org-highest-priority))
                              (1+ curpriority))
                             ((= curpriority simple-call-tree-org-lowest-priority) nil))))
    (simple-call-tree-set-attribute 'priority nextpriority func t)))

;; simple-call-tree-info: DONE  
(cl-defun simple-call-tree-set-tags (value funcs)
  "Set the org tags for the function(s) FUNCS.
By default FUNCS is set to the list of marked items or the function at point if there are no marked items."
  (interactive (let* ((funcs (or simple-call-tree-marked-items
                                 (list (or (simple-call-tree-get-parent)
                                           (simple-call-tree-get-function-at-point)))))
                      currenttags)
                 (dolist (func funcs)
                   (callf2 append (sixth (car (simple-call-tree-get-item func))) currenttags))
                 (callf cl-remove-duplicates currenttags :test 'equal)
                 (list (simple-call-tree-string-to-tags
                        (org-fast-tag-selection currenttags nil simple-call-tree-org-tag-alist))
                       funcs)))
  (dolist (func funcs)
    (simple-call-tree-set-attribute 'tags value func t)))

;; simple-call-tree-info: DONE  
(defun simple-call-tree-add-tags (value funcs &optional remove)
  "Add tags in VALUE to the function(s) FUNCS.
By default FUNCS is set to the list of marked items or the function at point if there are no marked items.
If REMOVE is non-nil remove the tags instead."
  (interactive (let* ((funcs (or simple-call-tree-marked-items
                                 (list (or (simple-call-tree-get-parent)
                                           (simple-call-tree-get-function-at-point)))))
                      currenttags)
                 (dolist (func funcs)
                   (callf2 append (sixth (car (simple-call-tree-get-item func))) currenttags))
                 (callf cl-remove-duplicates currenttags :test 'equal)
                 (list (simple-call-tree-string-to-tags
                        (org-fast-tag-selection currenttags nil simple-call-tree-org-tag-alist))
                       funcs
                       current-prefix-arg)))
  (dolist (func funcs)
    (let* ((currenttags (sixth (car (simple-call-tree-get-item func))))
           (newtags (if remove (cl-set-difference currenttags value :test 'equal)
                      (cl-union value currenttags))))
      (simple-call-tree-set-attribute 'tags newtags func))))

;;;###autoload
;; simple-call-tree-info: DONE  
(cl-defun simple-call-tree-display-buffer (&optional files)
  "Display call tree for current buffer.
If optional arg FILES is supplied it specifies a list of files to search for functions to display in the tree.
When called interactively files will be prompted for and only functions in the current buffer will be used."
  (interactive "P")
  (let (buffers dir regexp)
    (or files
        (if (y-or-n-p "Include other files?")
            (whilelast
             (setq dir
                   (if (and (featurep 'ido)
                            (or (eq ido-mode 'file)
                                (eq ido-mode 'both)))
                       (ido-read-directory-name "Dir containing files: ")
                     (read-directory-name "Dir containing files: ")))
             (list-directory dir)
             (setq regexp (read-regexp "Regexp matching filenames (RET to finish)"))
             (unless (string= regexp "")
               (mapc (lambda (name) (if (string-match regexp name)
                                        (add-to-list 'files (concat dir name))))
                     (directory-files dir))))))
    (save-excursion
      (setq buffers (cl-loop for file in files collect (find-file file))))
    (if (or (not files) (called-interactively-p))
        (add-to-list 'buffers (current-buffer)))
    ;; If we already have a call tree for those buffers, just redisplay it
    (if (and (get-buffer simple-call-tree-buffer-name)
	     (not (cl-set-exclusive-or
		   buffers simple-call-tree-buffers)))
	(switch-to-buffer simple-call-tree-buffer-name)
      (simple-call-tree-build-tree buffers)
      (setq simple-call-tree-jump-ring (make-ring simple-call-tree-jump-ring-max)
	    simple-call-tree-jump-ring-index 0))))

;;;###autoload
;; simple-call-tree-info: DONE  
(defun simple-call-tree-build-tree (&optional buffers)
  "Build the simple-call-tree and display it in the \"*Simple Call Tree*\" buffer.
If BUFFERS is supplied it should be a list of buffer to analyze, otherwise the buffers
listed in `simple-call-tree-buffers' will be used."
  (interactive)
  (callf or buffers simple-call-tree-buffers)
  (simple-call-tree-analyze buffers)
  (setq simple-call-tree-inverted nil
        simple-call-tree-marked-items nil
        simple-call-tree-buffers buffers)
  (case simple-call-tree-default-sort-method
    (position (simple-call-tree-reverse))
    (name (simple-call-tree-sort-by-name))
    (numdescend (simple-call-tree-sort-by-num-descendants))
    (face (simple-call-tree-sort-by-face))
    (size (simple-call-tree-sort-by-size))
    (priority (simple-call-tree-sort-by-priority))
    (todo (simple-call-tree-sort-by-todo)))
  (goto-char (point-min)))

;;;###autoload
;; simple-call-tree-info: DONE  
(cl-defun simple-call-tree-current-function (func &optional wide)
  "Display call tree for function FUNC.
If called interactively FUNC will be set to the symbol nearest point,
unless a prefix arg is used in which case the function returned by `which-function'
will be used.
Note: `which-function' may give incorrect results if `imenu' has not been used in
the current buffer.
If a call tree containing FUNC has not already been created then the user is prompted
for which files to build the tree from.

If optional arg WIDE is non-nil then the *Simple Call Tree* buffer will be widened,
otherwise it will be narrowed around FUNC."
  (interactive (list (if current-prefix-arg
                         (which-function)
                       (simple-call-tree-get-function-at-point (current-buffer)))))
  (if (simple-call-tree-get-item func)
      (if (get-buffer simple-call-tree-buffer-name)
          (switch-to-buffer simple-call-tree-buffer-name)
        (simple-call-tree-list-callers-and-functions))
    (simple-call-tree-display-buffer))
  (simple-call-tree-jump-to-function func)
  (if wide (simple-call-tree-toggle-narrowing 1)
    (simple-call-tree-toggle-narrowing -1)))

;; simple-call-tree-info: DONE  
(cl-defun simple-call-tree-list-callers-and-functions (&optional (maxdepth simple-call-tree-default-maxdepth)
                                                               (funclist simple-call-tree-alist))
  "List callers and functions in FUNCLIST to depth MAXDEPTH.
By default FUNCLIST is set to `simple-call-tree-alist'."
  (switch-to-buffer (get-buffer-create simple-call-tree-buffer-name))
  (if (not (equal major-mode 'simple-call-tree-mode))
      (simple-call-tree-mode))
  (read-only-mode -1)
  (erase-buffer)
  (let ((maxdepth (max maxdepth 1)))
    (dolist (item funclist)
      (simple-call-tree-list-callees-recursively
       (car item)
       maxdepth 1 funclist))
    (setq simple-call-tree-current-maxdepth (max maxdepth 1))
    ;; remove the empty line at the end
    (delete-char -1)
    (read-only-mode 1)))

;; simple-call-tree-info: DONE  
(defun simple-call-tree-listed-items nil
  "Return list of items which are currently visible in the *Simple Call Tree* buffer.
Items returned are elements of `simple-call-tree-alist'"
  (let ((items))
    (with-current-buffer simple-call-tree-buffer-name
      (goto-char (point-min))
      (re-search-forward "^\\([|*]\\)\\( +\\w+\\)?\\( \\[#.\\]\\)? \\(\\S-+\\)")
      (add-to-list 'items
                   (simple-call-tree-get-item
                    (simple-call-tree-get-function-at-point)))
      (while (condition-case err
                 (simple-call-tree-move-next-samelevel)
               (error nil))
        (add-to-list 'items
                     (simple-call-tree-get-item
                      (simple-call-tree-get-function-at-point)))))
    (reverse items)))

;; simple-call-tree-info: DONE  
(defun simple-call-tree-export-org-tree (buf &optional display)
  "Create an org-tree from the currently visible items, and put it in an org buffer.
The style of links used for child headers is controlled by `simple-call-tree-org-link-style'."
  (interactive (list (ido-read-buffer "Append to buffer: " "simple-call-tree-export.org")
                     current-prefix-arg))
  (let ((exportbuf (get-buffer-create buf)))
    (with-current-buffer exportbuf
      (org-mode)
      (goto-char (point-max))
      (dolist (item (simple-call-tree-listed-items))
        (simple-call-tree-list-callees-recursively
         (car item)
         simple-call-tree-current-maxdepth
         1
         simple-call-tree-alist
         simple-call-tree-inverted
         'simple-call-tree-insert-org-header))
      (org-update-radio-target-regexp))
    (if display (switch-to-buffer exportbuf))))

;; simple-call-tree-info: DONE  
(defun simple-call-tree-export-items (buf &optional display)
  "Export the currently visible items into a buffer."
  (interactive (list (ido-read-buffer "Append to buffer: " "*Simple Call Tree Export*")
                     current-prefix-arg))
  (let ((exportbuf (get-buffer-create buf)))
    (dolist (item (simple-call-tree-listed-items))
      (let* ((startmark (second (car item)))
             (endmark (third (car item)))
             (buf (marker-buffer startmark))
             (startpos (with-current-buffer buf
                         (save-excursion
                           (goto-char (marker-position startmark))
                           ;; make sure we capture comment lines
                           (while (and (= (forward-line -1) 0)
                                       (comment-only-p (line-beginning-position)
                                                       (line-end-position))))
                           (unless (= (line-beginning-position) (point-min))
                             (forward-line 1))
                           (point))))
             (endpos (marker-position endmark))
             (text (with-current-buffer buf (buffer-substring startpos endpos))))
        (with-current-buffer exportbuf
          (goto-char (point-max))
          (insert text))))
    (if display (switch-to-buffer exportbuf))))

;; simple-call-tree-info: DONE  
(cl-defun simple-call-tree-list-callees-recursively (item &optional (maxdepth 2)
                                                        (curdepth 1)
                                                        (funclist simple-call-tree-alist)
                                                        (inverted simple-call-tree-inverted)
                                                        (displayfunc 'simple-call-tree-insert-item)
                                                        (marked simple-call-tree-marked-items))
  "Insert a call tree for the item, to depth MAXDEPTH.
item must be the car of one of the elements of FUNCLIST which is set to `simple-call-tree-alist' by default.
The optional arguments MAXDEPTH and CURDEPTH specify the maximum and current depth of the tree respectively.
If INVERTED is non-nil then the tree will be displayed inverted (default `simple-call-tree-inverted').
DISPLAYFUNC is used to display each item (default `simple-call-tree-insert-item', which see).
MARKED is a list of marked items (default `simple-call-tree-marked-items', which see).
This is a recursive function, and you should not need to set CURDEPTH."
  (let* ((fname (first item))
         (callees (cdr (simple-call-tree-get-item fname funclist)))
         done)
    (funcall displayfunc item curdepth inverted (simple-call-tree-marked-p fname marked))
    (insert "\n")
    (if (< curdepth maxdepth)
        (dolist (callee callees)
          (unless (and simple-call-tree-nodups (member (car callee) done))
            (simple-call-tree-list-callees-recursively callee maxdepth (1+ curdepth) funclist inverted displayfunc))
          (add-to-list 'done (car callee))))))

;; Propertize todo, priority & tags appropriately
;; Add invisibility property to text so that `simple-call-tree-hide-marked' works
;; simple-call-tree-info: DONE  
(defun simple-call-tree-insert-item (item curdepth &optional inverted marked)
  "Display ITEM at depth CURDEPTH in the call tree.
If optional arg INVERTED is non-nil reverse the arrow for the item.
If optional arg MARKED is non-nil use a * instead of a |."
  (let* ((fname (first item))
         (pos (second item))
         (todo (fourth item))
         (priority (fifth item))
         (tags (simple-call-tree-tags-to-string (sixth item)))
         (pre (concat (if todo (concat " " (propertize todo 'font-lock-face
                                                       (org-get-todo-face todo))))
                      (if priority (propertize (concat " [#" (char-to-string priority) "]")
                                               'font-lock-face
                                               (or (org-face-from-face-or-color
                                                    'priority 'org-priority
                                                    (cdr (assoc priority org-priority-faces)))
                                                   'org-priority)))))
         (arrowtail (make-string (* 2 (1- curdepth)) 45))
         (arrow (if inverted (concat (if (> curdepth 1) "<" pre) arrowtail " ")
                  (concat arrowtail (if (> curdepth 1) ">" pre) " ")))
         (fnface (get-text-property 0 'face fname))
         (pre2 (concat (if marked "*" "|") arrow
                       (propertize fname
                                   'font-lock-face (list :inherit (or fnface 'default) :underline t)
                                   'mouse-face 'highlight
                                   'location pos)))
         (post (concat (make-string (max 0 (- (/ (window-width) 2) (length pre2))) 32) tags)))
    (insert pre2 post)))

;; simple-call-tree-info: DONE  
(defun simple-call-tree-insert-org-header (item curdepth &optional inverted marked)
  "Display ITEM at depth CURDEPTH in the call tree.
Ignore optional args INVERTED and MARKED; they are just for compatibility with `simple-call-tree-list-callees-recursively'."
  (let* ((fname (first item))
         (marker (second item))
         (todo (aif (fourth item)
                   (concat " " (propertize it 'font-lock-face (org-get-todo-face it)))))
         (priority (aif (fifth item)
                       (propertize (concat " [#" (char-to-string it) "]")
                                   'font-lock-face
                                   (or (org-face-from-face-or-color
                                        'priority 'org-priority
                                        (cdr (assoc it org-priority-faces)))
                                       'org-priority))))
         (tags (simple-call-tree-tags-to-string (sixth item)))
         (stars (make-string curdepth 42)))
    (if (and (> curdepth 1)
             (eq simple-call-tree-org-link-style 'radio))
        (insert stars " " fname)
      (with-current-buffer (marker-buffer marker)
        (save-excursion
          (goto-char (marker-position marker))
          (call-interactively 'org-store-link)))
      (let* ((link (concat
                    " [[" (substring-no-properties (caar org-stored-links)) "][" fname "]]"))
             (pre (concat stars todo priority)))
        (insert (concat pre link
                        (if (sixth item)
                            (make-string (max (- (floor (/ (window-width) 1.7))
                                                 (length pre)
                                                 (length fname)
                                                 (length tags))
                                              5) 32))
                        tags)))
      (if (eq simple-call-tree-org-link-style 'radio)
          (insert "\n<<<" fname ">>>")))
    (callf cdr org-stored-links)))

;; simple-call-tree-info: DONE
(defun simple-call-tree-outline-level nil
  "Return the outline level of the function at point.

Note: this may not give correct results if the current headline was reached by moving
the cursor manually since it depends on the match string from a previous outline movement
command. To ensure correct results do a search for `outline-regexp' from the beginning of
the line first in order to capture the match-string.
We can't do that in this function as it causes other problems with outline mode commands."
  (with-current-buffer simple-call-tree-buffer-name
    (save-excursion
      (let ((arrowlen (length (match-string 1))))
        (if (= arrowlen 0) 1 (1+ (/ (1- arrowlen) 2)))))))

;; simple-call-tree-info: DONE  
(defun simple-call-tree-get-parent nil
  "Return the name of the parent of the function at point according to the current tree.
If there is no parent, return nil."
  (with-current-buffer simple-call-tree-buffer-name
    (save-excursion
      (if (condition-case nil
              (simple-call-tree-move-top)
            (error nil))
          (simple-call-tree-get-function-at-point)))))

;; simple-call-tree-info: DONE
(defun simple-call-tree-get-toplevel nil
  "Return the name of the toplevel parent of the subtree at point."
  (with-current-buffer simple-call-tree-buffer-name
    (save-excursion
      (move-beginning-of-line nil)
      (re-search-forward outline-regexp)
      (let ((level (simple-call-tree-outline-level)))
        (if (condition-case nil
                (outline-up-heading (1- level))
              (error nil))
            (simple-call-tree-get-function-at-point))))))

;; simple-call-tree-info: DONE
(defun simple-call-tree-narrow-to-function (func &optional pos)
  "Narrow the source buffer containing FUNC to that function.
If optional arg POS is supplied then move point to POS after
narrowing."
  (let* ((trio (car (simple-call-tree-get-item func)))
         (buf (marker-buffer (second trio)))
         (start (marker-position (second trio)))
         (end (marker-position (third trio))))
    (with-current-buffer buf
      (goto-char start)
      (beginning-of-line)
      (narrow-to-region (point) end)
      (if pos (goto-char pos)))))

;; Only sort visible items
;; simple-call-tree-info: DONE  
(defun simple-call-tree-sort (predicate)
  "Sort the branches and sub-branches of `simple-call-tree-alist' and `simple-call-tree-inverted-alist' by PREDICATE.
PREDICATE should be a function taking two arguments and returning non-nil if the first one should sort before the second.
When sorting the toplevel items the cars of the corresponding items in `simple-call-tree-alist' are passed as args,
and when sorting the branches of those items the items in the cdr are passed."
  (let ((invertedtree nil))
    (dolist (branch simple-call-tree-alist)
      (setcdr branch (sort (cdr branch) predicate)))
    (callf sort simple-call-tree-alist
      (lambda (a b)
        (funcall predicate (car a) (car b))))
    (setq invertedtree t)
    (dolist (branch simple-call-tree-inverted-alist)
      (setcdr branch (sort (cdr branch) predicate)))
    (callf sort simple-call-tree-inverted-alist
      (lambda (a b)
        (funcall predicate (car a) (car b))))))

;; simple-call-tree-info: DONE  
(defun simple-call-tree-reverse nil
  "Reverse the order of the branches & sub-branches in `simple-call-tree-alist' and `simple-call-tree-inverted-alist'."
  (interactive)
  (dolist (branch simple-call-tree-alist)
    (setcdr branch (reverse (cdr branch))))
  (callf reverse simple-call-tree-alist)
  (dolist (branch simple-call-tree-inverted-alist)
    (setcdr branch (reverse (cdr branch))))
  (callf reverse simple-call-tree-inverted-alist)
  (simple-call-tree-revert))

;; simple-call-tree-info: DONE
(defun simple-call-tree-count-descendants (func depth alist)
  "Count the number of descendents of item named FUNC to depth DEPTH."
  (let ((item (simple-call-tree-get-item func alist)))
    (+ (1- (length item))
       (if (> depth 1)
           (cl-loop for child in (cdr item)
		    sum (simple-call-tree-count-descendants (car child) 
							    (1- depth)
							    alist))
         0))))

;; simple-call-tree-info: DONE  
(defun simple-call-tree-sort-by-num-descendants (depth)
  "Sort the branches in the *Simple Call Tree* buffer by the number of descendants to depth DEPTH.
When call interactively DEPTH is prompted for."
  (interactive (list (read-number "Depth: " 1)))
  (let ((normallist (copy-tree simple-call-tree-alist))
        (invlist (copy-tree simple-call-tree-inverted-alist)))
    (simple-call-tree-sort
     (lambda (a b)
       (let ((alist (if invertedtree invlist normallist)))
         (> (simple-call-tree-count-descendants (car a) depth alist)
            (simple-call-tree-count-descendants (car b) depth alist))))))
  (simple-call-tree-revert)
  (setq simple-call-tree-current-sort-order 'numdescend))

;; simple-call-tree-info: DONE
(defun simple-call-tree-sort-by-name nil
  "Sort the functions in the *Simple Call Tree* buffer alphabetically.
The toplevel functions will be sorted, and the functions in each branch will be sorted separately."
  (interactive)
  (simple-call-tree-sort (lambda (a b) (string< (car a) (car b))))
  (simple-call-tree-revert)
  (setq simple-call-tree-current-sort-order 'name))

;; simple-call-tree-info: DONE
(defun simple-call-tree-sort-by-position nil
  "Sort the functions in the *Simple Call Tree* buffer by position.
The toplevel functions will be sorted, and the functions in each branch will be sorted separately."
  (interactive)
  (simple-call-tree-sort (lambda (a b)
                           (or (string< (buffer-name (marker-buffer (second a)))
                                        (buffer-name (marker-buffer (second b))))
                               (< (marker-position (second a))
                                  (marker-position (second b))))))
  (simple-call-tree-revert)
  (setq simple-call-tree-current-sort-order 'position))

;; simple-call-tree-info: DONE  
(defun simple-call-tree-sort-by-face nil
  "Sort the items in the *Simple Call Tree* buffer according to the display face name.
This should sort the items by type (e.g. function, variable, etc.) since different types are generally displayed
with different faces.
The toplevel functions will be sorted, and the functions in each branch will be sorted separately."
  (interactive)
  (simple-call-tree-sort (lambda (a b) (string< (symbol-name (get-text-property 0 'face (car a)))
                                                (symbol-name (get-text-property 0 'face (car b))))))
  (simple-call-tree-revert)
  (setq simple-call-tree-current-sort-order 'face))

;; simple-call-tree-info: DONE
(defun simple-call-tree-sort-by-todo nil
  "Sort the items in the *Simple Call Tree* buffer by TODO state."
  (interactive)
  (simple-call-tree-sort
   (lambda (a b)
     (let ((todoa (fourth a))
           (todob (fourth b)))
       (if todoa
           (if todob
               (let ((all (simple-call-tree-org-todo-keywords)))
                 (< (position todoa all :test 'equal)
                    (position todob all :test 'equal)))
             t)))))
  (simple-call-tree-revert)
  (setq simple-call-tree-current-sort-order 'todo))

;; simple-call-tree-info: DONE
(defun simple-call-tree-sort-by-priority nil
  "Sort the items in the *Simple Call Tree* buffer by priority level."
  (interactive)
  (simple-call-tree-sort
   (lambda (a b)
     (let ((prioa (fifth a))
           (priob (fifth b)))
       (if prioa
           (if priob
               (< prioa priob)
             t)))))
  (simple-call-tree-revert)
  (setq simple-call-tree-current-sort-order 'priority))

;; simple-call-tree-info: DONE
(defun simple-call-tree-sort-by-size nil
  "Sort the items in the *Simple Call Tree* buffer by size."
  (interactive)
  (let ((alist (copy-tree simple-call-tree-alist)))
    (simple-call-tree-sort
     (lambda (a b)
       (let* ((itema (car (simple-call-tree-get-item (car a) alist)))
              (itemb (car (simple-call-tree-get-item (car b) alist))))
         (> (- (marker-position (third itema))
               (marker-position (second itema)))
            (- (marker-position (third itemb))
               (marker-position (second itemb))))))))
  (simple-call-tree-revert)
  (setq simple-call-tree-current-sort-order 'size))

;; simple-call-tree-info: DONE  
(defun simple-call-tree-sort-by-mark nil
  "Sort the marked items in the *Simple Call Tree* buffer before the unmarked ones."
  (interactive)
  (simple-call-tree-sort
   (lambda (a b)
     (or (simple-call-tree-marked-p (car a))
         (not (simple-call-tree-marked-p (car b))))))
  (simple-call-tree-revert)
  (setq simple-call-tree-current-sort-order 'mark))

;; simple-call-tree-info: DONE  
(defun simple-call-tree-move-item-up (&optional arg)
  (interactive (list current-prefix-arg))
  (with-current-buffer simple-call-tree-buffer-name
    (simple-call-tree-move-top)
    (let* ((arg (or (and arg (max arg 1)) 1))
           (item (simple-call-tree-get-item (simple-call-tree-get-function-at-point)))
           (pos (position item simple-call-tree-alist :test 'equal))
           (newpos (- pos arg)))
      (if (>= newpos 0)
          (progn
            (setq simple-call-tree-alist
                  (append (subseq simple-call-tree-alist 0 newpos)
                          (list item)                                 
                          (subseq simple-call-tree-alist newpos pos)
                          (subseq simple-call-tree-alist (1+ pos))))
            (read-only-mode -1)
            (outline-move-subtree-up arg)
            (read-only-mode 1))))))

;; simple-call-tree-info: DONE  
(defun simple-call-tree-move-item-down (&optional arg)
  (interactive (list current-prefix-arg))
  (with-current-buffer simple-call-tree-buffer-name
    (simple-call-tree-move-top)
    (let* ((arg (or (and arg (max arg 1)) 1))
           (item (simple-call-tree-get-item (simple-call-tree-get-function-at-point)))
           (pos (position item simple-call-tree-alist :test 'equal))
           (newpos (+ pos arg 1)))
      (if (< newpos (length simple-call-tree-alist))
          (progn (setq simple-call-tree-alist
                       (append (subseq simple-call-tree-alist 0 pos)
                               (subseq simple-call-tree-alist (1+ pos) newpos)
                               (list item)
                               (subseq simple-call-tree-alist newpos)))
                 (read-only-mode -1)
                 (outline-move-subtree-down arg)
                 (read-only-mode 1))))))

;; simple-call-tree-info: DONE
(defun simple-call-tree-store-state nil
  "Store the current state of the displayed call tree, and return as an alist."
  (move-beginning-of-line nil)
  (or (re-search-forward outline-regexp nil t)
      (progn (simple-call-tree-move-prev)
             (re-search-forward outline-regexp nil t)))
  (list 'narrowed (if (get-buffer simple-call-tree-buffer-name)
                      (simple-call-tree-buffer-narrowed-p))
        'depth simple-call-tree-current-maxdepth
        'level (if (get-buffer simple-call-tree-buffer-name)
                   (simple-call-tree-outline-level))
        'tree (if simple-call-tree-inverted
                  simple-call-tree-inverted-alist
                simple-call-tree-alist)
        'thisfunc (if (get-buffer simple-call-tree-buffer-name)
                      (simple-call-tree-get-function-at-point))
        'topfunc (if (get-buffer simple-call-tree-buffer-name)
                     (simple-call-tree-get-toplevel))
        'nodups simple-call-tree-nodups))
;; Restore hidden/unhidden state after sorting
;; simple-call-tree-info: DONE  
(defun simple-call-tree-restore-state (state)
  "Restore the *Simple Call Tree* buffer to the state in STATE."
  (let ((depth (or (plist-get state 'depth)
                   simple-call-tree-default-maxdepth))
        (tree (plist-get state 'tree))
        (topfunc (plist-get state 'topfunc))
        (thisfunc (plist-get state 'thisfunc))
        (level (plist-get state 'level))
        (narrowed (plist-get state 'narrowed))
        (nodups (plist-get state 'nodups)))
    (simple-call-tree-list-callers-and-functions depth tree)
    (if (or topfunc thisfunc)
        (simple-call-tree-jump-to-function (or topfunc thisfunc) t))
    (if narrowed (simple-call-tree-toggle-narrowing -1))
    (setq simple-call-tree-current-maxdepth depth
          simple-call-tree-nodups nodups)
    (if (and level (> level 1) thisfunc)
        (search-forward
         thisfunc
         (save-excursion (outline-end-of-subtree) (point)) t))))

;;; Major-mode commands bound to keys

;; simple-call-tree-info: DONE
(defun simple-call-tree-quit nil
  "Quit the *Simple Call Tree* buffer."
  (interactive)
  (let ((win (get-buffer-window simple-call-tree-buffer-name)))
    (if win (with-selected-window win
              (unless (not (featurep 'fm))
                (if fm-working (fm-toggle))
                (fm-unhighlight 0)
                (fm-unhighlight 1))
              (if (> (length (window-list)) 1)
                  (delete-window)
                (bury-buffer))))))

;; simple-call-tree-info: DONE  
(defun simple-call-tree-invert-buffer nil
  "Invert the tree in *Simple Call Tree* buffer."
  (interactive)
  (callf not simple-call-tree-inverted)
  (simple-call-tree-revert))

;; simple-call-tree-info: DONE
(defun simple-call-tree-change-maxdepth (maxdepth)
  "Alter the maximum tree depth in the *Simple Call Tree* buffer."
  (interactive "P")
  (setq simple-call-tree-current-maxdepth
        (if current-prefix-arg (prefix-numeric-value current-prefix-arg)
          (floor (abs (read-number "Maximum depth to display: " 2)))))
  (simple-call-tree-revert))

;; simple-call-tree-info: DONE  
(defun simple-call-tree-view-function nil
  "Display the source code corresponding to current header.
If the current header is a calling or toplevel function then display that function.
If it is a called function then display the position in the calling function where it is called."
  (interactive)
  ;; Following 2 lines are required to get the outline level with `simple-call-tree-outline-level'
  (beginning-of-line)
  (re-search-forward outline-regexp)
  (let* ((level (simple-call-tree-outline-level))
         (funmark (get-text-property
                   (next-single-property-change
                    (line-beginning-position)
                    'location) 'location))
         (buf (marker-buffer funmark))
         (pos (marker-position funmark)))
    (display-buffer buf)
    (with-selected-window (get-buffer-window buf)
      (goto-char pos)
      (if (featurep 'fm) (fm-highlight 1 (line-beginning-position) (line-end-position)))
      (if (eq level 1)
          (recenter 1)
        (case simple-call-tree-default-recenter
          (top (recenter 0))
          (middle (recenter))
          (bottom (recenter -1)))))))

;; simple-call-tree-info: DONE  
(cl-defun simple-call-tree-visit-function (&optional arg)
  "Visit the source code corresponding to the current header.
If the current header is a calling or toplevel function then visit that function.
If it is a called function then visit the position in the calling function where it is called.
If ARG is non-nil, or a prefix arg is supplied when called interactively then narrow
the source buffer to the function."
  (interactive "P")
  ;; Following 2 lines are required to get the outline level with `simple-call-tree-outline-level'
  (beginning-of-line)
  (re-search-forward outline-regexp)
  (let* ((level (simple-call-tree-outline-level))
         (funmark (get-text-property
                   (next-single-property-change
                    (line-beginning-position)
                    'location) 'location))
         (buf (marker-buffer funmark))
         (pos (marker-position funmark))
         (thisfunc (simple-call-tree-get-function-at-point))
         (parent (simple-call-tree-get-parent))
         (visitfunc (if simple-call-tree-inverted
                        thisfunc
                      (or parent thisfunc))))
    (pop-to-buffer buf)
    (goto-char pos)
    (unless (not (featurep 'fm))
      (fm-unhighlight 0)
      (fm-unhighlight 1))
    (if arg (simple-call-tree-narrow-to-function visitfunc pos))
    (if (eq level 1)
        (recenter 1)
      (case simple-call-tree-default-recenter
        (top (recenter 0))
        (middle (recenter))
        (bottom (recenter -1))))))

;; simple-call-tree-info: DONE
(defun simple-call-tree-compare-items (str1 str2)
  "Return non-nil if strings STR1 and STR2 correspond to the same item.
Check is done by comparing the strings and their faces.
If either string has no face then they are assumed to be the same item."
  (let ((face1 (or (get-text-property 0 'font-lock-face str1)
                   (get-text-property 0 'face str1)))
        (face2 (or (get-text-property 0 'font-lock-face str2)
                   (get-text-property 0 'face str2))))
    (and (equal str1 str2)
         (or (not face1)
             (not face2)
             (equal face1 face2)
             (member face1 face2)
             (member face2 face1)))))

;; simple-call-tree-info: DONE
(defun simple-call-tree-goto-func (fnstr)
  "Move the cursor in the *Simple Call Tree* buffer to the item with name FNSTR.
If available the 'face property of FNSTR is checked to make sure we have the correct item
for cases where there are two different types of object with the same name.
Return the position of the start of the item or nil if it couldn't be found."
  (let* ((fnregex (regexp-opt (list fnstr)))
         (lineregex (concat "^[|*]\\( \\w+\\)?\\( \\[#.\\]\\)? \\("
                            fnregex "\\)\\s-*\\(:.*:\\)?$"))
         found)
    (with-current-buffer simple-call-tree-buffer-name
      (widen)
      (goto-char (point-min))
      (if (setq found (re-search-forward lineregex nil t))
          (progn (while (and found (not (simple-call-tree-compare-items fnstr (match-string 3))))
                   (setq found (re-search-forward lineregex nil t)))
                 (if found (re-search-backward fnregex)))))))

;; simple-call-tree-info: DONE  
(cl-defun simple-call-tree-jump-to-function (fnstr &optional skipring)
  "Move cursor to the line corresponding to the function with name FNSTR.
When called interactively FNSTR will be set to the function name under point,
or if called with a prefix arg it will be prompted for.
Unless optional arg SKIPRING is non-nil (which will be true if called with a negative
prefix arg) then the function name will be added to `simple-call-tree-jump-ring'"
  (interactive (list (if (or current-prefix-arg
                             (not (simple-call-tree-get-parent)))
                         (ido-completing-read "Jump to function: " (mapcar 'caar simple-call-tree-alist))
                       (simple-call-tree-get-function-at-point))
                     (< (prefix-numeric-value current-prefix-arg) 0)))
  (let* ((narrowedp (simple-call-tree-buffer-narrowed-p)))
    (simple-call-tree-goto-func fnstr)
    (unless skipring (simple-call-tree-jump-ring-add fnstr))
    (if narrowedp (simple-call-tree-toggle-narrowing -1)
      (case simple-call-tree-default-recenter
        (top (recenter 0))
        (middle (recenter))
        (bottom (recenter -1))
        (t (recenter arg))))))

;; simple-call-tree-info: DONE
(defun simple-call-tree-jump-prev nil
  "Jump to the previous function in the `simple-call-tree-jump-ring'.
The current index into the ring is `simple-call-tree-jump-ring-index'."
  (interactive)
  (unless (ring-empty-p simple-call-tree-jump-ring)
    (let ((len (cadr simple-call-tree-jump-ring)))
      (setq simple-call-tree-jump-ring-index
            (mod (1+ simple-call-tree-jump-ring-index) len))
      (simple-call-tree-jump-to-function
       (ring-ref simple-call-tree-jump-ring
                 simple-call-tree-jump-ring-index)
       t)
      (message "Position %d in jump ring history"
               simple-call-tree-jump-ring-index))))

;; simple-call-tree-info: DONE
(defun simple-call-tree-jump-next nil
  "Jump to the next function in the `simple-call-tree-jump-ring'.
The current index into the ring is `simple-call-tree-jump-ring-index'."
  (interactive)
  (unless (ring-empty-p simple-call-tree-jump-ring)
    (let ((len (cadr simple-call-tree-jump-ring)))
      (setq simple-call-tree-jump-ring-index
            (mod (1- simple-call-tree-jump-ring-index) len))
      (simple-call-tree-jump-to-function
       (ring-ref simple-call-tree-jump-ring
                 simple-call-tree-jump-ring-index)
       t)
      (message "Position %d in jump ring history"
               simple-call-tree-jump-ring-index))))

;; simple-call-tree-info: DONE
(defun simple-call-tree-jump-ring-add (fnstr)
  "Add the function at point to the jump-ring.
Adds the string FNSTR to the `simple-call-tree-jump-ring' at the position indicated by
`simple-call-tree-jump-ring-index', and reset `simple-call-tree-jump-ring-index' to 0.
When called interactively the name of the function at point is used for FNSTR."
  (interactive (list (simple-call-tree-get-function-at-point)))
  (setf (cadr simple-call-tree-jump-ring) (- (ring-length simple-call-tree-jump-ring)
                                             simple-call-tree-jump-ring-index))
  (setq simple-call-tree-jump-ring-index 0)
  (ring-insert simple-call-tree-jump-ring fnstr)
  (message "Added %s to `simple-call-tree-jump-ring'" fnstr))

;; simple-call-tree-info: DONE
(defun simple-call-tree-move-top nil
  "Move cursor to the parent of this function."
  (interactive)
  (with-current-buffer simple-call-tree-buffer-name
    (condition-case err
        (outline-up-heading simple-call-tree-current-maxdepth)
      (error nil))))

;; simple-call-tree-info: DONE
(defun simple-call-tree-move-next nil
  "Move cursor to the next item."
  (interactive)
  (outline-next-visible-heading 1)
  (let ((nextpos (next-single-property-change (point) 'face)))
    (if nextpos (goto-char nextpos))))

;; simple-call-tree-info: DONE
(defun simple-call-tree-move-prev nil
  "Move cursor to the previous item."
  (interactive)
  (outline-previous-visible-heading 1)
  (goto-char (next-single-property-change (point) 'face)))

;; simple-call-tree-info: DONE
(defun simple-call-tree-move-next-samelevel nil
  "Move cursor to the next item at the same level as the current one."
  (interactive)
  (outline-forward-same-level 1)
  (goto-char (next-single-property-change (point) 'face)))

;; simple-call-tree-info: DONE
(defun simple-call-tree-move-prev-samelevel nil
  "Move cursor to the previous item at the same level as the current one."  
  (interactive)
  (outline-backward-same-level 1)
  (goto-char (next-single-property-change (point) 'face)))

;; simple-call-tree-info: DONE  
(defun simple-call-tree-move-next-marked nil
  "Move cursor to the next marked item."
  (interactive)
  (end-of-line)
  (re-search-forward "^\\*"))

;; simple-call-tree-info: DONE  
(defun simple-call-tree-move-prev-marked nil
  "Move cursor to the next marked item."
  (interactive)
  (beginning-of-line)
  (re-search-backward "^\\*"))

;; simple-call-tree-info: DONE
(defun simple-call-tree-buffer-narrowed-p nil
  "Return non-nil if *Simple Call Tree* buffer is narrowed."
  (with-current-buffer simple-call-tree-buffer-name
    (or (/= (point-min) 1)
        (/= (point-max) (1+ (buffer-size))))))

;; simple-call-tree-info: DONE  
(defun simple-call-tree-toggle-narrowing (&optional state)
  "Toggle narrowing of *Simple Call Tree* buffer.
If optional arg STATE is > 0, or if called interactively with a positive prefix arg,
then widen the buffer. If STATE is < 0 or if called interactively with a negative
prefix arg then narrow the buffer.
Otherwise toggle the buffer between narrow and wide state.
When narrowed, the buffer will be narrowed to the subtree at point."
  (interactive "P")
  (with-current-buffer simple-call-tree-buffer-name
    (let ((pos (point)))
      (if (or (and state (> (prefix-numeric-value state) 0))
              (and (not state) (simple-call-tree-buffer-narrowed-p)))
          (widen)
        (if (looking-at "^[|*] ")
            (re-search-forward "^[|*] ")
          (re-search-backward "^[|*] "))
        (outline-mark-subtree)
        (narrow-to-region (region-beginning) (region-end))
        (let (select-active-regions) (deactivate-mark))
	(fit-window-to-buffer))
      (goto-char pos))))

;; simple-call-tree-info: DONE  
(defun simple-call-tree-toggle-duplicates nil
  "Toggle the inclusion of duplicate sub-branches in the call tree."
  (interactive)
  (callf not simple-call-tree-nodups)
  (with-current-buffer simple-call-tree-buffer-name
    (simple-call-tree-revert)))

;; simple-call-tree-info: DONE  
(cl-defun simple-call-tree-apply-command (cmd &optional
                                            (funcs (or simple-call-tree-marked-items
                                                       (list (or (unless simple-call-tree-inverted
                                                                   (simple-call-tree-get-parent))
                                                                 (simple-call-tree-get-function-at-point))))))
  "Apply command CMD on function(s) FUNCS.
By default FUNCS is set to the list of marked items or the function at point if there are no marked items.
The command CMD will be called interactively on each function after switching to the source code buffer,
and narrowing the buffer around the function."
  (interactive (list (read-command "Command: ")))
  (dolist (func funcs)
    (let ((buf (marker-buffer
                (second (car (simple-call-tree-get-item func))))))
      (switch-to-buffer-other-window buf)
      (condition-case err
          (progn
            (simple-call-tree-narrow-to-function func)
            (call-interactively cmd))
        ((quit error) nil))
      (widen)))
  (switch-to-buffer-other-window simple-call-tree-buffer-name))

;; simple-call-tree-info: DONE
(defun simple-call-tree-query-replace nil
  "Perform query-replace on the marked items or the item at point in the *Simple Call Tree* buffer.
This just calls `simple-call-tree-apply-command' with the `query-replace' command."
  (interactive)
  (simple-call-tree-apply-command 'query-replace))

;; simple-call-tree-info: DONE  
(defun simple-call-tree-query-replace-regexp nil
  "Perform `query-replace-regexp' on the marked items or the item at point in the *Simple Call Tree* buffer.
This just calls `simple-call-tree-apply-command' with the `query-replace-regexp' command."
  (interactive)
  (simple-call-tree-apply-command 'query-replace-regexp))

;; simple-call-tree-info: DONE  
(defun simple-call-tree-bookmark nil
  "Set bookmarks the marked items or the item at point in the *Simple Call Tree* buffer."
  (interactive)
  (simple-call-tree-apply-command 'bookmark-set))

;; simple-call-tree-info: DONE
(defun simple-call-tree-delete-other-windows nil
  "Make the *Simple Call Tree* buffer fill the frame."
  (interactive)
  (unless (not (featurep 'fm))
    (fm-unhighlight 0)
    (fm-unhighlight 1)
    (setq fm-working nil))
  (delete-other-windows))

;; Change so that it only marks visible items
;; simple-call-tree-info: DONE  
(defun simple-call-tree-mark (func)
  "Mark the item named FUNC.
If FUNC is nil then mark the current line and add the item to `simple-call-tree-marked-items'."
  (interactive (list (or (simple-call-tree-get-parent)
                         (simple-call-tree-get-function-at-point))))
  (if (and (or (not func) (simple-call-tree-goto-func func))
           (progn (beginning-of-line)
                  (re-search-forward "^\\([|*]\\)\\( +\\w+\\)?\\( \\[#.\\]\\)? \\(\\S-+\\)"
                                     (line-end-position) t)))
      (progn 
        (read-only-mode -1)
        (replace-match "*" nil t nil 1)
        (read-only-mode 1)
        (add-to-list 'simple-call-tree-marked-items
                     (or func (match-string 4))
                     nil 'simple-call-tree-compare-items)
        (if (called-interactively-p)
            (simple-call-tree-move-next-samelevel)))))

;; simple-call-tree-info: DONE  
(defun simple-call-tree-unmark (func)
  "Unmark the item named FUNC."
  (interactive (list (or (simple-call-tree-get-parent)
                         (simple-call-tree-get-function-at-point))))
  (if (simple-call-tree-goto-func func)
      (let ((start (line-beginning-position)))
        (read-only-mode -1)
        (replace-regexp "\\*" "|" nil start (1+ start))
        (read-only-mode 1)
        (callf2 remove* func simple-call-tree-marked-items
          :test 'simple-call-tree-compare-items)
        (if (called-interactively-p)
            (simple-call-tree-move-next-samelevel)))))

;; Easier to remember the function name than the raw code.
;; simple-call-tree-info: DONE
(cl-defun simple-call-tree-marked-p (str &optional (alist simple-call-tree-marked-items))
  "Return non-nil if STR is in `simple-call-tree-marked-items'.
Membership is checked by the contents and font of STR.
If optional arg ALIST is supplied then use alist instead of `simple-call-tree-marked-items'."
  (member* str alist :test 'simple-call-tree-compare-items))

;; simple-call-tree-info: DONE  
(defun simple-call-tree-unmark-all nil
  "Unmark all items."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (read-only-mode -1)
    (replace-regexp "\\*" "|" nil (point-min) (point-max))
    (read-only-mode 1))
  (setq simple-call-tree-marked-items nil))

;; simple-call-tree-info: DONE  
(defun simple-call-tree-mark-by-pred (pred &optional unmark)
  "Mark items in `simple-call-tree-alist' that return non-nil when passed as an arg to PRED function.
If UNMARK is non-nil unmark the items instead.
PRED should be a function which takes an item from `simple-call-tree-alist' as its only argument."
  (save-excursion
    (dolist (item simple-call-tree-alist)
      (if (funcall pred item)
          (if unmark
              (simple-call-tree-unmark (caar item))
            (simple-call-tree-mark (caar item)))))))

;; simple-call-tree-info: DONE  
(defun simple-call-tree-toggle-marks nil
  "Toggle marks (unmarked become marked and marked become unmarked)."
  (interactive)
  (let ((marked (copy-list simple-call-tree-marked-items)))
    (simple-call-tree-unmark-all)
    (simple-call-tree-mark-by-pred
     (lambda (x) (not (simple-call-tree-marked-p (caar x) marked))))))
  
;; simple-call-tree-info: DONE  
(defun simple-call-tree-mark-by-name (regex &optional unmark)
  "Mark all items with names matching regular expression REGEX.
If UNMARK is non-nil unmark the items instead."
  (interactive (list (read-regexp "Regular expression matching name: ")
                     current-prefix-arg))
  (simple-call-tree-mark-by-pred
   (lambda (x) (string-match regex (caar x)))
   unmark))

;; simple-call-tree-info: DONE  
(defun simple-call-tree-mark-by-source (regex &optional unmark)
  "Mark all items with source code matching regular expression REGEX.
If UNMARK is non-nil unmark the items instead."
  (interactive (list (read-regexp "Regular expression matching source code: ")))
  (simple-call-tree-mark-by-pred
   (lambda (x)
     (let ((buf (marker-buffer (second (car x))))
           (start (marker-position (second (car x))))
           (end (marker-position (third (car x)))))
       (with-current-buffer buf
         (save-excursion
           (widen)
           (goto-char start)
           (re-search-forward regex end t)))))
   unmark))

;; simple-call-tree-info: DONE  
(defun simple-call-tree-mark-by-tag-match (match &optional todo-only unmark)
  "Mark all items with code matching regular expression REGEX.
If UNMARK is non-nil unmark the items instead."
  (interactive (list (read-string "Match: ")
                     (y-or-n-p "TODO items only?")
                     current-prefix-arg))
  (let ((matcher (cdr (org-make-tags-matcher match))))
    (simple-call-tree-mark-by-pred
     (lambda (x)
       (let ((todo (fourth (car x)))
             (tags-list (sixth (car x))))
         (eval matcher)))
     unmark)))

;; simple-call-tree-info: DONE  
(defun simple-call-tree-mark-by-priority (value &optional unmark)
  "Mark all items with priority VALUE.
If UNMARK is non-nil unmark the items instead."
  (interactive (progn (message "Priority %c-%c, SPC to remove: "
                               simple-call-tree-org-highest-priority simple-call-tree-org-lowest-priority)
                      (list (read-char-exclusive) current-prefix-arg)))
  (if (= value ?\ ) (setq value nil)
    (callf upcase value))
  (simple-call-tree-mark-by-pred (lambda (x) (eq (fifth (car x)) value)) unmark))

;; simple-call-tree-info: DONE
(defun simple-call-tree-mark-by-todo (regex &optional unmark)
  "Mark all items with TODO state matching regular expression REGEX.
If UNMARK is non-nil unmark the items instead.
If REGEX is nil or \"\" then mark/unmark items with no TODO state"
  (interactive (list (read-regexp "Regexp matching TODO states (leave blank to match items with no TODO)")
                     current-prefix-arg))
  (if (equal regex "") (setq regex nil))
  (simple-call-tree-mark-by-pred
   (lambda (x)
     (let ((todo (fourth (car x)))
           (case-fold-search t))
       (or (and regex todo (string-match regex todo))
           (not (or regex todo)))))
   unmark))

;; Get list of all possible faces by checking `simple-call-tree-major-mode-alist' with first
;; element of `simple-call-tree-alist'.
;; simple-call-tree-info: DONE  
(defun simple-call-tree-mark-by-face (face &optional unmark)
  "Mark all items with display face FACE.
If UNMARK is non-nil unmark the items instead."
  (interactive (let* ((modevals (with-current-buffer
                                    (marker-buffer
                                     (second (caar simple-call-tree-alist)))
                                  major-mode))
                      (faces (or (second (assoc modevals simple-call-tree-major-mode-alist))
                                 simple-call-tree-default-valid-fonts)))
                 (list (intern-soft (ido-completing-read "Face" (mapcar 'symbol-name faces)))
                       current-prefix-arg)))
  (simple-call-tree-mark-by-pred
   (lambda (x)
     (let* ((str (caar x))
            (face2 (or (get-text-property 0 'face str)
                       (get-text-property 0 'font-lock-face str))))
       (or (eq face face2)
           (member face face2)
           (member face2 face))))
   unmark))

;; simple-call-tree-info: DONE  
(defun simple-call-tree-mark-by-buffer (buf &optional unmark)
  "Mark all items corresponding to source code in buffer BUF.
BUF may be a buffer object or the name of a buffer.
If UNMARK is non-nil unmark the items instead."
  (interactive (list (ido-completing-read
                      "Buffer: "
                      (cl-remove-duplicates
                       (mapcar (lambda (x) (buffer-name (marker-buffer (second (car x)))))
                               simple-call-tree-alist)
                       :test 'equal)
                      nil t)
                     current-prefix-arg))
  (simple-call-tree-mark-by-pred
   (lambda (x) (equal (buffer-name (marker-buffer (second (car x))))
                      (if (stringp buf) buf (buffer-name buf))))
   unmark))

;; simple-call-tree-info: DONE  
(defun simple-call-tree-kill-marked nil
  "Remove all marked items from the *Simple Call Tree* buffer."
  (interactive)
  (read-only-mode -1)  
  (dolist (func simple-call-tree-marked-items)
    (simple-call-tree-goto-func func)
    (outline-mark-subtree)
    (kill-region (region-beginning) (region-end))
    (condition-case err (kill-line) (error nil))
    (callf2 remove func simple-call-tree-marked-items))
  (read-only-mode 1))

;; This command should be bound to g key for compatibility with dired.
;; simple-call-tree-info: DONE  
(defun simple-call-tree-revert nil
  "Redisplay the *Simple Call Tree* buffer."
  (interactive)
  (simple-call-tree-restore-state (simple-call-tree-store-state)))

(unless (not (featurep 'fm))
  (add-to-list 'fm-modes '(simple-call-tree-mode simple-call-tree-visit-function))
  (add-hook 'simple-call-tree-mode-hook 'fm-start))
(unless (not (featurep 'hl-line))
  (add-hook 'simple-call-tree-mode-hook
            (lambda nil
              (hl-line-mode 1))))

(provide 'simple-call-tree)

;;; simple-call-tree.el ends here

;; (magit-push)
;; (yaoddmuse-post "EmacsWiki" "simple-call-tree.el" (buffer-name) (buffer-string) "update")

