;;; simple-call-tree+.el --- Extensions to simple-call-tree

;; Filename: simple-call-tree+.el
;; Description: extensions to simple-call-tree
;; Author: Joe Bloggs <vapniks@yahoo.com>
;; Maintainer: Joe Bloggs <vapniks@yahoo.com>
;; Copyleft (Ↄ) 2012, Joe Bloggs, all rites reversed.
;; Created: 2012-11-01 21:28:07
;; Version: 1.4
;; Last-Updated: 2013-10-02 17:00:00
;;           By: Joe Bloggs
;; URL: http://www.emacswiki.org/emacs/download/simple-call-tree+.el
;;      https://github.com/vapniks/simple-call-tree-ext
;; Keywords: programming
;; Compatibility: GNU Emacs 24.2.1
;;
;; Features that might be required by this library:
;;
;; `anaphora' `thingatpt' `outline-magic' `fm' `org' `cl'
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

;;; Installation:
;;
;; Put simple-call-tree+.el in a directory in your load-path, e.g. ~/.emacs.d/
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


;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;;  `simple-call-tree-default-recenter'
;;    How to recenter the window after moving to another function in the "*Simple Call Tree*" buffer.
;;  `simple-call-tree-default-valid-fonts'
;;    List of fonts to use for finding objects to include in the call tree.
;;  `simple-call-tree-default-invalid-fonts'
;;    List of fonts that should not be in the text property of any valid token.
;;  `simple-call-tree-default-sort-method'
;;    The default sort method to use when a call tree is newly created.
;;  `simple-call-tree-default-maxdepth'
;;    The depth at which new call trees should be displayed.
;;  `simple-call-tree-major-mode-alist'
;;    Alist of major modes, and information to use for identifying objects for the simple call tree.
;;  `simple-call-tree-org-link-style'
;;    Style used for links of child headers when exporting org tree using `simple-call-tree-export-org-tree'.
;;  `simple-call-tree-jump-ring-max'
;;    Maximum number of elements in `simple-call-tree-jump-ring', before old elements are removed.

;;
;; All of the above can customized by:
;;      M-x customize-group RET simple-call-tree+ RET
;;

;;; Acknowledgements:
;;
;; Alex Schroeder - the creator of the original simple-call-tree.el
;;                  (available here: http://www.emacswiki.org/emacs/simple-call-tree.el)
;;

;;; TODO
;;
;; If anyone wants to implement the following ideas, please do:
;; More reliable code for building tree (handle duplicate function names properly).
;; Fix code so that we can have several calls to the same function in the same tree.
;; Code for marking functions (like with files in dired mode) and then applying operations to the marked functions.
;; Code for rearranging functions.
;; Code for renaming functions.

;;; Require
(require 'thingatpt)
(require 'outline-magic nil t)
(require 'fm nil t)
(require 'anaphora)
(eval-when-compile (require 'cl))
(require 'org)
;;; Code:

;; simple-call-tree-info: DELEGATED  
(defgroup simple-call-tree nil
  "Simple call tree - display a simple call tree for functions in a buffer."
  :group 'tools
  :link '(url-link "http://www.emacswiki.org/SimpleCallTree"))

(defcustom simple-call-tree-default-recenter 'middle
  "How to recenter the window after moving to another function in the \"*Simple Call Tree*\" buffer.
Can be one of the following symbols: 'top 'middle 'bottom.
This variable is used by the `simple-call-tree-jump-to-function' function when no prefix arg is given,
and by `simple-call-tree-visit-function' and `simple-call-tree-view-function'."
  :group 'simple-call-tree
  :type '(choice (const :tag "Top" top)
                 (const :tag "Middle" middle)
                 (const :tag "Bottom" bottom)))

(defcustom simple-call-tree-default-valid-fonts '(font-lock-function-name-face
                                                  font-lock-variable-name-face)
  "List of fonts to use for finding objects to include in the call tree."
  :group 'simple-call-tree
  :type '(repeat face))

(defcustom simple-call-tree-default-invalid-fonts '(font-lock-comment-face
                                                    font-lock-string-face
                                                    font-lock-doc-face
                                                    font-lock-keyword-face
                                                    font-lock-warning-face
                                                    font-lock-preprocessor-face)
  "List of fonts that should not be in the text property of any valid token."
  :group 'simple-call-tree
  :type '(repeat face))

(defcustom simple-call-tree-default-sort-method 'position
  "The default sort method to use when a call tree is newly created.
The children of each header will be sorted separately."
  :group 'simple-call-tree
  :type '(choice (const :tag "Sort by position" position)
                 (const :tag "Sort alphabetically" alphabet)
                 (const :tag "Sort by number of children" numchild)
                 (const :tag "Sort by face" face)))

(defcustom simple-call-tree-default-maxdepth 2
  "The depth at which new call trees should be displayed."
  :group 'simple-call-tree
  :type 'integer)

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
                  haskell-ds-forward-decl
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
                     (end-of-defun)))))
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
or a function of no args which moves point to the start of the next function in the buffer.

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

;; simple-call-tree-info: APPT  :this:test:
(defcustom simple-call-tree-org-todo-keywords nil
  "List of different TODO keywords, if nil then `org-todo-keywords' will be used."
  :group 'simple-call-tree
  :type 'list)

(defcustom simple-call-tree-org-highest-priority org-highest-priority
  "See `org-highest-priority'")

(defcustom simple-call-tree-org-lowest-priority org-lowest-priority
  "See `org-lowest-priority'")

(defcustom simple-call-tree-org-tag-alist org-tag-alist
  "See `org-tag-alist'")

(defun simple-call-tree-org-todo-keywords nil
  "Return list of all TODO states.
If `simple-call-tree-org-todo-keywords' is nil then the states in `org-todo-keywords' are returned
as a flat list."
  (or simple-call-tree-org-todo-keywords
      (remove-if (lambda (x) (or (symbolp x)
                                 (equal x "|")))
                 (mapcan 'identity org-todo-keywords))))

(defcustom simple-call-tree-org-priority-levels nil
  '("A" "B" "C" "D")
  :group 'simple-call-tree
  :type 'list)

;; Saves a little typing
(defmacro whilelast (&rest forms)
  `(while (progn ,@forms)))

(defmacro whilenotlast (&rest forms)
  `(while (not (progn ,@forms))))

(defmacro simple-call-tree-get-item (func)
  "Return the item in `simple-call-tree-alist' corresponding with function named FUNC."
  `(assoc-if (lambda (x) (string= (car x) ,func)) simple-call-tree-alist))

(defun simple-call-tree-tags-to-string (tags)
  (if (> (length tags) 0)
      (concat ":" (mapconcat 'identity tags ":") ":")))

(defun simple-call-tree-string-to-tags (str)
  (aif str (split-string (substring-no-properties it) "[ \f\t\n\r\v,;:]+" t)))

;; This is still not exactly right: it will match both abc & abc' on abc'
;; where ' could be any char in the expression prefix syntax class.
;; This happens in haskell mode for example when you have defined two functions
;; named func and func' for example.
(defun simple-call-tree-symbol-as-regexp (symbolname)
  (let* ((modevals (assoc major-mode simple-call-tree-major-mode-alist))
         (start (seventh modevals))
         (end (eighth modevals)))
    (concat (or start "\\_<")
            (regexp-opt (list symbolname))
            (or end "\\_>"))))

;; Major-mode for simple call tree
(define-derived-mode simple-call-tree-mode outline-mode "Simple Call Tree"
  "The major-mode for the one-key menu buffer."
  :group 'simple-call-tree
  (setq simple-call-tree-mode-map (make-keymap)
        buffer-read-only nil)
  (outline-minor-mode 1)
  (setq outline-regexp "^|\\([-<>]*\\)\\(\\( \\w+\\)?\\)\\(\\( \\[#.\\]\\)?\\) "
        outline-level 'simple-call-tree-outline-level)
  ;; Set keymap
  (define-key simple-call-tree-mode-map (kbd "q") 'simple-call-tree-quit)
  (define-prefix-command 'simple-call-tree-sort-map)
  (define-key simple-call-tree-mode-map (kbd "s") 'simple-call-tree-sort-map)
  (define-key simple-call-tree-mode-map (kbd "s a") 'simple-call-tree-sort-alphabetically)
  (define-key simple-call-tree-mode-map (kbd "s p") 'simple-call-tree-sort-positionally)
  (define-key simple-call-tree-mode-map (kbd "s f") 'simple-call-tree-sort-by-face)
  (define-key simple-call-tree-mode-map (kbd "s c") 'simple-call-tree-sort-by-num-children)
  (define-key simple-call-tree-mode-map (kbd "s r") 'simple-call-tree-reverse)
  (if (featurep 'outline-magic)
      (define-key simple-call-tree-mode-map (kbd "<tab>") 'outline-cycle)
    (define-key simple-call-tree-mode-map (kbd "<tab>") 'outline-toggle-children))
  (define-key simple-call-tree-mode-map (kbd "a") 'show-all)
  (define-key simple-call-tree-mode-map (kbd "1") 'simple-call-tree-delete-other-windows)
  (define-key simple-call-tree-mode-map (kbd "h") 'hide-sublevels)
  (define-key simple-call-tree-mode-map (kbd "SPC") 'simple-call-tree-view-function)
  (define-key simple-call-tree-mode-map (kbd "C-o") 'simple-call-tree-view-function)
  (define-key simple-call-tree-mode-map (kbd "v") 'simple-call-tree-view-function)
  (define-key simple-call-tree-mode-map (kbd "<return>") 'simple-call-tree-visit-function)
  (define-key simple-call-tree-mode-map (kbd "o") 'simple-call-tree-visit-function)
  (define-key simple-call-tree-mode-map (kbd "j") 'simple-call-tree-jump-to-function)
  (define-key simple-call-tree-mode-map (kbd "J") '(lambda nil (interactive)
                                                     (setq current-prefix-arg 1)
                                                     (call-interactively 'simple-call-tree-jump-to-function)))
  (define-key simple-call-tree-mode-map (kbd "C-j") '(lambda nil (interactive)
                                                       (setq current-prefix-arg 1)
                                                       (call-interactively 'simple-call-tree-jump-to-function)))
  (define-key simple-call-tree-mode-map (kbd ".") 'simple-call-tree-jump-ring-add)
  (define-key simple-call-tree-mode-map (kbd "u") 'simple-call-tree-move-up)
  (define-key simple-call-tree-mode-map (kbd "^") 'simple-call-tree-move-up)
  (define-key simple-call-tree-mode-map (kbd "n") 'simple-call-tree-move-next)
  (define-key simple-call-tree-mode-map (kbd "p") 'simple-call-tree-move-prev)
  (define-key simple-call-tree-mode-map (kbd "C-f") 'simple-call-tree-move-next-samelevel)
  (define-key simple-call-tree-mode-map (kbd "C-b") 'simple-call-tree-move-prev-samelevel)
  (define-key simple-call-tree-mode-map (kbd "<S-down>") 'simple-call-tree-move-next-samelevel)
  (define-key simple-call-tree-mode-map (kbd "<S-up>") 'simple-call-tree-move-prev-samelevel)
  (define-key simple-call-tree-mode-map (kbd "N") 'simple-call-tree-move-next-samelevel)
  (define-key simple-call-tree-mode-map (kbd "P") 'simple-call-tree-move-prev-samelevel)
  (define-key simple-call-tree-mode-map (kbd "i") 'simple-call-tree-invert-buffer)
  (define-key simple-call-tree-mode-map (kbd "d") 'simple-call-tree-change-maxdepth)
  (define-key simple-call-tree-mode-map (kbd "D") 'simple-call-tree-toggle-duplicates)
  (define-key simple-call-tree-mode-map (kbd "/") 'simple-call-tree-toggle-narrowing)
  (define-key simple-call-tree-mode-map (kbd "<") 'simple-call-tree-jump-prev)
  (define-key simple-call-tree-mode-map (kbd ">") 'simple-call-tree-jump-next)
  (define-key simple-call-tree-mode-map (kbd "m") 'simple-call-tree-bookmark)
  (define-key simple-call-tree-mode-map (kbd "%") 'simple-call-tree-query-replace)
  (define-key simple-call-tree-mode-map (kbd "C-%") 'simple-call-tree-query-replace-regexp)
  (define-key simple-call-tree-mode-map (kbd "C-M-x") 'simple-call-tree-eval-defun)
  (define-key simple-call-tree-mode-map (kbd "!") 'simple-call-tree-apply-command)
  (define-key simple-call-tree-mode-map (kbd "M-p") 'simple-call-tree-jump-prev)
  (define-key simple-call-tree-mode-map (kbd "M-n") 'simple-call-tree-jump-next)
  (define-key simple-call-tree-mode-map (kbd "w") 'widen)
  (define-key simple-call-tree-mode-map (kbd "R") 'simple-call-tree-build-tree)
  (define-key simple-call-tree-mode-map (kbd "<S-right>") 'simple-call-tree-next-todo)
  (define-key simple-call-tree-mode-map (kbd "<S-left>") 'simple-call-tree-prev-todo)
  (define-key simple-call-tree-mode-map (kbd "<S-up>") 'simple-call-tree-up-priority)
  (define-key simple-call-tree-mode-map (kbd "<S-down>") 'simple-call-tree-down-priority)
  (define-key simple-call-tree-mode-map (kbd "C-c C-c") 'simple-call-tree-set-tags)
  (define-key simple-call-tree-mode-map (kbd "C-c C-q") 'simple-call-tree-set-tags)
  (use-local-map simple-call-tree-mode-map)
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
      ["Operate on function at point..."
       (keymap "Operate"
        (queryreplace menu-item "Replace String..." simple-call-tree-query-replace
                      :help "Perform query-replace on the function at point")
        (queryreplaceregex menu-item "Replace Regexp..." simple-call-tree-query-replace-regexp
                           :help "Perform query-replace-regexp on the function at point")
        (eval menu-item "Evaluate" simple-call-tree-eval-defun
              :help "Evaluate the function at point. With a prefix arg instrument it for debugging.")
        (bookmark menu-item "Add Bookmark" simple-call-tree-bookmark
                  :help "Create a bookmark for the position corresponding to the function at point")
        (arbitrary menu-item "Apply Arbitrary Command..." simple-call-tree-apply-command
                   :help "Apply an arbitrary elisp command to the function at point"))]
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
      ["Parent Branch" simple-call-tree-move-up
       :help "Goto the parent branch of this branch"]
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
       :visible (featurep 'outline-magic)
       :keys "<tab>"]
      ["Toggle Children Visibility" outline-toggle-children
       :help "Toggle the visibility of the children of this header"
       :visible (not (featurep 'outline-magic))
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
      ["Sort Tree..." (keymap "Sort"
                       (alpha menu-item "Alphabetically" simple-call-tree-sort-alphabetically)
                       (position menu-item "Positionally" simple-call-tree-sort-positionally)
                       (numchild menu-item "Number of children" simple-call-tree-sort-by-num-children)
                       (face menu-item "By face" simple-call-tree-sort-by-face)
                       (reverse menu-item "Reverse order" simple-call-tree-reverse))]
      ["Change Depth..." simple-call-tree-change-maxdepth
       :help "Change the depth of the tree"]
      ["Toggle Narrowing" simple-call-tree-toggle-narrowing
       :help "Toggle between narrow/wide buffer"
       :style toggle
       :selected (simple-call-tree-buffer-narrowed-p)]
      ["---" "---"]
      ["Export As Org-Tree" simple-call-tree-export-org-tree
       :help "Export the call tree to an org-mode buffer"]))
  (setq mode-line-format
        (append
         (subseq mode-line-format 0
                 (1+ (position 'mode-line-buffer-identification
                               mode-line-format)))
         (list '(:eval (format (concat "|Maxdepth=%d|"
                                       "Sorted "
                                       (case simple-call-tree-current-sort-order
                                         (position "by position|")
                                         (alphabet "alphabetically|")
                                         (numchild "by number of children|")
                                         (face "by face|")))
                               simple-call-tree-current-maxdepth)))
         (subseq mode-line-format
                 (+ 2 (position 'mode-line-buffer-identification
                                mode-line-format))))))

(defvar simple-call-tree-alist nil
  "Alist of functions and the functions they call, and markers for their locations.
Each element is a list of lists. The first list in each element is in the form
 (FUNC START END) where FUNC is the function name and START & END are markers for the
positions of the start and end of the function. The other lists contain information
for functions called by FUNC, and are in the form (FUNC2 POS) where FUNC2 is the name
of the called function and POS is the position of the call.")

(defvar simple-call-tree-inverted-alist nil
  "Alist of functions and the functions that call them, and markers for their locations.
This is an inverted version of `simple-call-tree-alist'.")

(defvar simple-call-tree-inverted nil
  "Indicates if the *Simple Call Tree* buffer is currently inverted or not.
If non-nil then children correspond to callers of parents in the outline tree.
Otherwise it's the other way around.")

(defvar simple-call-tree-current-maxdepth nil
  "The current maximum depth of the tree in the *Simple Call Tree* buffer.
The minimum value is 0 which means show top level functions only.")

(defcustom simple-call-tree-jump-ring-max 20
  "Maximum number of elements in `simple-call-tree-jump-ring', before old elements are removed."
  :group 'simple-call-tree
  :type 'integer)

(defvar simple-call-tree-jump-ring (make-ring simple-call-tree-jump-ring-max)
  "Ring to hold history of functions jumped to in *Simple Call Tree* buffer.")

(defvar simple-call-tree-jump-ring-index 0
  "The current position in the jump ring.")

(defvar simple-call-tree-current-sort-order simple-call-tree-default-sort-method
  "The current sort order of the call tree.
See `simple-call-tree-default-sort-method' for possible values.")

(defvar simple-call-tree-nodups nil
  "If non-nil then duplicate sub-branches will not be included in the tree.
I.e. if a function makes multiple calls to the same function then only one of these calls will
be shown in the tree.")

(defvar simple-call-tree-buffers nil
  "Buffers analyzed to create the simple-call-tree.")

(defvar simple-call-tree-tags-regexp
  "simple-call-tree-info:\\s-*\\(\\w+\\)?\\s-*\\(\\[#[A-Z]\\]\\)?\\s-*\\(:[a-zA-Z0-9:,;-_]+:\\)?"
  "Regular expression to match org properties (todo, priority & tags) in source code.")

;;; Functions from simple-call-tree.el (some are rewritten)
(defun simple-call-tree-add (start end alist)
  "Add tokens between START and END to ALIST.
ALIST is an item of simple-call-tree-alist."
  (dolist (item simple-call-tree-alist)
    (goto-char start)
    (while (re-search-forward (simple-call-tree-symbol-as-regexp (caar item))
                              end t)
      ;; need to go back so that the text properties are read correctly
      (left-word 1)
      (if (simple-call-tree-valid-face-p)
          (setcdr alist (cons (list (caar item) (point-marker)) (cdr alist))))
      (right-word 1))))

(defun* simple-call-tree-analyze (&optional (buffers (list (current-buffer))))
  "Analyze the current buffer, or the buffers in list BUFFERS.
The result is stored in `simple-call-tree-alist'.
Optional arg BUFFERS is a list of buffers to analyze together.
By default it is set to a list containing the current buffer."
  (interactive)
  (setq simple-call-tree-alist nil)
  ;; First add all the functions defined in the buffers to simple-call-tree-alist.
  (let (pos oldpos count1 pair nextfunc item endtest oldpos startmark endmark tags)
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
            (setq tags (simple-call-tree-get-tags))
            (setq startmark (point-marker))
            (cond ((functionp endtest) (funcall endtest))
                  (endtest (end-of-defun))
                  ((setq pair (simple-call-tree-next-func pos))
                   (goto-char (- (car pair) (length (cdr pair)))))
                  (t (goto-char (point-max))))
            (setq endmark (point-marker))
            (add-to-list 'simple-call-tree-alist (list (list nextfunc startmark endmark
                                                             (car tags)
                                                             (second tags)
                                                             (third tags))))
            (setq count1 (1+ count1))
            (message "Identifying functions...%d:%s" count1 nextfunc)))))
    ;; Now find functions called
    (loop for item in simple-call-tree-alist
          for count2 from 1
          for buf = (marker-buffer (second (car item)))
          for start = (marker-position (second (car item)))
          for end = (marker-position (third (car item)))
          do (with-current-buffer buf
               (save-excursion
                 (message "Identifying functions called...%d/%d" count2 count1)
                 (simple-call-tree-add start end item))))
    (message "Creating inverted list...")
    (setq simple-call-tree-inverted-alist
          (simple-call-tree-invert))
    (message "simple-call-tree done")))

(defun simple-call-tree-invert nil
  "Invert `simple-call-tree-alist' and return the result."
  (let (result)
    (dolist (item simple-call-tree-alist)
      (let* ((caller (first item))
             (callername (first caller))
             (callees (cdr item)))
        (unless (assoc-if (lambda (x) (string= (car x) callername)) result)
          (setq result (cons (list (list callername (second caller) (third caller)))
                             result)))
        (dolist (callee callees)
          (let* ((calleename (first callee))
                 (callerpos (second callee))
                 (calleeitem (car (simple-call-tree-get-item calleename)))
                 (elem (assoc-if (lambda (x) (string= (car x) calleename)) result)))
            (if elem
                (setcdr elem (cons (list callername callerpos) (cdr elem)))
              (setq result (cons (list (list calleename
                                             (second calleeitem)
                                             (third calleeitem))
                                       (list callername callerpos))
                                 result)))))))
    result))

;;; New functions (not in simple-call-tree.el)

(defun simple-call-tree-valid-face-p nil
  "Return t if face at point is a valid function name face, and nil otherwise."
  (let ((faces (get-text-property (point) 'face))
        (invalidfonts (or (third (assoc major-mode simple-call-tree-major-mode-alist))
                          simple-call-tree-default-invalid-fonts)))
    (unless (listp faces) (setq faces (list faces)))
    (not (intersection faces invalidfonts))))

(defun* simple-call-tree-get-function-at-point (&optional (buf "*Simple Call Tree*"))
  "Return the name of the function nearest point in the *Simple Call Tree* buffer.
If optional arg BUF is supplied then use BUF instead of the *Simple Call Tree* buffer.
If there is no function on this line of the *Simple Call Tree* buffer, return nil."
  (with-current-buffer buf
    (if (equal buf "*Simple Call Tree*")
        (save-excursion
          (goto-char (line-beginning-position))
          (if (re-search-forward (concat outline-regexp "\\(\\S-+\\)")
                                 (line-end-position) t)
              (substring-no-properties (match-string 6))
            (previous-line)
            (re-search-forward (concat outline-regexp "\\(\\S-+\\)")
                               (line-end-position) t)
            (substring-no-properties (match-string 6))))
      (symbol-name (if (functionp 'symbol-nearest-point)
                       (symbol-nearest-point)
                     (symbol-at-point))))))

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
          (funcall nextfunc)
          (setq start (point)))
      (while (and (not (and (memq (get-text-property start 'face) validfonts)
                            (not (memq (get-text-property start 'face) invalidfonts))
                            (or (not starttest)
                                (save-excursion (funcall starttest start)))))
                  (setq start (next-single-property-change start 'face)))))
    (unless (not start)
      (setq end (next-single-property-change start 'face))
      (unless (not end)
        (cons end (buffer-substring start end))))))

;; simple-call-tree-info:  
(defun* simple-call-tree-get-tags (&optional (lookback -5))
  "Extract TODO state, priority, and tags from lines previous to the current one.
The LOOKBACK argument indicates how many lines backwards to search and should be negative."
  (let ((end (point)) todo priority tags)
    (forward-line lookback)
    (if (re-search-forward
         "simple-call-tree-info:\\s-*\\(\\w+\\)?\\(\\s-*\\[#\\([A-Z]\\)\\]\\)?\\(\\s-*\\(:[a-zA-Z0-9:,;-_]+:\\)\\)?\\s-*$"
         end t)
        (progn
          (aif (match-string 1) (setq todo (substring-no-properties it)))
          (aif (match-string 3) (setq priority (substring-no-properties it)))
          (setq tags (simple-call-tree-string-to-tags (match-string 5)))))
    (goto-char end)
    (list todo priority tags)))

;; simple-call-tree-info:   
(defun* simple-call-tree-set-attribute (attr value
                                             &optional
                                             (func (or (simple-call-tree-get-parent)
                                                       (simple-call-tree-get-function-at-point)))
                                             (updatesrc t))
  "Set the todo, priority, or tags for an item in `simple-call-tree-alist', and update the buffer and source code.
ATTR can be one of: 'todo, 'priority, or 'tags
VALUE is the corresponding value: a string for 'todo or 'priority (a single letter), or a list of strings for 'tags.
FUNC is the name of the function corresponding to the item to be updated.
The *Simple Call Tree* buffer and comments in the source code (just before FUNC) will be updated with the corresponding
information. If UPDATESRC is nil then don't bother updating the source code.
"
  (let* ((item (car (simple-call-tree-get-item func)))
         (marker (second item))
         (buf (marker-buffer marker))
         (end (marker-position marker))
         srcval)
    (case attr
      (todo (setf srcval (concat value " \\2 \\3")
                  (fourth item) value))
      (priority (setf srcval (concat "\\1" (if value (concat " [#" value "]") nil) " \\3")
                      (fifth item) value))
      (tags (setf srcval (concat "\\1 \\2 " (simple-call-tree-tags-to-string value))
                  (sixth item) value)))
    (if updatesrc
        (with-current-buffer buf
          (save-excursion
            (goto-char end)
            (forward-line -5)
            (if (re-search-forward
                 "simple-call-tree-info:\\s-*\\(\\w+\\)?\\s-*\\(\\[#[A-Z]\\]\\)?\\s-*\\(:[a-zA-Z0-9:,;-_]+:\\)?"
                 end t)
                (replace-match (concat "simple-call-tree-info: " srcval))
              (goto-char end)
              (forward-line -1)
              (end-of-line)
              (insert "\nsimple-call-tree-info: " value)
              (setq end (point))
              (beginning-of-line)
              (comment-region (point) end)))))
    (save-excursion
      (goto-char (point-min))
      (read-only-mode -1)
      (if (re-search-forward            ;don't be tempted to use `outline-regexp' here!
           (concat "^|\\(\\( \\w+\\)?\\)\\(\\( \\[#.\\]\\)?\\) " func
                   "\\(\\s-*\\(:[a-zA-Z0-9:,;-_]+:\\)?\\)\\s-*$") nil t)
          (progn (show-children)        ;hack! otherwise it doesn't always work properly
                 (kill-line 0)
                 (simple-call-tree-insert-item item 1 nil)))
      (read-only-mode 1))))

;; simple-call-tree-info: TODO  :a:b:
(defun* simple-call-tree-set-todo (value &optional
                                         (func (or (simple-call-tree-get-parent)
                                                   (simple-call-tree-get-function-at-point))))
  (interactive (list (org-icompleting-read
                      "State: " (simple-call-tree-org-todo-keywords)
                      nil t)))
  (simple-call-tree-set-attribute 'todo value func t))

(defun simple-call-tree-next-todo nil
  "Move to next todo state for current function"
  (interactive)
  (let* ((func (or (simple-call-tree-get-parent)
                   (simple-call-tree-get-function-at-point)))
         (curtodo (fourth (car (simple-call-tree-get-item func))))
         (states (simple-call-tree-org-todo-keywords))
         (pos (position curtodo states :test 'equal)))
    (simple-call-tree-set-attribute
     'todo
     (nth (if pos (mod (1+ pos) (length states)) 0) states)
     func t)))

(defun simple-call-tree-prev-todo nil
  "Move to previous todo state for current function"
  (interactive)
  (let* ((func (or (simple-call-tree-get-parent)
                   (simple-call-tree-get-function-at-point)))
         (curtodo (fourth (car (simple-call-tree-get-item func))))
         (states (simple-call-tree-org-todo-keywords))
         (pos (position curtodo states)))
    (simple-call-tree-set-attribute
     'todo
     (nth (if pos (mod (1- pos) (length states)) 0) states)
     func t)))

(defun simple-call-tree-up-priority nil
  "Change current function to the next priority level."
  (interactive)
  (let* ((func (or (simple-call-tree-get-parent)
                   (simple-call-tree-get-function-at-point)))
         (curpriority (aand (fifth (car (simple-call-tree-get-item func)))
                            (string-to-char it)))
         (nextpriority (cond ((not curpriority) simple-call-tree-org-lowest-priority)
                             ((and (<= curpriority simple-call-tree-org-lowest-priority)
                                   (> curpriority simple-call-tree-org-highest-priority))
                              (1- curpriority))
                             ((= curpriority simple-call-tree-org-highest-priority) nil))))
    (simple-call-tree-set-attribute 'priority (and nextpriority (char-to-string nextpriority)) func t)))

(defun simple-call-tree-down-priority nil
  "Change current function to the previous priority level."
  (interactive)
  (let* ((func (or (simple-call-tree-get-parent)
                   (simple-call-tree-get-function-at-point)))
         (curpriority (aand (fifth (car (simple-call-tree-get-item func)))
                            (string-to-char it)))
         (nextpriority (cond ((not curpriority) simple-call-tree-org-highest-priority)
                             ((and (< curpriority simple-call-tree-org-lowest-priority)
                                   (>= curpriority simple-call-tree-org-highest-priority))
                              (1+ curpriority))
                             ((= curpriority simple-call-tree-org-lowest-priority) nil))))
    (simple-call-tree-set-attribute 'priority (and nextpriority (char-to-string nextpriority)) func t)))

(defun* simple-call-tree-set-tags (value &optional
                                         (func (or (simple-call-tree-get-parent)
                                                   (simple-call-tree-get-function-at-point))))
  "Set the org tags for the toplevel function at point"
  (interactive (let* ((func (or (simple-call-tree-get-parent)
                                (simple-call-tree-get-function-at-point)))
                      (item (simple-call-tree-get-item func))
                      (currenttags (sixth (car item))))
                 (list (simple-call-tree-string-to-tags
                        (org-fast-tag-selection
                         currenttags nil
                         simple-call-tree-org-tag-alist)))))
  (simple-call-tree-set-attribute 'tags value func t))

;;;###autoload
(defun* simple-call-tree-display-buffer (&optional files)
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
      (setq buffers (loop for file in files collect (find-file file))))
    (if (or (not files) (called-interactively-p))
        (add-to-list 'buffers (current-buffer)))
    (simple-call-tree-build-tree buffers)
    (setq simple-call-tree-jump-ring (make-ring simple-call-tree-jump-ring-max)
          simple-call-tree-jump-ring-index 0)))

;;;###autoload
(defun simple-call-tree-build-tree (&optional buffers)
  "Build the simple-call-tree and display it in the \"*Simple Call Tree*\" buffer.
If BUFFERS is supplied it should be a list of buffer to analyze, otherwise the buffers
listed in `simple-call-tree-buffers' will be used."
  (interactive)
  (setq buffers (or buffers simple-call-tree-buffers))
  (simple-call-tree-analyze buffers)
  (case simple-call-tree-default-sort-method
    (alphabet (simple-call-tree-sort-alphabetically))
    (position (simple-call-tree-sort-positionally))
    (numchild (simple-call-tree-sort-by-face))
    (face (simple-call-tree-sort-by-face)))
  (setq simple-call-tree-inverted nil)
  (simple-call-tree-list-callers-and-functions)
  (setq simple-call-tree-buffers buffers))

;;;###autoload
(defun* simple-call-tree-current-function (func &optional wide)
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
      (if (get-buffer "*Simple Call Tree*")
          (switch-to-buffer "*Simple Call Tree*")
        (simple-call-tree-list-callers-and-functions))
    (simple-call-tree-display-buffer))
  (simple-call-tree-jump-to-function func)
  (if wide (simple-call-tree-toggle-narrowing 1)
    (simple-call-tree-toggle-narrowing -1)))

(defun* simple-call-tree-list-callers-and-functions (&optional (maxdepth simple-call-tree-default-maxdepth)
                                                               (funclist simple-call-tree-alist))
  "List callers and functions in FUNCLIST to depth MAXDEPTH.
By default FUNCLIST is set to `simple-call-tree-alist'."
  (switch-to-buffer (get-buffer-create "*Simple Call Tree*"))
  (if (not (equal major-mode 'simple-call-tree-mode)) (simple-call-tree-mode))
  (setq buffer-read-only nil)
  (erase-buffer)
  (let ((maxdepth (max maxdepth 1)))
    (dolist (item funclist)
      (simple-call-tree-list-callees-recursively
       (car item)
       maxdepth 1 funclist))
    (setq simple-call-tree-current-maxdepth (max maxdepth 1)
          buffer-read-only t)))

(defun* simple-call-tree-export-org-tree nil
  "Create an org-tree from the current call tree and put it in an org buffer.
The style of links used for child headers is controlled by `simple-call-tree-org-link-style'."
  (interactive)
  (let ((buf (generate-new-buffer "simple-call-tree.org")))
    (switch-to-buffer buf)
    (setq buffer-read-only nil)
    (erase-buffer)
    (org-mode)
    (dolist (item simple-call-tree-alist)
      (simple-call-tree-list-callees-recursively
       (car item)
       simple-call-tree-current-maxdepth
       1
       simple-call-tree-alist
       simple-call-tree-inverted
       'simple-call-tree-insert-org-header))
    (org-update-radio-target-regexp)))

(defun* simple-call-tree-list-callees-recursively (item &optional (maxdepth 2)
                                                        (curdepth 1)
                                                        (funclist simple-call-tree-alist)
                                                        (inverted simple-call-tree-inverted)
                                                        (displayfunc 'simple-call-tree-insert-item))
  "Insert a call tree for the function named FNAME, to depth MAXDEPTH.
FNAME must be the car of one of the elements of FUNCLIST which is set to `simple-call-tree-alist' by default.
The optional arguments MAXDEPTH and CURDEPTH specify the maximum and current depth of the tree respectively.
This is a recursive function, and you should not need to set CURDEPTH."
  (let* ((fname (first item))
         (callees (cdr (simple-call-tree-get-item fname)))
         done)
    (funcall displayfunc item curdepth inverted)
    (insert "\n")
    (if (< curdepth maxdepth)
        (dolist (callee callees)
          (unless (and simple-call-tree-nodups (member (car callee) done))
            (simple-call-tree-list-callees-recursively callee maxdepth (1+ curdepth) funclist inverted displayfunc))
          (add-to-list 'done (car callee))))))

;; Propertize todo, priority & tags appropriately
;; simple-call-tree-info: DELEGATED  
(defun simple-call-tree-insert-item (item curdepth inverted)
  "Display ITEM at depth CURDEPTH in the call tree."
  (let* ((fname (first item))
         (pos (second item))
         (todo (fourth item))
         (priority (fifth item))
         (tags (simple-call-tree-tags-to-string (sixth item)))
         (pre (concat (if todo (concat " " todo))
                      (if priority (concat " [#" priority "]"))))
         (arrowtail (make-string (* 2 (1- curdepth)) 45))
         (arrow (if inverted (concat (if (> curdepth 1) "<" pre) arrowtail " ")
                  (concat arrowtail (if (> curdepth 1) ">" pre) " ")))
         (face (get-text-property 0 'face fname))
         (pre2 (concat "|" arrow
                       (propertize fname
                                   'font-lock-face (list :inherit face :underline t)
                                   'mouse-face 'highlight
                                   'location pos)))
         (post (concat (make-string (max 0 (- (/ (window-width) 2) (length pre2))) 32)
                       tags)))
    (insert pre2 post)))

(defun simple-call-tree-insert-org-header (item curdepth inverted)
  "Display ITEM at depth CURDEPTH in the call tree."
  (let* ((fname (first item))
         (marker (second item))
         (stars (make-string curdepth 42)))
    (if (and (> curdepth 1)
             (eq simple-call-tree-org-link-style 'radio))
        (insert stars " " fname "\n")
      (with-current-buffer (marker-buffer marker)
        (save-excursion
          (goto-char (marker-position marker))
          (call-interactively 'org-store-link)))
      (insert stars " [[" (substring-no-properties (caar org-stored-links)) "][" fname "]]")
      (if (eq simple-call-tree-org-link-style 'radio)
          (insert "\n<<<" fname ">>>"))
      (setq org-stored-links (cdr org-stored-links)))))

(defun simple-call-tree-outline-level nil
  "Return the outline level of the function at point.

Note: this may not give correct results if the current headline was reached by moving
the cursor manually since it depends on the match string from a previous outline movement
command. To ensure correct results do a search for `outline-regexp' from the beginning of
the line first in order to capture the match-string.
We can't do that in this function as it causes other problems with outline mode commands."
  (with-current-buffer "*Simple Call Tree*"
    (save-excursion
      (let ((arrowlen (length (match-string 1))))
        (if (= arrowlen 0) 1 (1+ (/ (1- arrowlen) 2)))))))

(defun simple-call-tree-get-parent nil
  "Return the name of the parent of the function at point according to the current tree.
If there is no parent, return nil."
  (with-current-buffer "*Simple Call Tree*"
    (save-excursion
      (if (condition-case nil
              (simple-call-tree-move-up)
            (error nil))
          (simple-call-tree-get-function-at-point)))))

(defun simple-call-tree-get-toplevel nil
  "Return the name of the toplevel parent of the subtree at point."
  (with-current-buffer "*Simple Call Tree*"
    (save-excursion
      (move-beginning-of-line nil)
      (re-search-forward outline-regexp)
      (let ((level (simple-call-tree-outline-level)))
        (if (condition-case nil
                (outline-up-heading (1- level))
              (error nil))
            (simple-call-tree-get-function-at-point))))))

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

(defun simple-call-tree-sort (predicate)
  "Sort the branches and sub-branches of `simple-call-tree-alist' and `simple-call-tree-inverted-alist' by predicate."
  (dolist (branch simple-call-tree-alist)
    (setcdr branch (sort (cdr branch) predicate)))
  (setq simple-call-tree-alist
        (sort simple-call-tree-alist
              (lambda (a b)
                (funcall predicate (car a) (car b)))))
  (dolist (branch simple-call-tree-inverted-alist)
    (setcdr branch (sort (cdr branch) predicate)))
  (setq simple-call-tree-inverted-alist
        (sort simple-call-tree-inverted-alist
              (lambda (a b)
                (funcall predicate (car a) (car b))))))

(defun simple-call-tree-reverse nil
  "Reverse the order of the branches & sub-branches in `simple-call-tree-alist' and `simple-call-tree-inverted-alist'."
  (interactive)
  (dolist (branch simple-call-tree-alist)
    (setcdr branch (reverse (cdr branch))))
  (setq simple-call-tree-alist (reverse simple-call-tree-alist))
  (dolist (branch simple-call-tree-inverted-alist)
    (setcdr branch (reverse (cdr branch))))
  (setq simple-call-tree-inverted-alist (reverse simple-call-tree-inverted-alist))
  (simple-call-tree-restore-state (simple-call-tree-store-state)))

(defun simple-call-tree-sort-by-num-children nil
  "Sort the branches in the *Simple Call Tree* buffer by the number of children."
  (interactive)
  (setq simple-call-tree-alist
        (sort simple-call-tree-alist
              (lambda (a b) (> (length a) (length b)))))
  (setq simple-call-tree-inverted-alist
        (sort simple-call-tree-inverted-alist
              (lambda (a b) (> (length a) (length b)))))
  (simple-call-tree-restore-state (simple-call-tree-store-state))
  (setq simple-call-tree-current-sort-order 'numchild))

(defun simple-call-tree-sort-alphabetically nil
  "Sort the functions in the *Simple Call Tree* buffer alphabetically.
The toplevel functions will be sorted, and the functions in each branch will be sorted separately."
  (interactive)
  (simple-call-tree-sort (lambda (a b) (string< (car a) (car b))))
  (simple-call-tree-restore-state (simple-call-tree-store-state))
  (setq simple-call-tree-current-sort-order 'alphabet))

(defun simple-call-tree-sort-positionally nil
  "Sort the functions in the *Simple Call Tree* buffer by position.
The toplevel functions will be sorted, and the functions in each branch will be sorted separately."
  (interactive)
  (simple-call-tree-sort (lambda (a b)
                           (or (string< (buffer-name (marker-buffer (second a)))
                                        (buffer-name (marker-buffer (second b))))
                               (< (marker-position (second a))
                                  (marker-position (second b))))))
  (simple-call-tree-restore-state (simple-call-tree-store-state))
  (setq simple-call-tree-current-sort-order 'position))

(defun simple-call-tree-sort-by-face nil
  "Sort the items in the *Simple Call Tree* buffer according to the display face name.
This should sort the items by type (e.g. function, variable, etc.) since different types are generally displayed
with different faces.
The toplevel functions will be sorted, and the functions in each branch will be sorted separately."
  (interactive)
  (simple-call-tree-sort (lambda (a b) (string< (symbol-name (get-text-property 0 'face (car a)))
                                                (symbol-name (get-text-property 0 'face (car b))))))
  (simple-call-tree-restore-state (simple-call-tree-store-state))
  (setq simple-call-tree-current-sort-order 'face))

(defun simple-call-tree-store-state nil
  "Store the current state of the displayed call tree, and return as an alist."
  (move-beginning-of-line nil)
  (or (re-search-forward outline-regexp nil t)
      (progn (simple-call-tree-move-prev)
             (re-search-forward outline-regexp nil t)))
  (list 'narrowed (if (get-buffer "*Simple Call Tree*")
                      (simple-call-tree-buffer-narrowed-p))
        'depth simple-call-tree-current-maxdepth
        'level (if (get-buffer "*Simple Call Tree*")
                   (simple-call-tree-outline-level))
        'tree (if simple-call-tree-inverted
                  simple-call-tree-inverted-alist
                simple-call-tree-alist)
        'thisfunc (if (get-buffer "*Simple Call Tree*")
                      (simple-call-tree-get-function-at-point))
        'topfunc (if (get-buffer "*Simple Call Tree*")
                     (simple-call-tree-get-toplevel))
        'nodups simple-call-tree-nodups))

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

(defun simple-call-tree-quit nil
  "Quit the *Simple Call Tree* buffer."
  (interactive)
  (let ((win (get-buffer-window "*Simple Call Tree*")))
    (if win (with-selected-window win
              (unless (not (featurep 'fm))
                (if fm-working (fm-toggle))
                (fm-unhighlight 0)
                (fm-unhighlight 1))
              (if (> (length (window-list)) 1)
                  (delete-window)
                (bury-buffer))))))

(defun simple-call-tree-invert-buffer nil
  "Invert the tree in *Simple Call Tree* buffer."
  (interactive)
  (setq simple-call-tree-inverted
        (not simple-call-tree-inverted))
  (simple-call-tree-restore-state (simple-call-tree-store-state)))

(defun simple-call-tree-change-maxdepth (maxdepth)
  "Alter the maximum tree depth in the *Simple Call Tree* buffer."
  (interactive "P")
  (setq simple-call-tree-current-maxdepth
        (if current-prefix-arg (prefix-numeric-value current-prefix-arg)
          (floor (abs (read-number "Maximum depth to display: " 2)))))
  (simple-call-tree-restore-state (simple-call-tree-store-state)))

(defun simple-call-tree-view-function nil
  "Display the source code corresponding to current header.
If the current header is a calling or toplevel function then display that function.
If it is a called function then display the position in the calling function where it is called."
  (interactive)
  (move-beginning-of-line nil)
  (re-search-forward outline-regexp)
  (let* ((level (simple-call-tree-outline-level))
         (funmark (get-text-property (point) 'location))
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

(defun* simple-call-tree-visit-function (&optional arg)
  "Visit the source code corresponding to the current header.
If the current header is a calling or toplevel function then visit that function.
If it is a called function then visit the position in the calling function where it is called.
If ARG is non-nil, or a prefix arg is supplied when called interactively then narrow
the source buffer to the function."
  (interactive "P")
  (move-beginning-of-line nil)
  (re-search-forward outline-regexp)
  (let* ((level (simple-call-tree-outline-level))
         (funmark (get-text-property (point) 'location))
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

(defun* simple-call-tree-jump-to-function (fnstr &optional skipring)
  "Move cursor to the line corresponding to the function with name FNSTR.
When called interactively FNSTR will be set to the function name under point,
or if called with a prefix arg it will be prompted for.
Unless optional arg SKIPRING is non-nil (which will be true if called with a negative
prefix arg) then the function name will be added to `simple-call-tree-jump-ring'"
  (interactive (list (if current-prefix-arg
                         (if (and (featurep 'ido)
                                  (not (null ido-mode)))
                             (ido-completing-read "Jump to function: " (mapcar 'caar simple-call-tree-alist))
                           (completing-read "Jump to function: " (mapcar 'caar simple-call-tree-alist)))
                       (simple-call-tree-get-function-at-point))
                     (< (prefix-numeric-value current-prefix-arg) 0)))
  (let* ((narrowedp (simple-call-tree-buffer-narrowed-p)))
    (widen)
    (with-current-buffer "*Simple Call Tree*"
      (goto-char (point-min))
      (re-search-forward (concat "^|\\( \\w+\\)?\\( \\[#.\\]\\)? " (regexp-opt (list fnstr)) "\\s-*$"))
      (unless skipring (simple-call-tree-jump-ring-add fnstr))
      (if narrowedp (simple-call-tree-toggle-narrowing)
        (case simple-call-tree-default-recenter
          (top (recenter 0))
          (middle (recenter))
          (bottom (recenter -1))
          (t (recenter arg)))))))

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

(defun simple-call-tree-move-up nil
  "Move cursor to the parent of this function."
  (interactive)
  (with-current-buffer "*Simple Call Tree*"
    (move-beginning-of-line nil)
    (or (re-search-forward outline-regexp nil t)
        (progn (simple-call-tree-move-prev)
               (re-search-forward outline-regexp nil t)))
    (unless (= (simple-call-tree-outline-level) 1)
      (outline-up-heading 1)
      (let ((nextpos (next-single-property-change (point) 'face)))
        (if nextpos (goto-char nextpos))))))

(defun simple-call-tree-move-next nil
  "Move cursor to the next function."
  (interactive)
  (outline-next-visible-heading 1)
  (let ((nextpos (next-single-property-change (point) 'face)))
    (if nextpos (goto-char nextpos))))

(defun simple-call-tree-move-prev nil
  "Move cursor to the previous function."
  (interactive)
  (outline-previous-visible-heading 1)
  (goto-char (next-single-property-change (point) 'face)))

(defun simple-call-tree-move-next-samelevel nil
  "Move cursor to the next function at the same level as the current one."
  (interactive)
  (outline-forward-same-level 1)
  (goto-char (next-single-property-change (point) 'face)))

(defun simple-call-tree-move-prev-samelevel nil
  "Move cursor to the previous function at the same level as the current one."  
  (interactive)
  (outline-backward-same-level 1)
  (goto-char (next-single-property-change (point) 'face)))

(defun simple-call-tree-buffer-narrowed-p nil
  "Return non-nil if *Simple Call Tree* buffer is narrowed."
  (with-current-buffer "*Simple Call Tree*"
    (or (/= (point-min) 1)
        (/= (point-max) (1+ (buffer-size))))))

(defun simple-call-tree-toggle-narrowing (&optional state)
  "Toggle narrowing of *Simple Call Tree* buffer.
If optional arg STATE is > 0, or if called interactively with a positive prefix arg,
then widen the buffer. If STATE is < 0 or if called interactively with a negative
prefix arg then narrow the buffer.
Otherwise toggle the buffer between narrow and wide state.
When narrowed, the buffer will be narrowed to the subtree at point."
  (interactive "P")
  (with-current-buffer "*Simple Call Tree*"
    (let ((pos (point)))
      (if (or (and state (> (prefix-numeric-value state) 0))
              (and (not state) (simple-call-tree-buffer-narrowed-p)))
          (widen)
        (if (looking-at "^| ")
            (re-search-forward "^| ")
          (re-search-backward "^| "))
        (outline-mark-subtree)
        (narrow-to-region (region-beginning) (region-end))
        (let (select-active-regions) (deactivate-mark)))
      (goto-char pos))))

(defun simple-call-tree-toggle-duplicates nil
  "Toggle the inclusion of duplicate sub-branches in the call tree."
  (interactive)
  (setq simple-call-tree-nodups (not simple-call-tree-nodups))
  (with-current-buffer "*Simple Call Tree*"
    (simple-call-tree-restore-state (simple-call-tree-store-state))))

(defun* simple-call-tree-apply-command (cmd &optional (func (simple-call-tree-get-function-at-point)))
  "Apply command CMD on function FUNC.
By default FUNC is set to the function at point in the *Simple Call Tree* buffer.
The command CMD will be called interactively after switching to the source code buffer,
and narrowing the buffer around FUNC."
  (interactive (list (read-command "Command: ")
                     (simple-call-tree-get-function-at-point)))
  (let ((buf (marker-buffer
              (second (car (simple-call-tree-get-item func))))))
    (switch-to-buffer-other-window buf)
    (simple-call-tree-narrow-to-function func)
    (call-interactively cmd)
    (widen)
    (switch-to-buffer-other-window "*Simple Call Tree*")))

(defun simple-call-tree-query-replace nil
  "Perform query-replace on the function at point in the *Simple Call Tree* buffer.
This just calls `simple-call-tree-apply-command' with the `query-replace' command."
  (interactive)
  (simple-call-tree-apply-command 'query-replace))

(defun simple-call-tree-query-replace-regexp nil
  "Perform `query-replace-regexp' on the function at point in the *Simple Call Tree* buffer.
This just calls `simple-call-tree-apply-command' with the `query-replace-regexp' command."
  (interactive)
  (simple-call-tree-apply-command 'query-replace-regexp))

(defun simple-call-tree-eval-defun (&optional arg)
  "Evaluate the function/variable/struct corresponding to the branch at point.
With a prefix arg, and if the form is a function, instrument it for debugging with edebug."
  (interactive)
  (if arg (simple-call-tree-apply-command 'edebug-defun)
    (simple-call-tree-apply-command 'eval-defun)))

(defun simple-call-tree-bookmark nil
  "Set a bookmark at the position corresponding to the branch at point."
  (interactive)
  (simple-call-tree-apply-command 'bookmark-set))

(defun simple-call-tree-delete-other-windows nil
  "Make the *Simple Call Tree* buffer fill the frame."
  (interactive)
  (unless (not (featurep 'fm))
    (fm-unhighlight 0)
    (fm-unhighlight 1)
    (setq fm-working nil))
  (delete-other-windows))

(unless (not (featurep 'fm))
  (add-to-list 'fm-modes '(simple-call-tree-mode simple-call-tree-visit-function))
  (add-hook 'simple-call-tree-mode-hook 'fm-start))

(provide 'simple-call-tree+)

;;; simple-call-tree+.el ends here

;; (magit-push)
;; (yaoddmuse-post "EmacsWiki" "simple-call-tree+.el" (buffer-name) (buffer-string) "update")

