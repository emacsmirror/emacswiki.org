;;; imenu+.el --- Extensions to `imenu.el'.
;;
;; Filename: imenu+.el
;; Description: Extensions to `imenu.el'.
;; Author: Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 1999-2017, Drew Adams, all rights reserved.
;; Created: Thu Aug 26 16:05:01 1999
;; Version: 0
;; Package-Requires: ()
;; Last-Updated: Fri Mar  3 14:57:23 2017 (-0800)
;;           By: dradams
;;     Update #: 1045
;; URL: https://www.emacswiki.org/emacs/download/imenu%2b.el
;; Doc URL: http://emacswiki.org/ImenuMode
;; Keywords: tools, menus
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x, 24.x, 25.x
;;
;; Features that might be required by this library:
;;
;;   `imenu'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Extensions to `imenu.el'.
;;
;;   User options defined here:
;;
;;    `imenup-ignore-comments-flag' (Emacs 22+),
;;    `imenup-sort-ignores-case-flag'.
;;
;;   Commands defined here:
;;
;;    `imenup-add-defs-to-menubar',
;;    `imenup-toggle-case-sensitive-sorting',
;;    `imenup-toggle-ignoring-comments' (Emacs 22+),
;;    `imenup-toggle-sort',
;;
;;   Non-interactive functions defined here:
;;
;;    `imenup--sort-submenu', `imenup-invisible-p'.
;;
;;   Internal variables defined here:
;;
;;    `imenup-emacs-face-defn-regexp',
;;    `imenup-emacs-key-defn-regexp-1',
;;    `imenup-emacs-key-defn-regexp-2',
;;    `imenup-emacs-lisp-generic-expression',
;;    `imenup-emacs-option-defn-regexp', `imenup-last-sort-function',
;;    `imenup-lisp-fn-defn-regexp-1', `imenup-lisp-fn-defn-regexp-2',
;;    `imenup-lisp-macro-defn-regexp',
;;    `imenup-lisp-other-defn-regexp-1',
;;    `imenup-lisp-other-defn-regexp-2',
;;    `imenup-lisp-var-defn-regexp',
;;
;;
;;  ***** NOTE: The following functions and macro defined in `imenu.el'
;;              have been REDEFINED HERE:
;;
;;    `imenu--generic-function', `imenu--make-index-alist',
;;    `imenu--mouse-menu', `imenu--sort-by-name',
;;    `imenu--split-submenus', `imenu-progress-message',
;;    `imenu-update-menubar' (Emacs <22).
;;
;;
;;  ***** NOTE: The following variable defined in `imenu.el' has
;;              been REDEFINED HERE:
;;
;;  `imenu-sort-function'.
;;
;;  ***** NOTE: The following variable defined in `lisp-mode.el' has
;;              been REDEFINED HERE:
;;
;;  `lisp-imenu-generic-expression'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2014/12/23 dadams
;;     lisp-imenu-generic-expression, imenup-emacs-lisp-generic-expression:
;;       Use distinct submenu names, so no ambiguity: Others 2, Functions 2.
;; 2014/06/21 dadams
;;     *-lisp-other-defn-regexp-[12], *-lisp-fn-defn-regexp-[12], *-lisp-var-defn-regexp:
;;       Allow single-char names: \(\(\sw\|\s_\)+\), not \(\sw\(\sw\|\s_\)+\).
;; 2013/12/08 dadams
;;     Added: imenup-lisp-fn-defn-regexp-2.
;;     Renamed imenup-lisp-fn-defn-regexp to imenup-lisp-fn-defn-regexp-1.
;;     lisp-imenu-generic-expression, imenup-emacs-lisp-generic-expression:
;;       Use imenup-lisp-fn-defn-regexp-2 also.
;; 2013/10/18 dadams
;;     lisp-imenu-generic-expression, imenup-emacs-lisp-generic-expression:
;;       Prevent creating an Other submenu with "" as the regexp.
;; 2013/10/11 dadams
;;     imenu--make-index-alist: Fix for Emacs < 22 (no imenup-ignore-comments-flag).
;; 2013/10/08 dadams
;;     Added: imenup-ignore-comments-flag, imenup-toggle-ignoring-comments.
;;     Renamed: imenup-sort-ignores-case to imenup-sort-ignores-case-flag.
;;     Do not require hide-comnt.el.
;;     imenu--make-index-alist: Added menu item for toggling ignoring commented defs.
;;                              Do not use with-comments-hidden etc.  Now in imenu--generic-function.
;;     imenu--generic-function: Respect imenup-ignore-comments-flag.
;; 2013/08/13 dadams
;;     Added: imenup-lisp-other-defn-regexp-2.
;;     Renamed: imenup-lisp-other-defn-regexp to imenup-lisp-other-defn-regexp-1.
;;     imenup(-emacs)-lisp-generic-expression: Use imenup-lisp-other-defn-regexp-1 and *-2.
;;     imenup-lisp-other-defn-regexp-1: Added cl-deftype, cl-defstruct.
;;     imenup-lisp-fn-defn-regexp: Added cl-defun, defsubst.
;;     imenup-lisp-macro-defn-regexp: Added cl-defmacro, cl-define-compiler-macro.
;; 2013/07/25 dadams
;;     Require hide-comnt.el for Emacs 20 also - see comment in code for require.
;; 2013/07/24 dadams
;;     Require hide-comnt.el for Emacs 21+.
;; 2012/10/24 dadams
;;     imenup-toggle-case-sensitive-sorting: Improve message in case not currently sorting by name.
;; 2012/10/23 dadams
;;     Added defgroup.  Added: imenup-toggle-case-sensitive-sorting.
;;     Initialize imenup-last-sort-function to imenu--sort-by-name, not nil.
;;     Added redefinition of imenu--sort-by-name.
;;     Added redefinition of imenu--split-submenus: Fixes Emacs bug #12717.
;;     imenu--make-index-alist:
;;       Added toggle commands to menu.  Removed eval-and-compile - use now for Emacs 20 too.
;;       Updated for Emacs 24 (use user-error, not error).
;;     Renamed all Imenu+ stuff to use prefix imenup-.
;;     imenup-toggle-sort: Simplified, and arg is optional now.
;;     imenup--sort-submenu: Use imenu--subalist-p to correctly test a submenu.
;;     Require cl.el at compile time for all versions (use case macro now).
;;     Removed defaliases defining toggle-imenu*.
;; 2012/03/15 dadams
;;     imenu-update-menubar: Applied Emacs 24 bug fix to handle a dynamically composed keymap.
;;     Require cl.el for Emacs 20 when byte-compile.
;; 2012/01/01 dadams
;;     imenu-update-menubar: buffer-modified-tick -> buffer-chars-modified-tick.  (Sync w/ vanilla.)
;; 2011/11/24 dadams
;;     Added: imenup-invisible-p.
;;     imenu--generic-function: Use imenup-invisible-p, not just get-text-property (so overlays too).
;; 2011/11/23 dadams
;;     Make menu ignore invisible text and respect ignore-comments-flag.  Added (redefinition of):
;;       imenu--make-index-alist, imenu--generic-function, imenu-progress-message.
;; 2001/08/26 dadams
;;     imenu--sort-submenu: Copy MENU-ITEMS so sort doesn't modify it.  Thx  to Michael Heerdegen.
;; 2011/05/27 dadams
;;     imenu-lisp-var-defn-regexp:
;;       Corrected to allow \n after var name (\n is comment-end syntax, not whitespace, in Lisp).
;; 2011/05/08 dadams
;;     imenu-lisp-var-defn-regexp: Try not to create entries for vacuous defvars, e.g., (defvar foo).
;; 2011/03/18 dadams
;;     imenu-emacs-key-defn-regexp-[1|2]: Handle (kbd "...").
;;     emacs-lisp-imenu-generic-expression: Increased index from 4 to 5, to fit change for kbd.
;; 2011/01/04 dadams
;;     Removed autoload cookies from defvar, non-interactive fns.  Added for command.
;; 2007/01/16 dadams
;;     imenu-lisp-fn-defn-regexp: Updated for icicle-define-add-to-alist-command.
;; 2005/12/09 dadams
;;     imenu-lisp-fn-defn-regexp: Updated to include icicle-define*.
;;       Use regexp-opt for Emacs 20 version too.
;;       Moved Emacs 22 macro stuff to imenu-lisp-macro-defn-regexp.
;;     (emacs-)lisp-imenu-generic-expression:
;;       Updated Emacs 20 index to accomodate parens for icicle-define*.
;;     Added: imenu-emacs-(face|option)-defn-regexp,
;;     Removed: imenu-lisp-struct-defn-regexp.
;;     Renamed: imenu-lisp-type-defn-regexp to imenu-lisp-other-defn-regexp.
;; 2005/05/17 dadams
;;     Updated to work with Emacs 22.x.
;; 2004/11/21 dadams
;;     imenu-lisp-type-defn-regexp, imenu-lisp-fn-defn-regexp,
;;     imenu-lisp-var-defn-regexp: Got rid of purecopy & eval-when-compile.
;; 2004/11/20 dadams
;;     Refined to deal with Emacs 21 < 21.3.50 (soon to be 22.x)
;; 2004/10/12 dadams
;;     Updated for Emacs 21.
;; 2001/01/05 dadams
;;     Unquoted mapcar lambda args.
;; 2000/11/01 dadams
;;     Put imenu-add-defs-to-menubar inside condition-case, in (*-)lisp-mode-hooks
;; 1999/08/30 dadams
;;     1. imenu-emacs-key-defn-regexp-2: Added define-key-after.
;;     2. Updated emacs-lisp-imenu-generic-expression (Keys in Maps).
;; 1999/08/27 dadams
;;     1. Corrected: imenu-lisp-fn-defn-regexp, imenu-lisp-macro-defn-regexp,
;;                   imenu-lisp-var-defn-regexp, imenu--sort-submenu,
;;                   imenu-emacs-key-defn-regexp-2.
;;     2. Added: imenu--sort-submenu, imenu-update-menubar, imenu--mouse-menu.
;;        Redefinition of originals: imenu-update-menubar, imenu--mouse-menu.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(eval-when-compile (require 'cl)) ;; case

(require 'imenu)

;; Quiet the byte-compiler
(defvar imenu-menubar-modified-tick)
(defvar imenup-ignore-comments-flag)    ; Here (Emacs 22+).

;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defgroup Imenu-Plus nil
  "Various enhancements to Imenu."
  :prefix "imenup-" :group 'imenu
  :link `(url-link :tag "Send Bug Report"
          ,(concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=\
imenu+.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and library versions."))
  :link '(url-link :tag "Other Libraries by Drew"
          "http://www.emacswiki.org/DrewsElispLibraries")
  :link '(url-link :tag "Download" "http://www.emacswiki.org/imenu+.el")
  :link '(url-link :tag "Description" "http://www.emacswiki.org/ImenuMode#ImenuPlus")
  :link '(emacs-commentary-link :tag "Commentary" "imenu+"))

(when (fboundp 'syntax-ppss)            ; Emacs 22+.
  (defcustom imenup-ignore-comments-flag t
    "Non-nil means Imenu ignores comments."
    :type 'boolean :group 'Imenu-Plus))

;;;###autoload
(defcustom imenup-sort-ignores-case-flag nil
  "*Non-nil means that `imenu--sort-by-name' sorts case-insensitively."
  :type 'boolean :group 'Imenu-Plus)

;; Apologies, but this is the easiest way to override the default value of nil.
;; If you need to override this, comment it out or set it after loading this file.
(defconst imenu-sort-function 'imenu--sort-by-name)

(defvar imenup-last-sort-function `imenu--sort-by-name
  "The last non-nil value for `imenu-sort-function' during this session.")

;; For key definitions, we need to handle: strings and vectors, but also (kbd STRING).
;; We just match optional `(kbd ' followed by a string or a vector'.
(defvar imenup-emacs-key-defn-regexp-1 "(\\s-*\\(\\(global\\|local\\)-\\(un\\)?\
set-key\\|undefine-keys-bound-to\\)\\s-*\\((kbd\\s-*\\)?\\(\"[^\"]+\"\\|[[][^]]+[]]\\)"
  "*Regexp that recognizes Emacs key definitions.
See also `imenup-emacs-key-defn-regexp-2'.")

(defvar imenup-emacs-key-defn-regexp-2 "(\\s-*\\(define-key\\(-after\\)?\\s-+\
\\|substitute-key-definition\\s-+'\\)\\(\\S-+\\)\\s-*'?\\((kbd\\s-*\\)?\\(\"[^\"]+\"\\|[[][^]]+[]]\\)"
  "*Regexp that recognizes Emacs key definitions.
See also `imenup-emacs-key-defn-regexp-1'.")

(defvar imenup-lisp-other-defn-regexp-1
  (if (>= emacs-major-version 22)
      (concat "^\\s-*("
              (regexp-opt '("defgroup" "deftheme" "deftype" "cl-deftype" "defstruct" "cl-defstruct"
                            "defclass" "define-condition" "define-widget"
                            "defpackage")
                          t)
              "\\s-+'?\\(\\(\\sw\\|\\s_\\)+\\)")
    "(\\s-*def\\(type\\|class\\|ine-condition\\)\\s-+'?\\([^ \t()]+\\)")
  "*Regexp that recognizes other Lisp definitions.")

(defvar imenup-lisp-other-defn-regexp-2
  (if (>= emacs-major-version 22)
      (concat "^\\s-*("
              (regexp-opt '("defstruct" "cl-defstruct") t)
              "\\s-+(\\(\\(\\sw\\|\\s_\\)+\\)")
    "")
  "*Regexp that recognizes other Lisp defs, where the name is followed by (.")

(defvar imenup-lisp-fn-defn-regexp-1
  (if (>= emacs-major-version 22)
      (concat "^\\s-*("
              (regexp-opt '("defun" "cl-defun" "defun*" "defsubst" "cl-defsubst" "defadvice"
                            "define-skeleton" "define-minor-mode"
                            "define-global-minor-mode" "define-globalized-minor-mode"
                            "define-derived-mode" "define-generic-mode" "defsetf"
                            "define-setf-expander" "define-method-combination"
                            "defgeneric" "defmethod" "icicle-define-command"
                            "icicle-define-file-command")
                          t)
              "\\s-+\\(\\(\\sw\\|\\s_\\)+\\)")
    (concat "^\\s-*("
            (regexp-opt
             '("defun" "defun*" "defsubst" "defadvice" "define-skeleton"
               "define-derived-mode" "defsetf" "icicle-define-add-to-alist-command"
               "icicle-define-command" "icicle-define-file-command")
             t)
            "\\s-+\\(\\(\\sw\\|\\s_\\)+\\)"))
  "*Regexp that recognizes Lisp function definitions.")

(defvar imenup-lisp-fn-defn-regexp-2
  (concat "^\\s-*("
          (regexp-opt '("defalias" "fset") t)
          "\\s-+'\\s-*\\(\\(\\sw\\|\\s_\\)+\\)")
  "*Regexp that recognizes Lisp function definitions with a quoted name.")

(defvar imenup-lisp-macro-defn-regexp
  "(\\s-*\\(defmacro\\|cl-defmacro\\|cl-define-compiler-macro\\|define-compiler-macro\\|\
define-modify-macro\\)\\s-+\\([^ \t()]+\\)"
  "*Regexp that recognizes Lisp macro definitions.")

(defvar imenup-emacs-face-defn-regexp "(\\s-*\\(defface\\)\\s-+\\([^ \t()]+\\)"
  "*Regexp for Emacs face definitions (defface).")

(defvar imenup-emacs-option-defn-regexp "(\\s-*\\(defcustom\\)\\s-+\\([^ \t()]+\\)"
  "*Regexp for Emacs user option definitions (defcustom).")

(defvar imenup-lisp-var-defn-regexp
  (if (>= emacs-major-version 22)
      (concat "^\\s-*("
              (regexp-opt '("defvar" "defconst" "defconstant" "defcustom"
                            "defparameter" "define-symbol-macro")
                          t)
              "\\s-+\\(\\(\\sw\\|\\s_\\)+\\)"
              ;; Because \n has char syntax `>', not whitespace.  See Emacs bug #8638.
              "\\(\\s-\\|[\n]\\)+"
              "[^) \t\n]")
    "(\\s-*def\\(var\\|const\\)\\s-+\\([^ \t()]+\\)")
  "*Regexp that recognizes global Lisp variable definitions.")


;; REPLACE ORIGINAL in `lisp-mode.el'.
;;
;; Add `Functions', `Macros', `Structures'.
;;
(defconst lisp-imenu-generic-expression
    (delq nil (list
               (list "Other"        imenup-lisp-other-defn-regexp-1 2)
               (and (not (string= "" imenup-lisp-other-defn-regexp-2))
                    (list "Other 2" imenup-lisp-other-defn-regexp-2 2))
               (list "Macros"       imenup-lisp-macro-defn-regexp 2)
               (list "Functions"    imenup-lisp-fn-defn-regexp-1 (if (string-match "\\(?:\\)" "") 2 6))
               (list "Functions 2"  imenup-lisp-fn-defn-regexp-2 2)
               (list "Variables"    imenup-lisp-var-defn-regexp 2)
               ))
  "*Imenu generic expression for Lisp mode.
See `imenu-generic-expression'.")

(defvar imenup-emacs-lisp-generic-expression
  (delq nil (list
             (list "Other"        imenup-lisp-other-defn-regexp-1 2)
             (and (not (string= "" imenup-lisp-other-defn-regexp-2))
                  (list "Other 2" imenup-lisp-other-defn-regexp-2 2))
             (list "Keys in Maps" imenup-emacs-key-defn-regexp-2 5)
             (list "Keys"         imenup-emacs-key-defn-regexp-1 5)
             (list "Macros"       imenup-lisp-macro-defn-regexp 2)
             (list "Functions"    imenup-lisp-fn-defn-regexp-1 (if (string-match "\\(?:\\)" "") 2 6))
             (list "Functions 2"  imenup-lisp-fn-defn-regexp-2 2)
             (list "Variables"    imenup-lisp-var-defn-regexp 2)
             (list "User Options" imenup-emacs-option-defn-regexp 2)
             (list "Faces"        imenup-emacs-face-defn-regexp 2)
             ))
  "*Imenu generic expression for Emacs Lisp mode.
See `imenu-generic-expression'.")

(add-hook 'lisp-mode-hook
          (lambda ()
            (setq imenu-generic-expression  lisp-imenu-generic-expression)
            (condition-case nil (imenup-add-defs-to-menubar) (error nil))))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq imenu-generic-expression  imenup-emacs-lisp-generic-expression)
            (condition-case nil (imenup-add-defs-to-menubar) (error nil))))


;;;###autoload
(defun imenup-toggle-sort (&optional arg)
  "Toggle imenu between sorting menus and not.
With a prefix ARG, turn on if ARG is non-negative, off if negative.
See also command `imenup-toggle-case-sensitive-sorting'."
  (interactive "P")
  (if arg
      (setq imenu-sort-function  (and (wholenump (prefix-numeric-value arg))
                                      imenup-last-sort-function))
    (if imenu-sort-function
        (setq imenup-last-sort-function  imenu-sort-function ; Save it.
              imenu-sort-function        nil) ; Don't sort.
      (setq imenu-sort-function  imenup-last-sort-function)))
  (imenu--menubar-select imenu--rescan-item)
  (case imenu-sort-function
    (imenu--sort-by-name (message "Menu items now being sorted by name"))
    ((nil)               (message "Menu items are in buffer order (NOT SORTED)"))
    (otherwise           (message "Menu items now being sorted with `%s'" imenu-sort-function))))

;;;###autoload
(defun imenup-toggle-case-sensitive-sorting ()
  "Toggle option `imenup-sort-ignores-case-flag'.
This affects menu sorting using `imenu--sort-by-name'."
  (interactive)
  (setq imenup-sort-ignores-case-flag  (not imenup-sort-ignores-case-flag))
  (imenu--menubar-select imenu--rescan-item)
  (if (eq 'imenu--sort-by-name imenu-sort-function)
      (message "Sorting menu items by name %s" (if imenup-sort-ignores-case-flag
                                                   "now IGNORES case"
                                                 "is now case SENSITIVE"))
    (message "NOT sorting by name now, but will %s if you do" (if imenup-sort-ignores-case-flag
                                                                  "IGNORE case"
                                                                "RESPECT case"))))

(when (boundp 'imenup-ignore-comments-flag)
  (defun imenup-toggle-ignoring-comments ()
    "Toggle option `imenup-ignore-comments-flag'."
    (interactive)
    (setq imenup-ignore-comments-flag  (not imenup-ignore-comments-flag))
    (imenu--menubar-select imenu--rescan-item)
    (message "Ignoring definitions inside comments is now %s"
             (if imenup-ignore-comments-flag 'ON 'OFF))))


;; REPLACE ORIGINAL in `imenu.el'.
;;
;; Respect `imenup-sort-ignores-case-flag'
;;
;;;###autoload
(defun imenu--sort-by-name (item1 item2)
  "Return non-nil if ITEM1 comes before ITEM2 alphabetically.
The arguments are menu items, which have form (NAME . POSITION).
Their NAMEs are compared.

Comparison is case-sensitive if `imenup-sort-ignores-case-flag' is
non-nil.  You can toggle that option using `\\[imenup-toggle-sort]'."
  (let ((name1  (car item1))
        (name2  (car item2)))
    (when imenup-sort-ignores-case-flag (setq name1  (upcase name1)
                                              name2  (upcase name2)))
    (string-lessp name1 name2)))

;;;###autoload
(defun imenup-add-defs-to-menubar ()
  "Add \"Defs\" imenu entry to menu bar for current local keymap.
See `imenu' for more information."
  (interactive)
  (imenu-add-to-menubar "Defs"))

(defun imenup--sort-submenu (submenu predicate)
  "Create an imenu SUBMENU, sorting with PREDICATE."
  (let ((menu-name   (car submenu))
        (menu-items  (cdr submenu)))
    (cons menu-name (if (imenu--subalist-p submenu)
                        ;; Must copy, because MENU-ITEMS can be part of `imenu--index-alist'.
                        (sort (copy-sequence menu-items) predicate)
                      menu-items))))


;; REPLACE ORIGINAL in `imenu.el'.
;;
;; Correctly handle special menu items (distinguish from submenus).  Fixes Emacs bug #12717.
;;
(defun imenu--split-submenus (alist)
  "Split up each long alist that are nested within ALIST into nested alists.
Return a split and sorted copy of ALIST.  The returned alist DOES
NOT share structure with ALIST."
  (mapcar (lambda (elt)
            (if (imenu--subalist-p elt)
                (imenu--split-menu (cdr elt) (car elt))
              elt))
	  alist))


;; REPLACE ORIGINAL in `imenu.el'.
;;
;; Sort each submenu before splitting submenus, in addition to sorting among submenus.
;;
(defun imenu-update-menubar ()
  "Update the Imenu menu.  Use as `menu-bar-update-hook'."
  (when (and (current-local-map)
             (keymapp (lookup-key (current-local-map) [menu-bar index]))
             (or (not (boundp 'imenu-menubar-modified-tick))
                 (/= (buffer-chars-modified-tick) imenu-menubar-modified-tick))) ; Emacs 22+
    (when (boundp 'imenu-menubar-modified-tick) ; Emacs 22+
      (setq imenu-menubar-modified-tick  (buffer-chars-modified-tick)))
    (let ((index-alist  (imenu--make-index-alist t)))
      ;; Don't bother updating if the index-alist has not changed
      ;; since the last time we did it.
      (unless (equal index-alist imenu--last-menubar-index-alist)
        (let (menu menu1 old)
          (setq imenu--last-menubar-index-alist  index-alist
                index-alist                      (imenu--split-submenus
                                                  (if imenu-sort-function
                                                      (mapcar (lambda (sm)
                                                                (imenup--sort-submenu
                                                                 sm imenu-sort-function))
                                                              index-alist)
                                                    index-alist))
                menu                             (imenu--split-menu index-alist (buffer-name)))
          (if (>= emacs-major-version 22)
              (setq menu1  (imenu--create-keymap (car menu)
                                                 (cdr (if (< 1 (length (cdr menu)))
                                                          menu
                                                        (car (cdr menu))))
                                                 'imenu--menubar-select))
            (setq menu1  (imenu--create-keymap-1 (car menu)
                                                 (if (< 1 (length (cdr menu)))
                                                     (cdr menu)
                                                   (cdr (car (cdr menu))))
                                                 t)))
          (setq old  (lookup-key (current-local-map) [menu-bar index]))
	  ;; Next line was added in vanilla Emacs 24, with the comment.
          ;; This should never happen, but in some odd cases, potentially,
	  ;; lookup-key may return a dynamically composed keymap.
	  (when (keymapp (cadr old)) (setq old  (cadr old)))
          (setcdr old (cdr menu1)))))))


(eval-and-compile
 (when (< emacs-major-version 22)

   ;; REPLACE ORIGINAL in `imenu.el'.
   ;;
   ;; Use Emacs 22+ definition, which is vacuous.  Otherwise, if byte-compile in Emacs < 22 and use
   ;; the byte-compiled file in Emacs 22+, then get runtime error:
   ;; `Error in menu-bar-update-hook: (void-variable imenu-scanning-message)'.
   ;;
   (defmacro imenu-progress-message (prevpos &optional relpos reverse)   )))


;; REPLACE ORIGINAL in `imenu.el'.
;;
;; Add Imenu+ toggle commands to menu.
;;
(defun imenu--make-index-alist (&optional noerror)
  "Create an index alist for the definitions in the current buffer.
Include menu items for Imenu+ toggle commands, plus `*Rescan*'.
This works by using the hook function `imenu-create-index-function'.
Report an error if the list is empty unless NOERROR is supplied and
non-nil.  See `imenu--index-alist' for the format of the index alist."
  (or (and imenu--index-alist
           (or (not imenu-auto-rescan)
               (and imenu-auto-rescan  (> (buffer-size) imenu-auto-rescan-maxout))))
      ;; Get the index; truncate if necessary
      (progn (setq imenu--index-alist
                   (save-excursion (save-restriction (widen) (funcall imenu-create-index-function))))
             (imenu--truncate-items imenu--index-alist)))
  (or imenu--index-alist  noerror
      (if (fboundp 'user-error)
          (user-error "No items suitable for an index found in this buffer")
        (error "No items suitable for an index found in this buffer")))
  (or imenu--index-alist  (setq imenu--index-alist  (list nil)))
  (cons imenu--rescan-item              ; `*Rescan*'.
        (cons '("Toggle Case-Sensitive Name-Sort" IGNORE
                (lambda (&rest _ignore) (imenup-toggle-case-sensitive-sorting)))
              (cons '("Toggle Sorting" IGNORE (lambda (&rest _ignore) (imenup-toggle-sort)))
                    (if (fboundp 'imenup-toggle-ignoring-comments)
                        (cons '("Toggle Ignoring Commented Defs" IGNORE
                                (lambda (&rest _ignore) (imenup-toggle-ignoring-comments)))
                               imenu--index-alist)
                      imenu--index-alist)))))

;; Same as `thgcmd-invisible-p' in `thing-cmds.el', and `icicle-invisible-p' in `icicles-cmd2.el'.
(defun imenup-invisible-p (position)
  "Return non-nil if the character at POSITION is invisible."
  (if (fboundp 'invisible-p)            ; Emacs 22+
      (invisible-p position)
    (let ((prop  (get-char-property position 'invisible))) ; Overlay or text property.
      (if (eq buffer-invisibility-spec t)
          prop
        (or (memq prop buffer-invisibility-spec)  (assq prop buffer-invisibility-spec))))))


;; REPLACE ORIGINAL  in `imenu.el'.
;;
;; Ignore invisible definitions.
;;
(defun imenu--generic-function (patterns)
  "Return an index alist of the current buffer based on PATTERNS.
PATTERNS is an alist with elements that look like this:
 (MENU-TITLE REGEXP INDEX)
or like this:
 (MENU-TITLE REGEXP INDEX FUNCTION ARGUMENTS...)
with zero or more ARGUMENTS.  The former format creates a simple
element in the index alist when it matches; the latter creates a
special element of the form (INDEX-NAME POSITION-MARKER FUNCTION
ARGUMENTS...) with FUNCTION and ARGUMENTS copied from PATTERNS.

MENU-TITLE is a string used as the title for the submenu or nil if the
entries are not nested.

REGEXP is a regexp that should match a construct in the buffer that is
to be displayed in the menu; i.e., function or variable definitions,
etc.  It contains a substring which is the name to appear in the menu.
See the info section on Regexps for more information.  REGEXP may also
be a function, called without arguments.  It is expected to search
backwards.  It shall return true and set `match-data' if it finds
another element.

INDEX points to the substring in REGEXP that contains the name (of the
function, variable or type) that is to appear in the menu.

The variable `imenu-case-fold-search' determines whether or not the
regexp matches are case sensitive, and `imenu-syntax-alist' can be
used to alter the syntax table for the search.

See `lisp-imenu-generic-expression' for an example of PATTERNS.

Returns an index of the current buffer as an alist.  The elements in
the alist look like:
 (INDEX-NAME . INDEX-POSITION)
or like:
 (INDEX-NAME INDEX-POSITION FUNCTION ARGUMENTS...)
They may also be nested index alists like:
 (INDEX-NAME . INDEX-ALIST)
depending on PATTERNS."
  (let ((index-alist       (list 'dummy))
        (case-fold-search  (if (or (local-variable-p 'imenu-case-fold-search)
                                   (not (local-variable-p 'font-lock-defaults)))
                               imenu-case-fold-search
                             (nth 2 font-lock-defaults)))
        (old-table         (syntax-table))
        (table             (copy-syntax-table (syntax-table)))
        (slist             imenu-syntax-alist)
        prev-pos)
    (dolist (syn  slist)                ; Modify the syntax table used while matching regexps.
      (if (numberp (car syn))           ; The char(s) to modify may be a single char or a string.
          (modify-syntax-entry (car syn) (cdr syn) table)
        (mapc (lambda (c) (modify-syntax-entry c (cdr syn) table)) (car syn))))
    (goto-char (point-max))
    (imenu-progress-message prev-pos 0 t)
    (unwind-protect			; for syntax table
         (save-match-data
           (set-syntax-table table)
           (dolist (pat  patterns)      ; Map over the elements of `imenu-generic-expression'.
             (let ((menu-title  (car pat))
                   (regexp      (nth 1 pat))
                   (index       (nth 2 pat))
                   (function    (nth 3 pat))
                   (rest        (nthcdr 4 pat))
                   start beg)
               (goto-char (point-max))  ; Go backwards for convenience of adding items in order.
               (while (and (if (functionp regexp)
                               (funcall regexp)
                             (and (re-search-backward regexp nil t)
                                  ;; Do not count invisible definitions.
                                  (let ((invis  (imenup-invisible-p (point))))
                                    (or (not invis)
                                        (progn
                                          (while (and invis  (not (bobp)))
                                            (setq invis  (not (re-search-backward regexp nil 'MOVE))))
                                          (not invis))))))
                           ;; Exit loop if empty match -it means a bad regexp was specified.
                           (not (= (match-beginning 0) (match-end 0))))
                 (setq start  (point))
                 ;; Record the start of the line in which the match starts.
                 ;; That's the official position of this definition.
                 (goto-char (match-beginning index))
                 (beginning-of-line)
                 (setq beg  (point))
                 (imenu-progress-message prev-pos nil t)
                 ;; Add this sort of submenu only when find an item for it, to avoid empty menus.
                 (unless (assoc menu-title index-alist) (push (list menu-title) index-alist))
                 (when imenu-use-markers (setq beg  (copy-marker beg)))
                 (let ((item  (if function
                                  (nconc (list (match-string-no-properties index) beg function)
                                         rest)
                                (cons (match-string-no-properties index) beg)))
                       ;; This is the desired submenu, starting with its title (or nil).
                       (menu (assoc menu-title index-alist)))
                   ;; Insert the item unless it is already present or is in a comment being ignored.
                   (unless (or (and (boundp 'imenup-ignore-comments-flag)  imenup-ignore-comments-flag
                                    (nth 8 (syntax-ppss)))
                               (member item (cdr menu)))
                     (setcdr menu (cons item (cdr menu)))))
                 ;; Go to the start of the match, to make sure we keep making progress backwards.
                 (goto-char start))))
           (set-syntax-table old-table)))
    (imenu-progress-message prev-pos 100 t)
    ;; Sort each submenu by position.
    ;; This is in case one submenu gets items from two different regexps.
    (dolist (item  index-alist)
      (when (listp item) (setcdr item (sort (cdr item) 'imenu--sort-by-position))))
    (let ((main-element  (assq nil index-alist)))
      (nconc (delq main-element (delq 'dummy index-alist)) (cdr main-element)))))


;; REPLACE ORIGINAL in `imenu.el'.
;;
;; Sort each submenu before splitting submenus, instead of sorting among submenus after.
;;
(defun imenu--mouse-menu (index-alist event &optional title)
  "Let the user select from a buffer index from a mouse menu.
INDEX-ALIST is the buffer index.
EVENT is a mouse event.
TITLE is the menu title.
Returns t for rescan, or else an element or subelement of INDEX-ALIST."
  (setq index-alist  (imenu--split-submenus
                      (if imenu-sort-function
                          (mapcar (lambda (sm) (imenup--sort-submenu sm imenu-sort-function))
                                  index-alist)
                        index-alist)))
  (if (>= emacs-major-version 22)
      (let* ((menu  (imenu--split-menu index-alist (or title  (buffer-name))))
             (map   (imenu--create-keymap (car menu)
                                          (cdr (if (< 1 (length (cdr menu)))
                                                   menu
                                                 (car (cdr menu)))))))
        (popup-menu map event))
    (let ((menu  (imenu--split-menu index-alist (or title  (buffer-name))))
          position)
      (setq menu      (imenu--create-keymap-1 (car menu) (if (< 1 (length (cdr menu)))
                                                             (cdr menu)
                                                           (cdr (cadr menu))))
            position  (x-popup-menu event menu))
      (cond ((eq position nil)
             position)
            ;; If one call to x-popup-menu handled the nested menus, find the result by looking down
            ;; the menus here.
            ((and (listp position)  (numberp (car position))  (stringp (nth (1- (length position))
                                                                            position)))
             (let ((final  menu))
               (while position
                 (setq final     (assq (car position) final)
                       position  (cdr position)))
               (or (string= (car final) (car imenu--rescan-item))  (nthcdr 3 final))))
            ;; If x-popup-menu went just one level and found a leaf item, return the INDEX-ALIST
            ;; element for that.
            ((and (consp position)  (stringp (car position))  (null (cdr position)))
             (or (string= (car position) (car imenu--rescan-item))
                 (assq (car position) index-alist)))
            ;; If x-popup-menu went just one level and found a non-leaf item (a submenu),
            ;; recurse to handle the rest.
            ((listp position)
             (imenu--mouse-menu position event (if title
                                                   (concat title imenu-level-separator
                                                           (car (rassq position index-alist)))
                                                 (car (rassq position index-alist)))))))))

;;;;;;;;;;;;;;;;;;

(provide 'imenu+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; imenu+.el ends here
