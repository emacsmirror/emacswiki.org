;; ide-skel.el --- IDE skeleton for Emacs Lisp hackers

;; Copyright (C) 2008 Peter Karpiuk, Scott Tiger S.A.

;; Author: Peter Karpiuk <piotr.karpiuk (at) gmail (dot) com>
;; Maintainer: Peter Karpiuk <piotr.karpiuk (at) gmail (dot) com>
;; Created: 24 Apr 2008
;; Version 0.6.0
;; Keywords: ide speedbar

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;   Ide-skel is a skeleton (or framework) of IDE for Emacs users.
;;   Like Eclipse, it can be used as is with some predefined plugins
;;   on board, but is designed to extend by Emacs Lisp programmers to
;;   suite their own needs.  Emacs 22 only, tested under Linux only
;;   (under Windows ide-skel.el will rather not work, sorry).
;;
;; ** Configuration in .emacs
;;
;;     (require 'ide-skel)
;;
;;     ;; optional, but useful - see Emacs Manual
;;     (partial-completion-mode)
;;     (icomplete-mode)
;;
;;     ;; for convenience
;;     (global-set-key [f4] 'ide-skel-proj-find-files-by-regexp)
;;     (global-set-key [f5] 'ide-skel-proj-grep-files-by-regexp)
;;     (global-set-key [f10] 'ide-skel-toggle-left-view-window)
;;     (global-set-key [f11] 'ide-skel-toggle-bottom-view-window)
;;     (global-set-key [f12] 'ide-skel-toggle-right-view-window)
;;     (global-set-key [C-next] 'tabbar-backward)
;;     (global-set-key [C-prior]  'tabbar-forward)
;;
;; ** Side view windows
;;
;;   Left and right view windows are "speedbars" - they are embedded
;;   inside main Emacs frame and can be open/closed independently.
;;   Right view window summarizes information related to the current
;;   editor buffer - it can present content of such buffer in another
;;   way (eg. Imenu tree), or show an extra panel for buffer major
;;   mode operations (see SQL*Plus mode plugin example).  Left view
;;   window contains buffers such like buffer list (yet another,
;;   popular way for switching buffers), filesystem/project browser
;;   for easy navigation, or Info documentation browser,
;;   or... whatever you wish.
;;
;;   Side view windows are special - they cannot take focus and we can
;;   operate on it only with mouse (!).  Some window operations like
;;   delete-other-windows (C-x 1) are slighty modified to treat side
;;   view windows specially.
;;
;; ** Bottom view window
;;
;;   Let auxiliary buffers (*shell*, *Messages*, *Help*, *Compilation*
;;   and another buffers with '*' in name) pop up/show in bottom
;;   window only.  BUT, if you want, you can open any buffer in any
;;   window (except side windows) as usual - that's only nice
;;   heuristic, not pressure.
;;
;;   Bottom view window remembers last selected buffer within it, so
;;   if you close this window and open later, it will show you buffer
;;   which you expect.
;;
;; ** Tabbars
;;
;;   Ide-skel uses (great) tabbar.el package with some modifications:
;;
;;     - there is no division into major mode groups (like in
;;       Eclipse),
;;
;;     - side view windows, bottom view window and editor windows have
;;       different tabsets,
;;
;;     - you can scroll tabs with mouse wheel,
;;
;;     - the Home button in window left corner acts as window menu
;;       (you can add your items to it in your plugin),
;;
;;     - mouse-3 click on tab kills its buffer
;;
;; * Project
;;
;;   Here, "project" means a directory tree checked out from CVS or
;;   SVN.  One project can contain source files of many types.  When
;;   we edit any project file, Emacs can easily find the project root
;;   directory simply by looking at filesystem.
;;
;;   So, we can execute many commands (grep, find, replace) on all
;;   project source files or on all project source files of the same
;;   type as file edited now (see Project menu).  Ide-skel package
;;   also automatically configures partial-completion-mode for project
;;   edited now.
;;
;;   There is no configuration for concrete projects needed (and
;;   that's great advantage in my opinion).

;; If you find this package useful, send me a postcard to address:
;;
;;  Peter Karpiuk
;;  Scott Tiger S.A.
;;  ul. Gawinskiego 8
;;  01-645 Warsaw
;;  Poland


;; * Notes for Emacs Lisp hackers
;;
;;   Each side window buffer should have:
;;
;;     - name that begins with space,
;;
;;     - tab label (string) - buffer local IDE-SKEL-TABBAR-TAB-LABEL
;;       variable,
;;
;;     - keep condition function (IDE-SKEL-KEEP-CONDITION-FUNCTION),
;;
;;     - menu (IDE-SKEL-TABBAR-MENU-FUNCTION) - optional.
;;
;;   Side window buffer is enabled (can be choosed by mouse click on
;;   his tab) if it has buffer local variable IDE-SKEL-TABBAR-ENABLED
;;   set to non-nil.  There may be many live side window buffers, but
;;   unavailable in current context ("context" means buffer edited in
;;   editor window) if they have IDE-SKEL-TABBAR-ENABLED set to nil.
;;
;;   Hiding side window operation disables all window buffers.  "Show
;;   side window" event handler should enable (and maybe create) side
;;   window buffers based on current context.  When you switch to
;;   other buffer in editor window (switching the context), all side
;;   window buffers for which keep condition function returns nil are
;;   disabled.  Handlers for EDITOR-BUFFER-CHANGED event should enable
;;   (and maybe create) additional buffers based on current context.
;;
;; ** Side window events
;;
;;   Event handlers should be implemented as an abnormal hook:
;;
;;     ide-skel-side-view-window-functions
;;
;;   It should be function with parameters
;;
;;     - side: symbol LEFT or RIGHT
;;
;;     - event-type: symbol for event:
;;       SHOW/EDITOR-BUFFER-CHANGED/TAB-CHANGE/HIDE
;;
;;     - list (optional): event parameters specific for event type.
;;
;;   Events are send only for opened (existing and visible) windows.
;;
;;   Hook functions are called in order until one of them returns
;;   non-nil.
;;
;; *** Show
;;
;;   After side window open.  Event handler should enable (and maybe
;;   create) buffers appropriate for current context.  After event
;;   handle, if no side window buffer is selected, there will be
;;   selected one of them.  No parameters.
;;
;; *** Editor Buffer Changed
;;
;;   After editor buffer changed (aka context switch).
;;
;;   Before event, buffers for which keep condition function returns
;;   nil, are disabled.  Event handler should enable (and maybe
;;   create) buffers appropriate for new context.
;;
;;   Parameters: before-buffer current-buffer.
;;
;; *** Tab Change
;;
;;   Before side window buffer change (as result of mouse click on tab
;;   or ide-skel-side-window-switch-to-buffer function call).
;;   Parameters: current-buffer new-buffer
;;
;; *** Hide
;;
;;   Before side window hiding.  After event handling, all side window
;;   buffers are disabled.
;;
;; *** Functions & vars
;;
;;   In plugins, you can use variables with self-descriptive names:
;;
;;   ide-skel-selected-frame
;;   ide-skel-current-editor-window
;;   ide-skel-current-editor-buffer
;;   ide-skel-current-left-view-window
;;   ide-skel-current-right-view-window
;;
;;   Moreover, when user selects another buffer to edit, the
;;
;;     ide-skel-editor-buffer-changed-hook
;;
;;   hook is run.  It is similar to "editor buffer changed" event, but
;;   has no parameters and is run even when all side windows are
;;   closed.
;;
;; **** Functions
;;
;;   ide-skel-side-window-switch-to-buffer (side-window buffer)
;;     Switch buffer in side window (please use only this function for
;;     this operation).
;;
;;   ide-skel-get-side-view-buffer-create (name side-sym tab-label
;;      help-string keep-condition-function)
;;     Create new buffer for side view window.  NAME should begin with
;;     space, side sym should be LEFT or RIGHT.
;;
;; **** Local variables in side window buffers
;;
;;   ide-skel-tabbar-tab-label
;;   ide-skel-tabbar-tab-help-string
;;   ide-skel-tabbar-menu-function
;;   ide-skel-tabbar-enabled
;;   ide-skel-keep-condition-function

(require 'cl)
(require 'complete)
(require 'tree-widget)
(require 'tabbar)
(require 'recentf)

(defgroup ide-skel nil
  "Ide Skeleton"
  :group 'tools
  :version 21)

(defcustom ide-skel-tabbar-hidden-buffer-names-regexp-list '("^TAGS" "^diary$")
  "Buffer name that matches any of this regexps, will have no tab."
  :group 'ide-skel
  :tag "Hidden Buffer Names Regexp List"
  :type '(repeat regexp)
  :initialize 'custom-initialize-default
  :set (lambda (symbol value)
	 (when tabbar-mode
	   (tabbar-init-tabsets-store))
	 (set-default symbol value)))

(defcustom ide-skel-bottom-view-buffer-names-regexps '("\\*.*\\*")
  "Buffers with names matched by one of this regexps will be shown in bottom view."
  :group 'ide-skel
  :tag "Bottom View Buffer Names Regexps"
  :type '(repeat regexp)
  :initialize 'custom-initialize-default
  :set (lambda (symbol value)
	 (when tabbar-mode
	   (tabbar-init-tabsets-store))
	 (set-default symbol value))
  )

(defcustom ide-skel-bottom-view-buffer-names-disallowed-regexps '("\\*info\\*" "\\*Backtrace\\*")
  "Buffers with names matched by one of this regexps will NOT be shown in bottom view."
  :group 'ide-skel
  :tag "Bottom View Buffer Names Disallowed Regexps"
  :type '(repeat regexp)
  :initialize 'custom-initialize-default
  :set (lambda (symbol value)
	 (when tabbar-mode
	   (tabbar-init-tabsets-store))
	 (set-default symbol value))
  )

(defconst ide-skel-left-view-window-tabset-name "LeftView")
(defconst ide-skel-right-view-window-tabset-name "RightView")
(defconst ide-skel-bottom-view-window-tabset-name "BottomView")
(defconst ide-skel-editor-window-tabset-name "Editor")

(defun ide-skel-shine-color (color percent)
  (when (equal color "unspecified-bg")
    (setq color (if (< percent 0) "white" "black")))
  (apply 'format "#%02x%02x%02x" 
         (mapcar (lambda (value)
                   (min 65535 (max 0 (* (+ (/ value 650) percent) 650))))
                 (color-values color))))

(defun ide-skel-color-percentage (color)
  (truncate (* (/ (/ (reduce '+ (color-values color)) 3.0) 65535.0) 100.0)))

(defun ide-skel-shine-face-background (face-sym percent)
  (when (>= (ide-skel-color-percentage (face-background 'default)) 50)
    (setq percent (- percent)))
  (set-face-attribute face-sym nil
		      :background (ide-skel-shine-color (face-background 'default) percent)))

(defun ide-skel-shine-face-foreground (face-sym percent)
  (when (>= (ide-skel-color-percentage (face-foreground 'default)) 50)
    (setq percent (- percent)))
  (set-face-attribute face-sym nil
		      :foreground (ide-skel-shine-color (face-foreground 'default) percent)))


(defvar ide-skel-tabbar-tab-label-max-width 25
  "Max width for tab label.  Nil means no limit.  If label width is too big, it will be shortened with ... inside.")

(defvar ide-skel-tabbar-tab-label nil
  "Tab name.  Local for buffer in side view window.")
(make-variable-buffer-local 'ide-skel-tabbar-tab-label)

(defvar ide-skel-tabbar-tab-help-string nil
  "Tooltip text for tab in side view window.  Buffer local.")
(make-variable-buffer-local 'ide-skel-tabbar-tab-help-string)

(defvar ide-skel-tabset-name nil)
(make-variable-buffer-local 'ide-skel-tabset-name)

(defvar ide-skel-tabbar-menu-function nil)
(make-variable-buffer-local 'ide-skel-tabbar-menu-function)

(defvar ide-skel-tabbar-enabled nil)
(make-variable-buffer-local 'ide-skel-tabbar-enabled)

(defvar ide-skel-keep-condition-function nil)
(make-variable-buffer-local 'ide-skel-keep-condition-function)

(defvar ide-skel-current-left-view-window nil)
(defvar ide-skel-current-right-view-window nil)
(defvar ide-skel-current-editor-window nil)
(defvar ide-skel-current-editor-buffer nil)
(defvar ide-skel-selected-frame nil)

(defconst ide-skel-left-view-window-xpm "\
/* XPM */
static char * left_view_xpm[] = {
\"24 24 145 2\",
\"  	c None\",
\". 	c #000000\",
\"+ 	c #FBFED6\",
\"@ 	c #F3F6CE\",
\"# 	c #EBEEC7\",
\"$ 	c #E3E7BF\",
\"% 	c #DCE0B9\",
\"& 	c #D5D9B2\",
\"* 	c #FFFFFF\",
\"= 	c #FDFDFD\",
\"- 	c #F9F9F9\",
\"; 	c #F4F4F4\",
\"> 	c #DDDDDD\",
\", 	c #F2F5CD\",
\"' 	c #E4E8C0\",
\") 	c #DDE1BA\",
\"! 	c #D7DAB4\",
\"~ 	c #D1D4AE\",
\"{ 	c #FEFEFE\",
\"] 	c #FBFBFB\",
\"^ 	c #F8F8F8\",
\"/ 	c #F5F5F5\",
\"( 	c #F2F2F2\",
\"_ 	c #DBDBDB\",
\": 	c #E9EDC5\",
\"< 	c #D8DBB5\",
\"[ 	c #D2D5AF\",
\"} 	c #CDD0AA\",
\"| 	c #FCFCFC\",
\"1 	c #F6F6F6\",
\"2 	c #F3F3F3\",
\"3 	c #F0F0F0\",
\"4 	c #DADADA\",
\"5 	c #E1E5BD\",
\"6 	c #CDD0AB\",
\"7 	c #C8CCA6\",
\"8 	c #FAFAFA\",
\"9 	c #F7F7F7\",
\"0 	c #EFEFEF\",
\"a 	c #D9D9D9\",
\"b 	c #DADDB6\",
\"c 	c #C4C7A2\",
\"d 	c #EDEDED\",
\"e 	c #D7D7D7\",
\"f 	c #D3D6B0\",
\"g 	c #CFD3AD\",
\"h 	c #CBCFA9\",
\"i 	c #C8CBA6\",
\"j 	c #C0C39F\",
\"k 	c #F1F1F1\",
\"l 	c #EEEEEE\",
\"m 	c #ECECEC\",
\"n 	c #D6D6D6\",
\"o 	c #C9CDA7\",
\"p 	c #C6C9A4\",
\"q 	c #C3C6A1\",
\"r 	c #BFC39E\",
\"s 	c #BCBF9B\",
\"t 	c #EAEAEA\",
\"u 	c #D4D4D4\",
\"v 	c #C7CAA5\",
\"w 	c #C1C5A0\",
\"x 	c #BEC29D\",
\"y 	c #BBBF9B\",
\"z 	c #B9BC98\",
\"A 	c #EBEBEB\",
\"B 	c #E8E8E8\",
\"C 	c #D3D3D3\",
\"D 	c #C2C5A0\",
\"E 	c #BDC09C\",
\"F 	c #BABE99\",
\"G 	c #B8BB97\",
\"H 	c #B5B895\",
\"I 	c #E9E9E9\",
\"J 	c #E7E7E7\",
\"K 	c #D1D1D1\",
\"L 	c #BBBE9A\",
\"M 	c #B7BA96\",
\"N 	c #B4B794\",
\"O 	c #B2B592\",
\"P 	c #E5E5E5\",
\"Q 	c #D0D0D0\",
\"R 	c #B3B693\",
\"S 	c #B1B491\",
\"T 	c #AFB28F\",
\"U 	c #E3E3E3\",
\"V 	c #CECECE\",
\"W 	c #B4B793\",
\"X 	c #B0B390\",
\"Y 	c #AEB18F\",
\"Z 	c #ACAF8D\",
\"` 	c #E6E6E6\",
\" .	c #E4E4E4\",
\"..	c #E2E2E2\",
\"+.	c #CDCDCD\",
\"@.	c #ADB08E\",
\"#.	c #ABAE8C\",
\"$.	c #AAAD8B\",
\"%.	c #E0E0E0\",
\"&.	c #CBCBCB\",
\"*.	c #A9AC8A\",
\"=.	c #A7AA89\",
\"-.	c #DEDEDE\",
\";.	c #CACACA\",
\">.	c #ABAE8B\",
\",.	c #A8AB89\",
\"'.	c #A6A988\",
\").	c #A5A887\",
\"!.	c #C8C8C8\",
\"~.	c #A7AA88\",
\"{.	c #A6A987\",
\"].	c #A4A786\",
\"^.	c #A3A685\",
\"/.	c #DFDFDF\",
\"(.	c #C7C7C7\",
\"_.	c #A5A886\",
\":.	c #A2A584\",
\"<.	c #A1A483\",
\"[.	c #C6C6C6\",
\"}.	c #A4A785\",
\"|.	c #A0A382\",
\"1.	c #9FA282\",
\"2.	c #D8D8D8\",
\"3.	c #C4C4C4\",
\"4.	c #A3A684\",
\"5.	c #A2A484\",
\"6.	c #A0A383\",
\"7.	c #9EA181\",
\"8.	c #9DA080\",
\"9.	c #C3C3C3\",
\"0.	c #8D8F72\",
\"a.	c #8C8E72\",
\"b.	c #8B8D71\",
\"c.	c #8A8C70\",
\"d.	c #898B6F\",
\"e.	c #888A6F\",
\"f.	c #C5C5C5\",
\"g.	c #C2C2C2\",
\"h.	c #C1C1C1\",
\"i.	c #C0C0C0\",
\"j.	c #BEBEBE\",
\"k.	c #BDBDBD\",
\"l.	c #BBBBBB\",
\"m.	c #BABABA\",
\"n.	c #ABABAB\",
\"                                                \",
\"  . . . . . . . . . . . . . . . . . . . . . .   \",
\". + @ # $ % & . * * * * * * * * * * = - ; ; > . \",
\". , # ' ) ! ~ . * * * * * * * * * { ] ^ / ( _ . \",
\". : $ ) < [ } . * * * * * * * * * | - 1 2 3 4 . \",
\". 5 % ! [ 6 7 . * * * * * * * * = 8 9 ; 3 0 a . \",
\". b & ~ } 7 c . * * * * * * * { ] ^ / ( 0 d e . \",
\". f g h i c j . * * * * * * * | - 1 2 k l m n . \",
\". } o p q r s . * * * * * * = 8 9 ; 3 0 m t u . \",
\". v c w x y z . * * * * * = 8 9 / ( 0 d A B C . \",
\". D r E F G H . * * * * { ] ^ / 2 3 l A I J K . \",
\". E L z M N O . * * * { ] ^ 1 2 3 l m I J P Q . \",
\". z M H R S T . * * { ] ^ 1 2 k l m t B P U V . \",
\". H W O X Y Z . * = ] ^ 1 2 k 0 m t B `  ...+.. \",
\". O X T @.#.$.. = 8 ^ 1 2 k 0 m t B `  ...%.&.. \",
\". T @.Z $.*.=.. 8 9 / 2 k 0 m t B `  ...%.-.;.. \",
\". Z >.*.,.'.).. 9 / 2 3 l m t B `  ...%.-.> !.. \",
\". *.,.~.{.].^.. ; ( 3 l m t B `  ...%./.> _ (.. \",
\". ~.{._.^.:.<.. k 0 l m t B `  ...%./.> _ a [.. \",
\". _.}.:.<.|.1.. 0 d A I B `  ...%./.> _ a 2.3.. \",
\". 4.5.6.1.7.8.. m A I J P  ...%.-.> _ a 2.n 9.. \",
\". 0.a.b.c.d.e.. +.&.;.!.(.f.3.g.h.i.j.k.l.m.n.. \",
\"  . . . . . . . . . . . . . . . . . . . . . .   \",
\"                                                \"};
"
  "XPM format image used as left view window icon")

(defconst ide-skel-left-view-window-image
  (create-image ide-skel-left-view-window-xpm 'xpm t))

(defconst ide-skel-right-view-window-xpm "\
/* XPM */
static char * right_view_xpm[] = {
\"24 24 125 2\",
\"  	c None\",
\". 	c #000000\",
\"+ 	c #FFFFFF\",
\"@ 	c #A8AB89\",
\"# 	c #A6A987\",
\"$ 	c #A4A785\",
\"% 	c #A2A484\",
\"& 	c #A0A282\",
\"* 	c #919376\",
\"= 	c #A7AA88\",
\"- 	c #A5A886\",
\"; 	c #A2A584\",
\"> 	c #A0A383\",
\", 	c #9FA181\",
\"' 	c #909275\",
\") 	c #A3A685\",
\"! 	c #A1A483\",
\"~ 	c #9FA282\",
\"{ 	c #9DA080\",
\"] 	c #8F9174\",
\"^ 	c #A4A786\",
\"/ 	c #A0A382\",
\"( 	c #9EA181\",
\"_ 	c #9C9F7F\",
\": 	c #8E9073\",
\"< 	c #FEFEFE\",
\"[ 	c #9B9E7F\",
\"} 	c #8D8F73\",
\"| 	c #FCFCFC\",
\"1 	c #A1A484\",
\"2 	c #9EA180\",
\"3 	c #9A9D7E\",
\"4 	c #8C8E72\",
\"5 	c #FDFDFD\",
\"6 	c #FAFAFA\",
\"7 	c #9B9E7E\",
\"8 	c #999C7D\",
\"9 	c #8B8D71\",
\"0 	c #F7F7F7\",
\"a 	c #9FA281\",
\"b 	c #9A9C7D\",
\"c 	c #989B7C\",
\"d 	c #8A8C70\",
\"e 	c #FBFBFB\",
\"f 	c #F8F8F8\",
\"g 	c #F5F5F5\",
\"h 	c #9C9E7F\",
\"i 	c #9A9D7D\",
\"j 	c #979A7B\",
\"k 	c #898B70\",
\"l 	c #F6F6F6\",
\"m 	c #F3F3F3\",
\"n 	c #999C7C\",
\"o 	c #96997A\",
\"p 	c #888A6F\",
\"q 	c #F1F1F1\",
\"r 	c #9B9D7E\",
\"s 	c #989A7B\",
\"t 	c #959779\",
\"u 	c #87896E\",
\"v 	c #EFEFEF\",
\"w 	c #959879\",
\"x 	c #949678\",
\"y 	c #86886D\",
\"z 	c #ECECEC\",
\"A 	c #97997B\",
\"B 	c #949778\",
\"C 	c #939577\",
\"D 	c #85876C\",
\"E 	c #EAEAEA\",
\"F 	c #95987A\",
\"G 	c #919476\",
\"H 	c #84876C\",
\"I 	c #F9F9F9\",
\"J 	c #F0F0F0\",
\"K 	c #EEEEEE\",
\"L 	c #E8E8E8\",
\"M 	c #949779\",
\"N 	c #939578\",
\"O 	c #929476\",
\"P 	c #909375\",
\"Q 	c #83866B\",
\"R 	c #F4F4F4\",
\"S 	c #F2F2F2\",
\"T 	c #E6E6E6\",
\"U 	c #939678\",
\"V 	c #929477\",
\"W 	c #909376\",
\"X 	c #8F9275\",
\"Y 	c #82856A\",
\"Z 	c #E4E4E4\",
\"` 	c #8E9174\",
\" .	c #818469\",
\"..	c #EDEDED\",
\"+.	c #EBEBEB\",
\"@.	c #E9E9E9\",
\"#.	c #E2E2E2\",
\"$.	c #8D9073\",
\"%.	c #808368\",
\"&.	c #E7E7E7\",
\"*.	c #E5E5E5\",
\"=.	c #E0E0E0\",
\"-.	c #8C8F72\",
\";.	c #7F8268\",
\">.	c #D6D6D6\",
\",.	c #D5D5D5\",
\"'.	c #D4D4D4\",
\").	c #D2D2D2\",
\"!.	c #D1D1D1\",
\"~.	c #D0D0D0\",
\"{.	c #CECECE\",
\"].	c #CDCDCD\",
\"^.	c #CBCBCB\",
\"/.	c #CACACA\",
\"(.	c #C8C8C8\",
\"_.	c #C7C7C7\",
\":.	c #C5C5C5\",
\"<.	c #C4C4C4\",
\"[.	c #C2C2C2\",
\"}.	c #7D8066\",
\"|.	c #7C7F65\",
\"1.	c #7B7E64\",
\"2.	c #7B7D64\",
\"3.	c #7A7C63\",
\"4.	c #70725B\",
\"                                                \",
\"  . . . . . . . . . . . . . . . . . . . . . .   \",
\". + + + + + + + + + + + + + + + . @ # $ % & * . \",
\". + + + + + + + + + + + + + + + . = - ; > , ' . \",
\". + + + + + + + + + + + + + + + . # ) ! ~ { ] . \",
\". + + + + + + + + + + + + + + + . ^ ; / ( _ : . \",
\". + + + + + + + + + + + + + + < . ) ! ~ { [ } . \",
\". + + + + + + + + + + + + + + | . 1 & 2 _ 3 4 . \",
\". + + + + + + + + + + + + + 5 6 . > ( _ 7 8 9 . \",
\". + + + + + + + + + + + + 5 6 0 . a { 7 b c d . \",
\". + + + + + + + + + + + < e f g . { h i c j k . \",
\". + + + + + + + + + + < e f l m . _ 3 n j o p . \",
\". + + + + + + + + + < e f l m q . r 8 s o t u . \",
\". + + + + + + + + 5 e f l m q v . 8 c o w x y . \",
\". + + + + + + + 5 6 f l m q v z . c A w B C D . \",
\". + + + + + < | 6 0 g m q v z E . A F B C G H . \",
\". + + + + 5 e I 0 g m J K z E L . F M N O P Q . \",
\". + + < | 6 f l R S J K z E L T . M U V W X Y . \",
\". < 5 e I 0 g m q v K z E L T Z . U V * X `  .. \",
\". e I f l R S q v ..+.@.L T Z #.. V * X ` $.%.. \",
\". f l g m q J K z +.@.&.*.Z #.=.. W X ` $.-.;.. \",
\". >.,.'.).!.~.{.].^./.(._.:.<.[.. }.|.1.2.3.4.. \",
\"  . . . . . . . . . . . . . . . . . . . . . .   \",
\"                                                \"};
"
  "XPM format image used as right view window icon")

(defconst ide-skel-right-view-window-image
  (create-image ide-skel-right-view-window-xpm 'xpm t))

(defconst ide-skel-bottom-view-window-xpm "\
/* XPM */
static char * bottom_view_xpm[] = {
\"24 24 130 2\",
\"  	c None\",
\". 	c #000000\",
\"+ 	c #FFFFFF\",
\"@ 	c #FDFDFD\",
\"# 	c #F9F9F9\",
\"$ 	c #F6F6F6\",
\"% 	c #F4F4F4\",
\"& 	c #DDDDDD\",
\"* 	c #FEFEFE\",
\"= 	c #FBFBFB\",
\"- 	c #F8F8F8\",
\"; 	c #F5F5F5\",
\"> 	c #F2F2F2\",
\", 	c #DBDBDB\",
\"' 	c #FCFCFC\",
\") 	c #F3F3F3\",
\"! 	c #F0F0F0\",
\"~ 	c #DADADA\",
\"{ 	c #FAFAFA\",
\"] 	c #F7F7F7\",
\"^ 	c #F1F1F1\",
\"/ 	c #EFEFEF\",
\"( 	c #D9D9D9\",
\"_ 	c #EDEDED\",
\": 	c #D7D7D7\",
\"< 	c #EEEEEE\",
\"[ 	c #ECECEC\",
\"} 	c #D6D6D6\",
\"| 	c #EAEAEA\",
\"1 	c #D4D4D4\",
\"2 	c #EBEBEB\",
\"3 	c #E8E8E8\",
\"4 	c #D3D3D3\",
\"5 	c #E9E9E9\",
\"6 	c #E7E7E7\",
\"7 	c #D1D1D1\",
\"8 	c #E5E5E5\",
\"9 	c #D0D0D0\",
\"0 	c #E3E3E3\",
\"a 	c #CECECE\",
\"b 	c #E6E6E6\",
\"c 	c #E4E4E4\",
\"d 	c #E2E2E2\",
\"e 	c #CDCDCD\",
\"f 	c #E0E0E0\",
\"g 	c #CBCBCB\",
\"h 	c #CCCFAB\",
\"i 	c #CACDAA\",
\"j 	c #C8CBA8\",
\"k 	c #C7CAA7\",
\"l 	c #C5C8A5\",
\"m 	c #C3C6A4\",
\"n 	c #C2C5A3\",
\"o 	c #C0C3A1\",
\"p 	c #BEC1A0\",
\"q 	c #BDBF9E\",
\"r 	c #BBBE9D\",
\"s 	c #B9BC9B\",
\"t 	c #B8BA9A\",
\"u 	c #B6B999\",
\"v 	c #B4B797\",
\"w 	c #B3B596\",
\"x 	c #B1B495\",
\"y 	c #B0B293\",
\"z 	c #AEB192\",
\"A 	c #ADAF91\",
\"B 	c #ABAE8F\",
\"C 	c #9C9E82\",
\"D 	c #C9CCA8\",
\"E 	c #C6C9A6\",
\"F 	c #C4C7A5\",
\"G 	c #C1C4A2\",
\"H 	c #BFC2A1\",
\"I 	c #BEC19F\",
\"J 	c #BCBF9E\",
\"K 	c #BABD9C\",
\"L 	c #B7BA9A\",
\"M 	c #B6B998\",
\"N 	c #ABAE90\",
\"O 	c #AAAD8E\",
\"P 	c #9A9D81\",
\"Q 	c #C2C4A2\",
\"R 	c #BFC1A0\",
\"S 	c #BDC09F\",
\"T 	c #BCBE9D\",
\"U 	c #B9BB9B\",
\"V 	c #B7BA99\",
\"W 	c #B6B898\",
\"X 	c #B1B494\",
\"Y 	c #A9AB8D\",
\"Z 	c #999C80\",
\"` 	c #C1C3A2\",
\" .	c #BFC2A0\",
\"..	c #B9BC9C\",
\"+.	c #B8BB9A\",
\"@.	c #B7B999\",
\"#.	c #B5B898\",
\"$.	c #B4B697\",
\"%.	c #B2B596\",
\"&.	c #AAAD8F\",
\"*.	c #A7AA8C\",
\"=.	c #989B80\",
\"-.	c #BDC09E\",
\";.	c #B3B696\",
\">.	c #B2B595\",
\",.	c #B1B394\",
\"'.	c #AFB293\",
\").	c #A6A98B\",
\"!.	c #97997F\",
\"~.	c #A7A98C\",
\"{.	c #A6A88B\",
\"].	c #A4A78A\",
\"^.	c #A3A689\",
\"/.	c #A2A588\",
\"(.	c #A1A487\",
\"_.	c #A0A286\",
\":.	c #9FA185\",
\"<.	c #9EA084\",
\"[.	c #9D9F83\",
\"}.	c #9B9E82\",
\"|.	c #999B80\",
\"1.	c #989A7F\",
\"2.	c #97997E\",
\"3.	c #96987D\",
\"4.	c #95977D\",
\"5.	c #94967C\",
\"6.	c #92957B\",
\"7.	c #91947A\",
\"8.	c #909279\",
\"9.	c #85876F\",
\"                                                \",
\"  . . . . . . . . . . . . . . . . . . . . . .   \",
\". + + + + + + + + + + + + + + + + + @ # $ % & . \",
\". + + + + + + + + + + + + + + + + * = - ; > , . \",
\". + + + + + + + + + + + + + + + + ' # $ ) ! ~ . \",
\". + + + + + + + + + + + + + + + @ { ] % ^ / ( . \",
\". + + + + + + + + + + + + + + * = - ; > ! _ : . \",
\". + + + + + + + + + + + + + + ' # $ ) / < [ } . \",
\". + + + + + + + + + + + + + @ { ] % ^ < [ | 1 . \",
\". + + + + + + + + + + + + @ { ] ; > / _ 2 3 4 . \",
\". + + + + + + + + + + + * = - ; > ! < 2 5 6 7 . \",
\". + + + + + + + + + + * = - $ ) ! < [ 5 6 8 9 . \",
\". + + + + + + + + + * = - $ ) ^ < [ | 3 8 0 a . \",
\". + + + + + + + + @ = - $ ) ^ / [ | 3 b c d e . \",
\". + + + + + + + @ { - $ ) ^ / [ | 3 b c d f g . \",
\". . . . . . . . . . . . . . . . . . . . . . . . \",
\". h i j k l m n o p q r s t u v w x y z A B C . \",
\". D k E F n G H I J K s L M v w x y z A N O P . \",
\". E F m Q o R S T K U V W v w X y z A N O Y Z . \",
\". m n `  .I J r ..+.@.#.$.%.X y z A N &.Y *.=.. \",
\". G H p -.T K s t u #.;.>.,.'.z A N O Y *.).!.. \",
\". ~.{.].^./.(._.:.<.[.}.P |.1.2.3.4.5.6.7.8.9.. \",
\"  . . . . . . . . . . . . . . . . . . . . . .   \",
\"                                                \"};
"
  "XPM format image used as bottom view window icon")

(defconst ide-skel-bottom-view-window-image
  (create-image ide-skel-bottom-view-window-xpm 'xpm t))

(defvar ide-skel-win--win2-switch t)

(defvar ide-skel-win--minibuffer-selected-p nil)

;; (copy-win-node w)
;; (win-node-corner-pos w)
;; (make-win-node :corner-pos 0 :buffer b :horiz-scroll 0 :point 0 :mark nil :divisions nil)
;; (win-node-p w)
(defstruct win-node
  "Window configuration tree node."
  (corner-pos nil)   ; pair - original position of left top window corner
  (buf-corner-pos 1) ; position within the buffer at the upper left of the window
  buffer             ; the buffer window displays
  (horiz-scroll 0)   ; amount of horizontal scrolling, in columns
  (point 1)          ; point
  (mark nil)         ; the mark
  (edges nil)        ; (window-edges)
  (cursor-priority nil) 
  (fixed-size nil)
  (divisions nil))   ; children (list of division)

(defstruct division
  "Podzial okienka"
  win-node      ; winnode for window after division
  horizontal-p  ; division horizontal or vertical
  percent)      ; 0.0-1.0: width/height of parent after division

(defvar sel-window nil)
(defvar sel-priority nil)

(defvar ide-skel-ommited-windows nil)

(defvar ide-skel--fixed-size-windows nil)

;; args: 'left/right 'show/editor-buffer-changed/hide/tab-change &rest buffer...
(defvar ide-skel-side-view-window-functions nil)

(defvar ide-skel-editor-buffer-changed-hook nil)

(defvar ide-skel-last-buffer-change-event nil)
(defvar ide-skel-last-selected-window-or-buffer nil)

(defcustom ide-skel-bottom-view-window-size 0.35
  "Default bottom view window height in characters (int >= 5) or percent of Emacs frame height (0.0 - 1.0)"
  :group 'ide-skel
  :tag "Default Bottom View Window Height"
  :type (list 'restricted-sexp
	      :match-alternatives (list (lambda (value)
					  (or (and (floatp value)
						   (> value 0.0)
						   (< value 1.0))
					      (and (integerp value)
						   (>= value 5)))))))

(defcustom ide-skel-bottom-view-on-left-view t
  "Non-nil if bottom view lies partially on left view."
  :group 'ide-skel
  :tag "Bottom View on Left View"
  :type '(boolean)
  :initialize 'custom-initialize-default
  :set (lambda (symbol value)
	 (let ((is-bottom-view-window (ide-skel-get-bottom-view-window)))
	   (when is-bottom-view-window
	     (ide-skel-hide-bottom-view-window))
	   (unwind-protect
	       (set-default symbol value)
	     (when is-bottom-view-window
	       (ide-skel-show-bottom-view-window))))))

(defcustom ide-skel-bottom-view-on-right-view t
  "Non-nil if bottom view lies partially on right view."
  :group 'ide-skel
  :tag "Bottom View on Right View"
  :type '(boolean)
  :initialize 'custom-initialize-default
  :set (lambda (symbol value)
	 (let ((is-bottom-view-window (ide-skel-get-bottom-view-window)))
	   (when is-bottom-view-window
	     (ide-skel-hide-bottom-view-window))
	   (unwind-protect
	       (set-default symbol value)
	     (when is-bottom-view-window
	       (ide-skel-show-bottom-view-window))))))

(defconst ide-skel-unexpected-bottom-view-window-buffer-names '("*Completions*" "*Compile-Log*"))

(defvar ide-skel--last-bottom-view-buffer-name nil)

(defvar ide-skel-was-scratch nil)

(defvar ide-skel-bottom-view-window-oper-in-progress nil)

(defvar ide-skel--current-side-windows (cons nil nil))

(defcustom ide-skel-left-view-window-width 25
  "Default width of left view window."
  :group 'ide-skel
  :tag "Default Left View Window Width"
  :type '(integer)
  :initialize 'custom-initialize-default
  :set (lambda (symbol value)
	 (let ((is-left-view-window (ide-skel-get-left-view-window)))
	   (when is-left-view-window
	     (ide-skel-hide-left-view-window))
	   (unwind-protect
	       (set-default symbol value)
	     (when is-left-view-window
	       (ide-skel-show-left-view-window))))))

(defcustom ide-skel-right-view-window-width 30
  "Default width of right view window."
  :group 'ide-skel
  :tag "Default Right View Window Width"
  :type '(integer)
  :initialize 'custom-initialize-default
  :set (lambda (symbol value)
	 (let ((is-right-view-window (ide-skel-get-right-view-window)))
	   (when is-right-view-window
	     (ide-skel-hide-right-view-window))
	   (unwind-protect
	       (set-default symbol value)
	     (when is-right-view-window
	       (ide-skel-show-right-view-window))))))

(defcustom ide-skel-side-view-display-cursor nil
  "Non-nil if cursor should be displayed in side view windows"
  :group 'ide-skel
  :tag "Side View Display Cursor"
  :type 'boolean)

(defvar ide-skel-highlight-face 'ide-skel-highlight-face)
(defface ide-skel-highlight-face
  (list 
   (list '((background light))
         (append (list :inherit 'variable-pitch :background (ide-skel-shine-color (face-background 'default) -70) :foreground (face-background 'default))
                 (when (>= emacs-major-version 22) '(:box (:style released-button)))))
   (list '((background dark))
         (append (list :inherit 'variable-pitch :background (ide-skel-shine-color (face-background 'default) +70) :foreground (face-background 'default))
                 (when (>= emacs-major-version 22) '(:box (:style released-button)))))
   '(t (:inherit default)))
  "Face for selection in side views."
  :group 'ide-skel)

;;; buffer -> alist
;;;   :imenu-buffer
;;;   :default-left-tab-label, :default-right-tab-label
(defvar ide-skel-context-properties (make-hash-table :test 'eq))

(defvar ide-skel-last-left-view-window-tab-label nil)
(defvar ide-skel-last-right-view-window-tab-label nil)

(defvar ide-skel-buffer-list-buffer nil)
(defvar ide-skel-buffer-list nil)

(defvar ide-skel-buffer-list-tick nil)

(defconst ide-skel-tree-widget-open-xpm "\
/* XPM */
static char *open[] = {
/* columns rows colors chars-per-pixel */
\"11 15 49 1\",
\"  c #4D084D080B7B\",
\". c #5A705A700DBB\",
\"X c #7B647B6404B5\",
\"o c #7818781810F1\",
\"O c #7E1E7E1E16D4\",
\"+ c #5EB75D2D6FCF\",
\"@ c #5FD85D2D6FCF\",
\"# c #60415D2D6FCF\",
\"$ c #88BD88BD068F\",
\"% c #8A5D8A5D0969\",
\"& c #82F782F71033\",
\"* c #841B841B1157\",
\"= c #87BC87BC1125\",
\"- c #878787871696\",
\"; c #87D587BE172E\",
\": c #87C187C11812\",
\"> c #895A895A1B9C\",
\", c #8A0A8A0A1C10\",
\"< c #8E5B8DF21DE7\",
\"1 c #95DF95DF1A5F\",
\"2 c #95CC95CC1B5B\",
\"3 c #98D498D41EE5\",
\"4 c #9BBB9BBB2414\",
\"5 c #9BBB9BBB2622\",
\"6 c #9CDF9CDF2696\",
\"7 c #984C984C281C\",
\"8 c #9EA19EA129C1\",
\"9 c #A060A0602B4B\",
\"0 c #A3BAA3BA3148\",
\"q c #A78AA78A36FD\",
\"w c #A7BBA7BB38D9\",
\"e c #A7B7A7B73B03\",
\"r c #AB1AAB1A3B03\",
\"t c #ABD7ABD73C6C\",
\"y c #AFC5AFC54435\",
\"u c #B5D2B5D24A67\",
\"i c #B659B6594AEE\",
\"p c #B959B9595378\",
\"a c #BBCEBBCE5267\",
\"s c #BE64BE645A53\",
\"d c #C2D2C2D26078\",
\"f c #C43BC43B60D8\",
\"g c #C42EC42E60EE\",
\"h c #C44FC44F60EC\",
\"j c #C73BC73B66E7\",
\"k c #C65DC65D697B\",
\"l c #CECECECE7676\",
\"z c #D02CD02C7B7B\",
\"x c None\",
/* pixels */
\"xxxxxxxxxxx\",
\"xxxxxxxxxxx\",
\"xxxxxxxxxxx\",
\"xxxxxxxxxxx\",
\"x,> xxxxxxx\",
\"6zlpw07xxxx\",
\"5k32211=oxx\",
\"49ryuasfexx\",
\"$8yuasgdOxx\",
\"%qiashjtxxx\",
\"X&*<;-:.xxx\",
\"xxx@xxxxxxx\",
\"xxx#xxxxxxx\",
\"xxx+xxxxxxx\",
\"xxx+xxxxxxx\"
};
")

(defconst ide-skel-tree-widget-open-image
  (create-image ide-skel-tree-widget-open-xpm 'xpm t))

(defconst ide-skel-tree-widget-no-handle-xpm "\
/* XPM */
static char *no_handle[] = {
/* columns rows colors chars-per-pixel */
\"7 15 1 1\",
\"  c None\",
/* pixels */
\"       \",
\"       \",
\"       \",
\"       \",
\"       \",
\"       \",
\"       \",
\"       \",
\"       \",
\"       \",
\"       \",
\"       \",
\"       \",
\"       \",
\"       \"
};
")

(defconst ide-skel-tree-widget-no-handle-image
  (create-image ide-skel-tree-widget-no-handle-xpm 'xpm t))

(defconst ide-skel-tree-widget-no-guide-xpm "\
/* XPM */
static char *no_guide[] = {
/* columns rows colors chars-per-pixel */
\"4 15 1 1\",
\"  c None\",
/* pixels */
\"    \",
\"    \",
\"    \",
\"    \",
\"    \",
\"    \",
\"    \",
\"    \",
\"    \",
\"    \",
\"    \",
\"    \",
\"    \",
\"    \",
\"    \"
};
")

(defconst ide-skel-tree-widget-no-guide-image
  (create-image ide-skel-tree-widget-no-guide-xpm 'xpm t))

(defconst ide-skel-tree-widget-leaf-xpm "\
/* XPM */
static char *leaf[] = {
/* columns rows colors chars-per-pixel */
\"11 15 42 1\",
\"  c #224222422242\",
\". c #254525452545\",
\"X c #272727272727\",
\"o c #31DA31DA31DA\",
\"O c #4CAC4CAC4CAC\",
\"+ c #4F064F064F06\",
\"@ c #506050605060\",
\"# c #511651165116\",
\"$ c #57D657D657D6\",
\"% c #59A559A559A5\",
\"& c #5AAC5AAC5AAC\",
\"* c #5D5A5D5A5D5A\",
\"= c #5F025F025F02\",
\"- c #60C660C660C6\",
\"; c #617D617D617D\",
\": c #63D363D363D3\",
\"> c #8B908B908B90\",
\", c #8E3C8E3C8E3C\",
\"< c #8F588F588F58\",
\"1 c #93FC93FC93FC\",
\"2 c #949194919491\",
\"3 c #96AD96AD96AD\",
\"4 c #991899189918\",
\"5 c #99EA99EA99EA\",
\"6 c #9B619B619B61\",
\"7 c #9CD69CD69CD6\",
\"8 c #9E769E769E76\",
\"9 c #9FA59FA59FA5\",
\"0 c #A0C3A0C3A0C3\",
\"q c #A293A293A293\",
\"w c #A32EA32EA32E\",
\"e c #A480A480A480\",
\"r c #A5A5A5A5A5A5\",
\"t c #A755A755A755\",
\"y c #AA39AA39AA39\",
\"u c #AC77AC77AC77\",
\"i c #B1B7B1B7B1B7\",
\"p c #B283B283B283\",
\"a c #B7B7B7B7B7B7\",
\"s c #BD02BD02BD02\",
\"d c gray74\",
\"f c None\",
/* pixels */
\"fffffffffff\",
\"fffffffffff\",
\"fffffffffff\",
\"XXXXfffffff\",
\"%,25#offfff\",
\"*6qr$&.ffff\",
\"=1<3>wOffff\",
\";6648a@ffff\",
\";wweys#ffff\",
\":970ed#ffff\",
\"-tuipp+ffff\",
\"XXXXXX ffff\",
\"fffffffffff\",
\"fffffffffff\",
\"fffffffffff\"
};
")

(defconst ide-skel-tree-widget-leaf-image
  (create-image ide-skel-tree-widget-leaf-xpm 'xpm t))

(defconst ide-skel-tree-widget-handle-xpm "\
/* XPM */
static char *handle[] = {
/* columns rows colors chars-per-pixel */
\"7 15 2 1\",
\"  c #56D752D36363\",
\". c None\",
/* pixels */
\".......\",
\".......\",
\".......\",
\".......\",
\".......\",
\".......\",
\".......\",
\"       \",
\".......\",
\".......\",
\".......\",
\".......\",
\".......\",
\".......\",
\".......\"
};
")

(defconst ide-skel-tree-widget-handle-image
  (create-image ide-skel-tree-widget-handle-xpm 'xpm t))

(defconst ide-skel-tree-widget-guide-xpm "\
/* XPM */
static char *guide[] = {
/* columns rows colors chars-per-pixel */
\"4 15 2 1\",
\"  c #73C96E6E8484\",
\". c None\",
/* pixels */
\"... \",
\"... \",
\"... \",
\"... \",
\"... \",
\"... \",
\"... \",
\"... \",
\"... \",
\"... \",
\"... \",
\"... \",
\"... \",
\"... \",
\"... \"
};
")

(defconst ide-skel-tree-widget-guide-image
  (create-image ide-skel-tree-widget-guide-xpm 'xpm t))

(defconst ide-skel-tree-widget-end-guide-xpm "\
/* XPM */
static char *end_guide[] = {
/* columns rows colors chars-per-pixel */
\"4 15 2 1\",
\"  c #73C96E6E8484\",
\". c None\",
/* pixels */
\"... \",
\"... \",
\"... \",
\"... \",
\"... \",
\"... \",
\"... \",
\"... \",
\"....\",
\"....\",
\"....\",
\"....\",
\"....\",
\"....\",
\"....\"
};
")

(defconst ide-skel-tree-widget-end-guide-image
  (create-image ide-skel-tree-widget-end-guide-xpm 'xpm t))

(defconst ide-skel-tree-widget-empty-xpm "\
/* XPM */
static char *empty[] = {
/* columns rows colors chars-per-pixel */
\"11 15 39 1\",
\"  c #2BCF2BCF2BCF\",
\". c #31F831F831F8\",
\"X c #3F283F283F28\",
\"o c #41B141B141B1\",
\"O c #467946794679\",
\"+ c #476747674767\",
\"@ c #484648464846\",
\"# c #498749874987\",
\"$ c #4B684B684B68\",
\"% c #524F524F524F\",
\"& c #52D352D352D3\",
\"* c #554155415541\",
\"= c #561C561C561C\",
\"- c #598659865986\",
\"; c #5D775D775D77\",
\": c #5E7E5E7E5E7E\",
\"> c #60CE60CE60CE\",
\", c #615161516151\",
\"< c #61F361F361F3\",
\"1 c #642464246424\",
\"2 c #654865486548\",
\"3 c #678767876787\",
\"4 c #68D868D868D8\",
\"5 c #699569956995\",
\"6 c #6D556D556D55\",
\"7 c #6FB56FB56FB5\",
\"8 c #72CF72CF72CF\",
\"9 c #731073107310\",
\"0 c #757775777577\",
\"q c #7B747B747B74\",
\"w c #809080908090\",
\"e c #81F281F281F2\",
\"r c #820D820D820D\",
\"t c #84F984F984F9\",
\"y c #858285828582\",
\"u c #95E295E295E2\",
\"i c #9FFF9FFF9FFF\",
\"p c #A5A5A5A5A5A5\",
\"a c None\",
/* pixels */
\"aaaaaaaaaaa\",
\"aaaaaaaaaaa\",
\"aaaaaaaaaaa\",
\"aaaaaaaaaaa\",
\"a&% aaaaaaa\",
\",piy76<aaaa\",
\">u-===*#oaa\",
\":14690qe3aa\",
\"+;680qewOaa\",
\"@290qrt5aaa\",
\"XO+@#$$.aaa\",
\"aaaaaaaaaaa\",
\"aaaaaaaaaaa\",
\"aaaaaaaaaaa\",
\"aaaaaaaaaaa\"
};
")

(defconst ide-skel-tree-widget-empty-image
  (create-image ide-skel-tree-widget-empty-xpm 'xpm t))

(defconst ide-skel-tree-widget-close-xpm "\
/* XPM */
static char *close[] = {
/* columns rows colors chars-per-pixel */
\"11 15 45 1\",
\"  c #4EA14EA10DFA\",
\". c #5AA05AA00C52\",
\"X c #75297529068F\",
\"o c #7B647B6404B5\",
\"O c #8B888B880B91\",
\"+ c #8EDE8EDE0F5F\",
\"@ c #82F782F71033\",
\"# c #83A683A61157\",
\"$ c #84AD84AD13BC\",
\"% c #857985791489\",
\"& c #868086801590\",
\"* c #8A8A8A8A1697\",
\"= c #878787871812\",
\"- c #885388531936\",
\"; c #8BAB8BAB17B8\",
\": c #8CCC8CCC1A7D\",
\"> c #8DB68DB61BC4\",
\", c #90EC90EC11D0\",
\"< c #9161916114B5\",
\"1 c #92A292A2163F\",
\"2 c #8E8B8E8B2150\",
\"3 c #8F0F8F0F2274\",
\"4 c #9AF79AF72386\",
\"5 c #9D289D282655\",
\"6 c #9ED19ED1286E\",
\"7 c #9F599F592912\",
\"8 c #A31DA31D2D82\",
\"9 c #A3DDA3DD2DA2\",
\"0 c #A144A1442ED2\",
\"q c #A828A82833B4\",
\"w c #AB38AB383AEB\",
\"e c #AD21AD213DC2\",
\"r c #AD6DAD6D3E56\",
\"t c #AFFCAFFC4481\",
\"y c #B0AAB0AA429F\",
\"u c #B1B1B1B144E8\",
\"i c #B51DB51D4A5F\",
\"p c #B535B5354A8A\",
\"a c #B56FB56F4AEE\",
\"s c #B7B0B7B0525B\",
\"d c #BD14BD1459B1\",
\"f c #BFACBFAC5C55\",
\"g c #C5D9C5D965F7\",
\"h c #C85FC85F6D04\",
\"j c None\",
/* pixels */
\"jjjjjjjjjjj\",
\"jjjjjjjjjjj\",
\"jjjjjjjjjjj\",
\"jjjjjjjjjjj\",
\"j32 jjjjjjj\",
\"1uy84570.jj\",
\"O69wtpsd*jj\",
\"+qrtpsdf;jj\",
\",etisdfg:jj\",
\"<tasdfgh>jj\",
\"o@#$%&=-Xjj\",
\"jjjjjjjjjjj\",
\"jjjjjjjjjjj\",
\"jjjjjjjjjjj\",
\"jjjjjjjjjjj\"
};
")

(defconst ide-skel-tree-widget-close-image
  (create-image ide-skel-tree-widget-close-xpm 'xpm t))

(define-widget 'ide-skel-imenu-internal-node-widget 'tree-widget
  "Internal node widget.")

(define-widget 'ide-skel-imenu-leaf-widget 'push-button
  "Leaf widget."
  :format        "%[%t%]\n"
  :button-face   'variable-pitch
  )

(defvar ide-skel-imenu-sorted nil)
(make-variable-buffer-local 'ide-skel-imenu-sorted)

(defvar ide-skel-imenu-editor-buffer nil)
(make-variable-buffer-local 'ide-skel-imenu-editor-buffer)

(defvar ide-skel-imenu-open-paths nil)
(make-variable-buffer-local 'ide-skel-imenu-open-paths)

(defface imenu-side-view-face '((t :inherit variable-pitch :height 0.8))
  "Default face used in right view for imenu"
  :group 'ide-skel)

(define-widget 'ide-skel-info-tree-dir-widget 'tree-widget
  "Directory Tree widget."
  :expander 'ide-skel-info-tree-expand-dir
  :notify 'ide-skel-info-open
  :indent   0)

(define-widget 'ide-skel-info-tree-file-widget 'push-button
  "File widget."
  :format        "%[%t%]%d\n"
  :button-face   'variable-pitch
  :notify        'ide-skel-info-file-open)

(defvar ide-skel-info-open-paths nil)
(make-variable-buffer-local 'ide-skel-info-open-paths)

(defvar ide-skel-info-root-node nil)
(make-variable-buffer-local 'ide-skel-info-root-node)

(defvar ide-skel-info-buffer nil)

(define-widget 'ide-skel-dir-tree-dir-widget 'tree-widget
  "Directory Tree widget."
  :expander 'ide-skel-dir-tree-expand-dir
  :notify 'ide-skel-dir-open
  :indent   0)

(define-widget 'ide-skel-dir-tree-file-widget 'push-button
  "File widget."
  :format        "%[%t%]%d\n"
  :button-face   'variable-pitch
  :notify        'ide-skel-file-open)

(defvar ide-skel-dir-open-paths nil)
(make-variable-buffer-local 'ide-skel-dir-open-paths)

(defvar ide-skel-dir-root-dir "/")
(make-variable-buffer-local 'ide-skel-dir-root-dir)

(defvar ide-skel-dir-buffer nil)

(defconst ide-skel-cvs-dir-regexp "\\(\\.svn\\|CVS\\)$")

(defstruct ide-skel-project
  root-path
  include-file-path ; for PC-include-file-path variable
)

(defvar ide-skel-projects nil)

(defvar ide-skel-proj-find-results-buffer-name "*Proj find*")

(defvar ide-skel-project-menu
  '("Project"
    :filter ide-skel-project-menu)
  "Menu for CVS/SVN projects")

(defvar ide-skel-proj-find-project-files-history nil)
(defvar ide-skel-proj-grep-project-files-history nil)

(defvar ide-skel-proj-ignored-extensions '("semantic.cache"))

(defvar ide-skel-all-text-files-flag nil)

(defvar ide-skel-proj-grep-header nil)

(defvar ide-skel-proj-old-compilation-exit-message-function nil)
(make-variable-buffer-local 'ide-skel-proj-old-compilation-exit-message-function)

(defvar ide-skel-proj-grep-mode-map nil)

(defvar ide-skel-proj-grep-replace-history nil)

;;;

(copy-face 'mode-line 'mode-line-inactive)

(define-key tree-widget-button-keymap [drag-mouse-1] 'ignore)

(defun ide-skel-tabbar-tab-label (tab)
  "Return a label for TAB.
That is, a string used to represent it on the tab bar."
  (let* ((object (tabbar-tab-value tab))
	 (tabset (tabbar-tab-tabset tab))
	 (label (format " %s "
			(or (and (bufferp object)
				 (with-current-buffer object ide-skel-tabbar-tab-label)) ; local in buffer
			    object))))
    (when (and (not (memq tabset (list (tabbar-get-tabset ide-skel-left-view-window-tabset-name)
				       (tabbar-get-tabset ide-skel-right-view-window-tabset-name))))
	       (numberp ide-skel-tabbar-tab-label-max-width)
	       (> ide-skel-tabbar-tab-label-max-width 0))
      (setq label (tabbar-shorten label ide-skel-tabbar-tab-label-max-width)))
    label))

(defun ide-skel-tabbar-help-on-tab (tab)
  "Return the help string shown when mouse is onto TAB."
  (let ((tabset (tabbar-tab-tabset tab))
	(object (tabbar-tab-value tab)))
    (or (when (bufferp object)
	  (with-current-buffer object
	    (or ide-skel-tabbar-tab-help-string ; local in buffer
		(buffer-file-name))))
	"mouse-1: switch to buffer\nmouse-2: delete other windows\nmouse-3: kill buffer")))

(defun ide-skel-tabbar-buffer-groups ()
  "Return the list of group names the current buffer belongs to."
   (if (and (ide-skel-side-view-buffer-p (current-buffer))
	    (or (not ide-skel-tabbar-tab-label)
		(not ide-skel-tabbar-enabled)))
       nil
    (let ((result (list (or ide-skel-tabset-name ; local in current buffer
			    (when (ide-skel-bottom-view-buffer-p (current-buffer)) ide-skel-bottom-view-window-tabset-name)
			    ide-skel-editor-window-tabset-name))))
      (dolist (window (copy-list (window-list nil 1)))
	(when (eq (window-buffer window) (current-buffer))
	  (let ((tabset-name (ide-skel-get-tabset-name-for-window window)))
	    (unless (member tabset-name result)
	      (push tabset-name result)))))
      result)))

(defun ide-skel-tabbar-buffer-tabs ()
  "Return the buffers to display on the tab bar, in a tab set."
  ;; (message "ide-skel-tabbar-buffer-tabs %S" (current-buffer))
  (tabbar-buffer-update-groups)
  (let* ((window (selected-window))
	 (tabset (tabbar-get-tabset (ide-skel-get-tabset-name-for-window window))))
     (when (not (tabbar-get-tab (current-buffer) tabset))
       (tabbar-add-tab tabset (current-buffer) t))
    (tabbar-select-tab-value (current-buffer) tabset)
    tabset))

(defun ide-skel-tabbar-buffer-list ()
  "Return the list of buffers to show in tabs.
The current buffer is always included."
  (ide-skel-tabbar-faces-adapt)
  (delq t
        (mapcar #'(lambda (b)
		    (let ((buffer-name (buffer-name b)))
		      (cond
		       ((and (ide-skel-side-view-buffer-p b) 
			     (with-current-buffer b
			       (or (not ide-skel-tabbar-tab-label)
				   (not ide-skel-tabbar-enabled))))
			t)
		       ;; Always include the current buffer.
		       ((eq (current-buffer) b) b)
		       ;; accept if buffer has tabset name
		       ((with-current-buffer b ide-skel-tabset-name) b)
		       ;; remove if matches any regexp from ide-skel-tabbar-hidden-buffer-names-regexp-list
		       ((not (null (some (lambda (regexp)
					   (string-match regexp buffer-name))
					 ide-skel-tabbar-hidden-buffer-names-regexp-list)))
			t)
		       ;; accept if buffer has filename
		       ((buffer-file-name b) b)
		       ;; remove if name starts with space
		       ((and (char-equal ?\  (aref (buffer-name b) 0))
			     (not (ide-skel-side-view-buffer-p b)))
			t)
		       ;; accept otherwise
		       (b))))
		(buffer-list (selected-frame)))))

(defun ide-skel-get-tabset-name-for-window (window)
  (cond ((eq (ide-skel-get-left-view-window) window) ide-skel-left-view-window-tabset-name)
	((eq (ide-skel-get-right-view-window) window) ide-skel-right-view-window-tabset-name)
        ((eq (ide-skel-get-bottom-view-window) window) ide-skel-bottom-view-window-tabset-name)
	(t ide-skel-editor-window-tabset-name)))

(defun ide-skel-tabbar-select-tab (event tab)
  "On mouse EVENT, select TAB."
  (let* ((mouse-button (event-basic-type event))
	 (buffer (tabbar-tab-value tab))
	 (tabset-name (and (buffer-live-p buffer)
			   (with-current-buffer buffer ide-skel-tabset-name)))
	 (left-tabset (equal tabset-name ide-skel-left-view-window-tabset-name))
	 (right-tabset (equal tabset-name ide-skel-right-view-window-tabset-name)))
    (cond
     ((eq mouse-button 'mouse-1)
      (cond (left-tabset (ide-skel-side-window-switch-to-buffer ide-skel-current-left-view-window buffer))
	    (right-tabset (ide-skel-side-window-switch-to-buffer ide-skel-current-right-view-window buffer))
	    (t (switch-to-buffer buffer))))
     ((and (eq mouse-button 'mouse-2)
	   (not left-tabset)
	   (not right-tabset))
      (switch-to-buffer buffer)
      (delete-other-windows))
     ((and (eq mouse-button 'mouse-3)
	   (not left-tabset)
	   (not right-tabset))
      (kill-buffer buffer)))
    ;; Disable group mode.
    (set 'tabbar-buffer-group-mode nil)))

(defun ide-skel-tabbar-buffer-kill-buffer-hook ()
  "Hook run just before actually killing a buffer.
In Tabbar mode, try to switch to a buffer in the current tab bar,
after the current buffer has been killed.  Try first the buffer in tab
after the current one, then the buffer in tab before.  On success, put
the sibling buffer in front of the buffer list, so it will be selected
first."
  (let ((buffer-to-kill (current-buffer)))
    (save-selected-window
      (save-current-buffer
	;; cannot kill buffer from any side view window
	(when (and (eq header-line-format tabbar-header-line-format)
		   (not (ide-skel-side-view-buffer-p (current-buffer))))
          (dolist (window (copy-list (window-list nil 1)))
            (when (eq buffer-to-kill (window-buffer window))
              (select-window window)
              (let ((bl (tabbar-tab-values (funcall tabbar-current-tabset-function)))
                    found sibling)
                (while (and bl (not found))
                  (if (equal buffer-to-kill (car bl))
                      (setq found t)
                    (setq sibling (car bl)))
                  (setq bl (cdr bl)))
		(setq sibling (or sibling (car bl)))
                (if (and sibling
			 (not (eq sibling buffer-to-kill))
			 (buffer-live-p sibling))
                    ;; Move sibling buffer in front of the buffer list.
		    (switch-to-buffer sibling)
                  (let ((next-buffer (ide-skel--find-buffer-for-bottom-view-window)))
                    (when (eq next-buffer buffer-to-kill)
                      (setq next-buffer (some (lambda (buf)
                                                (if (or (eq buf buffer-to-kill)
							(ide-skel-side-view-buffer-p buf)
                                                        (ide-skel-hidden-buffer-name-p (buffer-name buf)))
                                                    nil
                                                  buf))
                                              (buffer-list (selected-frame)))))
                    (when next-buffer
                      (switch-to-buffer next-buffer)
                      (tabbar-current-tabset t))))))))))))

(defun ide-skel-tabbar-inhibit-function ()
  "Inhibit display of the tab bar in specified windows, that is
in `checkdoc' status windows and in windows with its own header
line."
  (let ((result (tabbar-default-inhibit-function))
	(sw (selected-window)))
    (when (and result
	       (ide-skel-side-view-window-p sw))
      (setq result nil))
    (when (not (eq header-line-format tabbar-header-line-format))
      (setq result t))
    result))

(defun ide-skel-tabbar-home-function (event)
  (let* ((window (posn-window (event-start event)))
	 (is-view-window (ide-skel-side-view-window-p window))
	 (buffer (window-buffer window))
	 extra-commands
	 (normal-window-counter 0))
    (dolist (win (copy-list (window-list nil 1)))
      (unless (ide-skel-side-view-window-p win)
	(incf normal-window-counter)))
    (with-selected-window window
      (when (and is-view-window
		 ide-skel-tabbar-menu-function)
	(setq extra-commands (funcall ide-skel-tabbar-menu-function)))
      (let ((close-p (when (or is-view-window
			       (> normal-window-counter 1))
		       (list '(close "Close" t))))
	    (maximize-p (when (and (not is-view-window)
				   (> normal-window-counter 1))
			  (list '(maximize "Maximize" t)))))
	(when (or close-p maximize-p)
	  (let ((user-selection
		 (car (x-popup-menu event (append (list 'keymap) close-p maximize-p extra-commands)))))
	    (cond ((eq user-selection 'close)
		   (call-interactively 'delete-window))
		  ((eq user-selection 'maximize)
		   (delete-other-windows window))
		  ((eq user-selection nil))
		  (t
		   (funcall user-selection)))))))))

(defun ide-skel-tabbar-mwheel-scroll-forward (event)
  (interactive "@e")
  (tabbar-press-scroll-left))

(defun ide-skel-tabbar-mwheel-scroll-backward (event)
  (interactive "@e")
  (tabbar-press-scroll-right))

(defun ide-skel-tabbar-mwheel-scroll (event)
  "Select the next or previous group of tabs according to EVENT."
  (interactive "@e")
  (if (tabbar--mwheel-up-p event)
      (ide-skel-tabbar-mwheel-scroll-forward event)
    (ide-skel-tabbar-mwheel-scroll-backward event)))

(defun ide-skel-tabbar-mwhell-mode-hook ()
  (setq tabbar-mwheel-mode-map 
	(let ((km (make-sparse-keymap)))
	  (if (get 'mouse-wheel 'event-symbol-elements)
	      ;; Use one generic mouse wheel event
	      (define-key km [A-mouse-wheel]
		'ide-skel-tabbar-mwheel-scroll)
	    ;; Use separate up/down mouse wheel events
	    (let ((up   (tabbar--mwheel-key tabbar--mwheel-up-event))
		  (down (tabbar--mwheel-key tabbar--mwheel-down-event)))
	      (define-key km `[header-line ,down]
		'ide-skel-tabbar-mwheel-scroll-backward)
	      (define-key km `[header-line ,up]
		'ide-skel-tabbar-mwheel-scroll-forward)
	      ))
	  km))
  (setcdr (assoc 'tabbar-mwheel-mode minor-mode-map-alist) tabbar-mwheel-mode-map))

(defun ide-skel-tabbar-mode-hook ()
  (setq tabbar-prefix-map
	(let ((km (make-sparse-keymap)))
	  (define-key km [(control home)]  'tabbar-press-home)
	  (define-key km [(control left)]  'tabbar-backward)
	  (define-key km [(control right)] 'tabbar-forward)
	  (define-key km [(control prior)] 'tabbar-press-scroll-left)
	  (define-key km [(control next)]  'tabbar-press-scroll-right)
	  km))
  (setq tabbar-mode-map
	(let ((km (make-sparse-keymap)))
	  (define-key km tabbar-prefix-key tabbar-prefix-map)
	  km))
  (setcdr (assoc 'tabbar-mode minor-mode-map-alist) tabbar-mode-map))

(defun ide-skel-tabbar-init-hook ()
  (setq tabbar-cycle-scope 'tabs
	tabbar-auto-scroll-flag nil)
  (setq
   tabbar-tab-label-function 'ide-skel-tabbar-tab-label
   tabbar-help-on-tab-function 'ide-skel-tabbar-help-on-tab
   tabbar-buffer-groups-function 'ide-skel-tabbar-buffer-groups
   tabbar-buffer-list-function 'ide-skel-tabbar-buffer-list
   tabbar-current-tabset-function 'ide-skel-tabbar-buffer-tabs
   tabbar-select-tab-function 'ide-skel-tabbar-select-tab
   tabbar-inhibit-functions (append '(ide-skel-tabbar-inhibit-function) 
				    (delq 'tabbar-default-inhibit-function tabbar-inhibit-functions))
   tabbar-home-function 'ide-skel-tabbar-home-function
   tabbar-home-help-function (lambda () "Window menu"))
  (add-hook 'kill-buffer-hook 'ide-skel-tabbar-buffer-kill-buffer-hook))

(defun ide-skel-tabbar-quit-hook ()
  (setq
   tabbar-current-tabset-function nil
   tabbar-tab-label-function nil
   tabbar-select-tab-function nil
   tabbar-help-on-tab-function nil
   tabbar-home-function nil
   tabbar-home-help-function nil
   tabbar-buffer-groups-function nil
   tabbar-buffer-list-function nil)
  (remove-hook 'kill-buffer-hook 'ide-skel-tabbar-buffer-kill-buffer-hook))

(defun ide-skel-tabbar-load-hook ()
  (add-hook 'tabbar-mode-hook 'ide-skel-tabbar-mode-hook)
  (add-hook 'tabbar-mwheel-mode-hook 'ide-skel-tabbar-mwhell-mode-hook)
  (add-hook 'tabbar-init-hook 'ide-skel-tabbar-init-hook t)
  (add-hook 'tabbar-quit-hook 'ide-skel-tabbar-quit-hook t)
  (custom-set-faces
   '(tabbar-default ((t (:inherit variable-pitch :background "gray82" :foreground "gray50" :height 0.8))))
   '(tabbar-selected ((t (:inherit tabbar-default :background "white" :foreground "blue" :box (:line-width 1 :color "black")))))
   '(tabbar-separator ((t (:inherit tabbar-default :height 0.2))))
   '(tabbar-highlight ((t ())))
   '(tabbar-button-highlight ((t (:inherit tabbar-button))))
   '(tabbar-unselected ((t (:inherit tabbar-default :background "gray72" :foreground "black" :box (:line-width 1 :color "black"))))))
  (ide-skel-tabbar-faces-adapt))
  
(defun ide-skel-tabbar-faces-adapt ()
  (ide-skel-shine-face-background 'tabbar-default +18)
  (set-face-attribute 'tabbar-selected nil :background (face-background 'default))
  (set-face-attribute 'tabbar-selected nil :foreground (face-foreground 'font-lock-function-name-face))
  (set-face-attribute 'tabbar-selected nil :box (list :line-width 1 :color (face-foreground 'default)))
  (ide-skel-shine-face-background 'tabbar-unselected +30)
  (set-face-attribute 'tabbar-unselected nil :foreground (face-foreground 'default))
  (set-face-attribute 'tabbar-unselected nil :box (list :line-width 1 :color (face-foreground 'default)))
  (ide-skel-shine-face-background 'tabbar-button +18)
  (ide-skel-shine-face-foreground 'tabbar-button +20))

(defun ide-skel-paradox-settings ()
  ;; hide scroll buttons
  (setq tabbar-scroll-left-button (cons (cons "" nil) (cons "" nil))
	tabbar-scroll-right-button (cons (cons "" nil) (cons "" nil))))

(ide-skel-paradox-settings)


;;; Views

(defun ide-skel-window-list ()
  (delq nil 
	(mapcar (lambda (win)
		  (unless (memq win ide-skel-ommited-windows)
		    win))
		(copy-list (window-list nil 1)))))

(defun ide-skel-next-window (&optional window minibuf all-frames)
  (let ((nw (next-window window minibuf all-frames)))
    (if (memq nw ide-skel-ommited-windows)
	(ide-skel-next-window nw minibuf all-frames)
      nw)))

(defun ide-skel-previous-window (window minibuf all-frames)
  (let ((pw (previous-window window minibuf all-frames)))
    (if (memq pw ide-skel-ommited-windows)
	window
      pw)))

(defun ide-skel-win--absorb-win-node (dest-win-node src-win-node)
  (dotimes (index (length src-win-node))
    (setf (elt dest-win-node index)
	  (elt src-win-node index))))

(defun ide-skel-win--create-win-node (object)
  (cond ((win-node-p object) (copy-win-node object))
	((windowp object)
	 (make-win-node :corner-pos (ide-skel-win-corner object)
			:buf-corner-pos (window-start object)
			:buffer (window-buffer object)
			:horiz-scroll (window-hscroll object)
			:point (window-point object)
			:mark nil
			:edges (window-edges object)
			:fixed-size (cdr (assoc (ide-skel-win-corner object) ide-skel--fixed-size-windows))
			:divisions nil))
	(t (error "Argument is not win-not nor window: %S" object))))

(defun ide-skel-win--get-corner-pos (object)
  (cond ((windowp object) (ide-skel-win-corner object))
	((win-node-p object) (win-node-corner-pos object))
	((consp object) object)
	(t (error "Invalid arg: %S" object))))

(defun ide-skel-win--corner-pos-equal (win-node1 win-node2)
  (let ((corner-pos1 (ide-skel-win--get-corner-pos win-node1))
	(corner-pos2 (ide-skel-win--get-corner-pos win-node2)))
    (equal corner-pos1 corner-pos2)))

(defun ide-skel-win--add-division (win-node division &optional at-end-p)
  (setf (win-node-divisions win-node)
	(if at-end-p 
	    (reverse (cons division (reverse (win-node-divisions win-node))))
	  (cons division (win-node-divisions win-node)))))
      
(defun ide-skel-win--remove-division (win-node &optional from-end-p)
  (let (result)
    (if from-end-p
	(let ((divs (reverse (win-node-divisions win-node))))
	  (setq result (car divs))
	  (setf (win-node-divisions win-node)
		(reverse (cdr divs))))
      (setq result (car (win-node-divisions win-node)))
      (setf (win-node-divisions win-node) (cdr (win-node-divisions win-node))))
    result))

(defun ide-skel-win--find-node (root predicate)
  "Return node for which predicate returns non-nil."
  (when root
    (if	(funcall predicate root)
	root
      (some (lambda (division)
	      (ide-skel-win--find-node (division-win-node division) predicate))
	    (win-node-divisions root)))))

(defun ide-skel-win--find-node-by-corner-pos (root corner-pos)
  "Return struct for window with specified corner coordinates."
  (setq corner-pos
	(cond ((windowp corner-pos) (ide-skel-win-corner corner-pos))
	      ((consp corner-pos) corner-pos)
	      (t (error "arg corner-pos %S is not a pair/window" corner-pos))))
  (ide-skel-win--find-node root
		      (lambda (win-node)
			(equal corner-pos (win-node-corner-pos win-node)))))

(defun ide-skel-win--get-window-list ()
  (let* ((start-win (selected-window))
	 (cur-win (ide-skel-next-window start-win 1 1))
	 (win-list (list start-win)))
    (while (not (eq cur-win start-win))
      (setq win-list (cons cur-win win-list))
      (setq cur-win (ide-skel-next-window cur-win 1 1)))
    (reverse win-list)))

(defun ide-skel-win--analysis (&optional window-proc)
  ;; (message "ide-skel-win--analysis BEGIN %S" (get-internal-run-time))
  (let ((window-size-fixed nil))
    (setq ide-skel--fixed-size-windows nil)
    (dolist (window (copy-list (window-list nil 1)))
      (with-selected-window window
	(cond ((eq window-size-fixed 'width)
	       (push (cons (ide-skel-win-corner window) (cons (window-width window) nil)) ide-skel--fixed-size-windows))
	      ((eq window-size-fixed 'height)
	       (push (cons (ide-skel-win-corner window) (cons nil (window-height window))) ide-skel--fixed-size-windows))
	      ((not window-size-fixed)
	       nil)
	      (t
	       (push (cons (ide-skel-win-corner window) (cons (window-width window) (window-height window))) ide-skel--fixed-size-windows)))))
    (dolist (window (ide-skel-window-list))
      (when (ide-skel-side-view-window-p window) (set-window-dedicated-p window nil)))
    (setq ide-skel-win--minibuffer-selected-p (eq (selected-window) (minibuffer-window)))
    (when ide-skel-win--minibuffer-selected-p
      (select-window (ide-skel-get-editor-window)))
    (when (memq (selected-window) ide-skel-ommited-windows)
      (select-window (ide-skel-next-window (selected-window) 1 1)))
    (let* (leaf-win
	   (counter 0)
	   (cursor-alist (mapcar (lambda (win) (prog1 (cons win counter) (incf counter))) (ide-skel-win--get-window-list)))
	   win-node-set)
      (select-window (ide-skel-win-get-upper-left-window))
      (while (setq leaf-win (get-window-with-predicate
			     (lambda (win)
			       (if ide-skel-win--win2-switch (ide-skel-win--is-leaf2 win) (ide-skel-win--is-leaf win))) 1 1))
	(let* ((parent-win (ide-skel-previous-window leaf-win 1 1))
	       (parent-node (car (member* (ide-skel-win-corner parent-win) win-node-set :test 'ide-skel-win--corner-pos-equal)))
	       (leaf-node   (car (member* (ide-skel-win-corner leaf-win) win-node-set :test 'ide-skel-win--corner-pos-equal))))
	  (unless leaf-node
	    (setq leaf-node (ide-skel-win--create-win-node leaf-win))
	    (setf (win-node-cursor-priority leaf-node) (cdr (assq leaf-win cursor-alist)))
	    (setq win-node-set (adjoin leaf-node win-node-set :test 'ide-skel-win--corner-pos-equal)))
	  (unless parent-node
	    (setq parent-node (ide-skel-win--create-win-node parent-win))
	    (setf (win-node-cursor-priority parent-node) (cdr (assq parent-win cursor-alist)))
	    (setq win-node-set (adjoin parent-node win-node-set :test 'ide-skel-win--corner-pos-equal)))

	  (let* ((is-horizontal (ide-skel-win--is-adjacent parent-win 'right leaf-win))
		 (size (if is-horizontal (window-width parent-win) (window-height parent-win)))
		 percent)
	    (setf (win-node-edges leaf-node) (window-edges leaf-win))
	    (when window-proc (funcall window-proc parent-win))
	    (when window-proc (funcall window-proc leaf-win))
	    (delete-window leaf-win)
	    (when window-proc (funcall window-proc parent-win))
	    (setq percent
		  (/ (float size) (if is-horizontal (window-width parent-win) (window-height parent-win))))
	    (ide-skel-win--add-division parent-node
					(make-division :win-node leaf-node
						       :horizontal-p is-horizontal
						       :percent percent)))))
      ;; if there was only one window
      (unless win-node-set
	(when window-proc (funcall window-proc (selected-window)))
	(let ((node (ide-skel-win--create-win-node (selected-window))))
	  (setq win-node-set (adjoin node win-node-set
				     :test 'ide-skel-win--corner-pos-equal))))
      ;; return root node
      (let ((root-node (car (member* (ide-skel-win-corner (selected-window))
				     win-node-set
				     :test 'ide-skel-win--corner-pos-equal))))
	(setf (win-node-edges root-node) (window-edges (selected-window)))
	;; (message "ide-skel-win--analysis END %S" (get-internal-run-time))
	root-node))))

(defun ide-skel-win-get-upper-left-window ()
  "Return window in left upper corner"
  (let (best-window)
    (dolist (win (ide-skel-window-list))
      (if (null best-window)
          (setq best-window win)
        (let* ((best-window-coords (window-edges best-window))
               (best-window-weight (+ (car best-window-coords) (cadr best-window-coords)))
               (win-coords (window-edges win))
               (win-weight (+ (car win-coords) (cadr win-coords))))
          (when (< win-weight best-window-weight)
            (setq best-window win)))))
    best-window))

(defun ide--is-right-window (window)
  (let ((bounds (window-edges window))
	(result t))
    (dolist (win (ide-skel-window-list))
      (let ((left-edge-pos (car (window-edges win))))
        (when (>= left-edge-pos (nth 2 bounds))
          (setq result nil))))
    result))

(defun ide-skel-get-win-width-delta (window)
  (if window-system
      (let ((bounds (window-edges window)))
	(+ (- (- (nth 2 bounds) (nth 0 bounds)) (window-width window))
	   (if (and (not scroll-bar-mode)
			 (ide--is-right-window window))
	       1
	     0)))
    1))

(defun ide-skel-win--split (window horizontal-p percentage)
  "Split window and return children."
  (let* ((delta (ide-skel-get-win-width-delta window))
	 (weight percentage)
	 (new-size (cond
		    ((integerp weight) (if (< weight 0) 
					   (if horizontal-p
					       (+ (window-width window) weight)
					     (+ (window-height window) weight))
					 (if horizontal-p (+ delta weight) weight)))
		    (t			; float
		     (when (< weight 0.0)
		       (setq weight (+ 1.0 weight)))
		     (if horizontal-p
			 (round (+ delta (* (window-width window) weight)))
		       (round (* (window-height window) weight)))))))
       (split-window window new-size horizontal-p)))
       
(defun ide-skel-win--process-win-node (win win-node &optional window-proc)
  (let ((win2 win))
    (set-window-buffer win (win-node-buffer win-node))
					; (set-window-start win (win-node-buf-corner-pos win-node))
    (set-window-hscroll win (win-node-horiz-scroll win-node))
    (set-window-point win (win-node-point win-node))
    (when window-proc (setq win (funcall window-proc win)))
    (dolist (division (win-node-divisions win-node))
      (when (not (null (division-win-node division)))
	(let ((child-window (ide-skel-win--split win (division-horizontal-p division) (division-percent division))))
	  (when window-proc (setq win (funcall window-proc win)))
	  (ide-skel-win--process-win-node child-window (division-win-node division) window-proc))))
    (with-selected-window win2
      (let ((fixed-size (win-node-fixed-size win-node))
	    (window-size-fixed nil))
	(when fixed-size
	  (when (car fixed-size)
	    (enlarge-window (- (car fixed-size) (window-width win2)) t))
	  (when (cdr fixed-size)
	    (enlarge-window (- (cdr fixed-size) (window-height win2)) nil)))))
    (when (win-node-cursor-priority win-node)
      (unless sel-window
	(setq sel-window win
	      sel-priority (win-node-cursor-priority win-node)))
      (when (< (win-node-cursor-priority win-node) sel-priority)
	(setq sel-window win
	      sel-priority (win-node-cursor-priority win-node))))))
  
(defun ide-skel-win--synthesis (window win-node &optional window-proc)
  (let ((window-size-fixed nil)
	sel-window
	sel-priority)
    (ide-skel-win--process-win-node window win-node window-proc)
    (when sel-window
      (select-window sel-window))
    (when ide-skel-win--minibuffer-selected-p
      (select-window (minibuffer-window)))
    (setq ide-skel-win--minibuffer-selected-p nil)
    (dolist (window (ide-skel-window-list))
      (when (ide-skel-side-view-window-p window) (set-window-dedicated-p window t)))))

(defun ide-skel-win--remove-child (win-node child-win-node)
  (if (eq win-node child-win-node)
      (let* ((division (ide-skel-win--remove-division win-node t))
	     (divisions (win-node-divisions win-node)))
	(when division
	  (ide-skel-win--absorb-win-node win-node (division-win-node division)))
	(setf (win-node-divisions win-node)
	      (append divisions (win-node-divisions win-node))))
    (dolist (division (win-node-divisions win-node))
      (if (and (eq (division-win-node division) child-win-node) (null (win-node-divisions (division-win-node division))))
	  (setf (division-win-node division) nil)
	(ide-skel-win--remove-child (division-win-node division) child-win-node)))))

(defun ide-skel-win-remove-window (window)
  "Remove window with coordinates WINDOW."
  (let* ((window-corner-pos (ide-skel-win-corner window))
	 (root-win-node (ide-skel-win--analysis))
	 (child-win-node (ide-skel-win--find-node-by-corner-pos root-win-node window-corner-pos)))
    (ide-skel-win--remove-child root-win-node child-win-node)
    (ide-skel-win--synthesis (selected-window) root-win-node)))

(defun ide-skel-win-add-window (buffer parent-window-edges edge-symbol size)
  "Split PARENT-WINDOW-EDGES window along specified edge.  In new window with width/height SIZE
show buffer BUFFER.  SIZE can be integer (character count) or float 0.0 - 1.0."
  (when (windowp parent-window-edges)
    (setq parent-window-edges (window-edges parent-window-edges)))
  (let ((horizontal-p (or (eq edge-symbol 'left) (eq edge-symbol 'right)))
	(replace-parent-p (or (eq edge-symbol 'top) (eq edge-symbol 'left)))
	(percentage
	 (if (or (eq edge-symbol 'bottom) (eq edge-symbol 'right))
	     (- size)
	   size)))
    (ide-skel-win--add-window buffer parent-window-edges horizontal-p percentage replace-parent-p)))

(defun ide-skel-win--add-window (buffer parent-window-edges horizontal-p percentage replace-parent-p)
  (let* ((root-win-node (ide-skel-win--analysis))
	 (new-win-node (make-win-node :buffer buffer)))
    (ide-skel-win--synthesis (selected-window) root-win-node
			(lambda (window)
			  (if (equal (window-edges window) parent-window-edges)
			      (let ((child-window (ide-skel-win--split window horizontal-p percentage)))
				(set-window-buffer (if replace-parent-p window child-window) buffer)
				(if replace-parent-p child-window window))
			    window)))))

(defun ide-skel-win--get-bounds (object)
  (cond ((windowp object) (window-edges object))
	((and (listp object) (= (length object) 4)) object)
	(t (error "Invalid object param: %S" object))))

(defun ide-skel-win--win-area (window)
  (let ((win-bounds (ide-skel-win--get-bounds window)))
    (* (- (nth 2 win-bounds) (nth 0 win-bounds))
       (- (nth 3 win-bounds) (nth 1 win-bounds)))))

(defun ide-skel-win--is-adjacent(window1 edge-symbol window2)
  "Non-nil if WINDOW1 sticks to WINDOW2 along specified edge."
  (let ((bounds1 (ide-skel-win--get-bounds window1))
	(bounds2 (ide-skel-win--get-bounds window2))
	result)
    (if (or (equal edge-symbol 'top) (equal edge-symbol 'bottom))
	(setq result (and
		      (equal (nth 0 bounds1) (nth 0 bounds2))   ; bounds.LEFT = bounds2.LEFT
		      (equal (nth 2 bounds1) (nth 2 bounds2))))	; bounds.RIGHT = bounds2.RIGHT
      (setq result (and
		    (equal (nth 1 bounds1) (nth 1 bounds2))    ; bounds.TOP = bounds2.TOP
		    (equal (nth 3 bounds1) (nth 3 bounds2))))) ; bounds.BOTTOM = bounds2.BOTTOM
    (when result
      (setq result
	    (cond ((equal edge-symbol 'top) (equal (nth 1 bounds1) (nth 3 bounds2)))	; bounds.TOP = bounds2.BOTTOM
		  ((equal edge-symbol 'bottom) (equal (nth 3 bounds1) (nth 1 bounds2))) ; bounds.BOTTOM = bounds2.TOP
		  ((equal edge-symbol 'left) (equal (nth 0 bounds1) (nth 2 bounds2)))   ; bounds.LEFT = bounds2.RIGHT
		  (t (equal (nth 2 bounds1) (nth 0 bounds2))))))
    result))

(defun ide-skel-win--is-leaf (&optional window)
  "Non-nil if WINDOW is a leaf."
  (unless window
    (setq window (selected-window)))
  ;; no window can stick from right or bottom
  (when (and (not (get-window-with-predicate
		   (lambda (win) (ide-skel-win--is-adjacent window 'right win)) 1 1))
	     (not (get-window-with-predicate
		   (lambda (win) (ide-skel-win--is-adjacent window 'bottom win)) 1 1)))
    (let ((parent (ide-skel-previous-window window 1 1)))
      ;; parent must exist and come from left or up
      (when (and parent
		 (or (ide-skel-win--is-adjacent window 'top parent)
		     (ide-skel-win--is-adjacent window 'left parent)))
	window))))

(defun ide-skel-win--is-leaf2 (&optional win2)
  "Non-nil if WIN2 is leaf."
  (unless win2
    (setq win2 (selected-window)))
  ;; no window can stick from right or bottom
  (when (and (not (get-window-with-predicate
		   (lambda (win) (ide-skel-win--is-adjacent win2 'right win))))
	     (not (get-window-with-predicate
		   (lambda (win) (ide-skel-win--is-adjacent win2 'bottom win)))))
    (let ((parent (ide-skel-previous-window win2 1 1)))
      ;; parent must exist and come from left or up
      (when (and parent
		 (or (ide-skel-win--is-adjacent win2 'top parent)
		     (ide-skel-win--is-adjacent win2 'left parent)))
	win2))))

(defun ide-skel-win-corner (window)
  (let ((coords (window-edges window)))
    (cons (car coords) (cadr coords))))

(defun ide-skel-window-size-changed (frame)
  (let* ((editor-window (ide-skel-get-editor-window))
	 (left-view-window (car ide-skel--current-side-windows))
	 (right-view-window (cdr ide-skel--current-side-windows))
	 (bottom-view-window (ide-skel-get-bottom-view-window)))
    (ide-skel-recalculate-view-cache)
    (when bottom-view-window
      (ide-skel-remember-bottom-view-window))
    (when left-view-window
      (setq ide-skel-left-view-window-width (window-width left-view-window)))
    (when right-view-window
      (setq ide-skel-right-view-window-width (window-width right-view-window)))))
      
(add-hook 'window-size-change-functions 'ide-skel-window-size-changed)

(setq special-display-regexps ide-skel-bottom-view-buffer-names-regexps)

(defun ide-skel-recalculate-view-cache ()
  (setq ide-skel-selected-frame (selected-frame)
	ide-skel-current-editor-window (ide-skel-get-editor-window))
  (setq ide-skel-current-editor-buffer (window-buffer ide-skel-current-editor-window)
        ide-skel-current-left-view-window (car ide-skel--current-side-windows)
	ide-skel-current-right-view-window (cdr ide-skel--current-side-windows)))

(defun ide-skel-get-last-selected-window ()
  (and ide-skel-last-selected-window-or-buffer
       (or (and (window-live-p (car ide-skel-last-selected-window-or-buffer))
		(car ide-skel-last-selected-window-or-buffer))
	   (and (buffer-live-p (cdr ide-skel-last-selected-window-or-buffer))
		(get-buffer-window (cdr ide-skel-last-selected-window-or-buffer))))))

(require 'mwheel)

(defvar ide-skel-mouse-wheel-events (list mouse-wheel-up-event mouse-wheel-down-event))

(run-with-idle-timer 0 t (lambda ()
;; 			   (when ide-skel-current-left-view-window
;; 			     (with-selected-window ide-skel-current-left-view-window
;; 			       (beginning-of-line)))
;; 			   (when ide-skel-current-right-view-window
;; 			     (with-selected-window ide-skel-current-right-view-window
;; 			       (beginning-of-line)))
			   (unless (or (active-minibuffer-window)
				       (memq 'down (event-modifiers last-input-event))
				       (memq (event-basic-type last-input-event) ide-skel-mouse-wheel-events)
				       (mouse-movement-p last-input-event))
			     ;; selected frame changed?
			     (unless (eq (selected-frame) ide-skel-selected-frame)
			       (ide-skel-recalculate-view-cache))
			     ;; side view windows cannot have cursor
			     (while (memq (selected-window) (list ide-skel-current-left-view-window
								  ide-skel-current-right-view-window))
			       (let ((win (ide-skel-get-last-selected-window)))
				 (if (and win (not (eq (selected-window) win)))
				     (select-window win)
				   (other-window 1))))
			     (setq ide-skel-last-selected-window-or-buffer
				   (cons (selected-window) (window-buffer (selected-window))))
			     ;; current buffer changed?
			     (let ((editor-buffer (window-buffer ide-skel-current-editor-window)))
			       (when (not (eq ide-skel-last-buffer-change-event editor-buffer))
				 (ide-skel-send-event nil 'editor-buffer-changed ide-skel-last-buffer-change-event editor-buffer))))))

(setq special-display-function
      (lambda (buffer &optional data)
	(let ((bottom-view-window (ide-skel-get-bottom-view-window)))
	  (if (and bottom-view-window
		   (eq bottom-view-window (selected-window))
		   (member (buffer-name buffer) ide-skel-unexpected-bottom-view-window-buffer-names))
	      (progn
		(show-buffer (ide-skel-get-editor-window) buffer)
		(ide-skel-get-editor-window))
	    (unless (ide-skel-get-bottom-view-window)
	      (ide-skel-show-bottom-view-window))
	    (set-window-buffer (ide-skel-get-bottom-view-window) buffer)
	    ;; (select-window (ide-skel-get-bottom-view-window))
	    (ide-skel-get-bottom-view-window)))))

;;; Bottom view

(defun ide-skel-hidden-buffer-name-p (buffer-name)
  (equal (elt buffer-name 0) 32))

(defun ide-skel-bottom-view-buffer-p (buffer)
  "Non-nil if buffer should be shown in bottom view."
  (let ((name (buffer-name buffer)))
    (or (with-current-buffer buffer
	  (and ide-skel-tabset-name
	       (string= ide-skel-tabset-name ide-skel-bottom-view-window-tabset-name)))
	(and (not (ide-skel-hidden-buffer-name-p name))
	     (some (lambda (regexp) (string-match regexp name)) ide-skel-bottom-view-buffer-names-regexps)
	     (not (some (lambda (regexp) (string-match regexp name)) ide-skel-bottom-view-buffer-names-disallowed-regexps))))))

(defun ide-skel-remember-bottom-view-window ()
  (let ((bottom-view-window (ide-skel-get-bottom-view-window)))
    (when bottom-view-window
      (setq ide-skel--last-bottom-view-buffer-name (buffer-name (window-buffer bottom-view-window))
	    ide-skel-bottom-view-window-size (max 5 (window-height bottom-view-window))))))

(defun ide-skel--find-buffer-for-bottom-view-window ()
  "Returns first buffer to display in bottom view window (always returns a buffer)."
  (let ((best-buffers (list (car (buffer-list (selected-frame))))))
    (some (lambda (buffer)
	    (when (ide-skel-bottom-view-buffer-p buffer)
	      (if (member (buffer-name buffer) ide-skel-unexpected-bottom-view-window-buffer-names)
		  (setq best-buffers (append best-buffers (list buffer)))
		(setq best-buffers (cons buffer best-buffers)))
	      nil))
	  (buffer-list (selected-frame)))
    (if (and (not ide-skel-was-scratch)
	     (get-buffer "*scratch*"))
	(progn
	  (setq ide-skel-was-scratch t)
	  (get-buffer "*scratch*"))
    (car best-buffers))))

(defun ide-skel--is-full-width-window (window &rest except-windows)
  (let ((bounds (window-edges window))
	(result t))
    (dolist (win (ide-skel-window-list))
      (unless (memq win except-windows)
        (let ((left-edge-pos (car (window-edges win))))
          (when (or (< left-edge-pos (car bounds))
                    (>= left-edge-pos (nth 2 bounds)))
            (setq result nil)))))
    result))

(defun ide-skel-get-bottom-view-window ()
  (let* ((editor-window (ide-skel-get-editor-window))
	 best-window)
    ;; get lowest window
    (dolist (win (copy-list (window-list nil 1)))
      (when (with-current-buffer (window-buffer win)
	      (and (or (not ide-skel-tabset-name)
		       (equal ide-skel-tabset-name ide-skel-bottom-view-window-tabset-name))
		   (not (eq win editor-window))))
        (if (null best-window)
            (setq best-window win)
          (when (> (cadr (window-edges win)) (cadr (window-edges best-window)))
            (setq best-window win)))))
    (when (and best-window
	       (not (ide-skel--is-full-width-window best-window (ide-skel-get-left-view-window) (ide-skel-get-right-view-window))))
      (setq best-window nil))
    best-window))

(defun ide-skel-show-bottom-view-window (&optional buffer)
  (interactive)
  (unless ide-skel-bottom-view-window-oper-in-progress
    (let ((saved-window (cons (selected-window) (window-buffer (selected-window)))))
      (unwind-protect
	  (unless (ide-skel-get-bottom-view-window) ;; if not open yet
	    (setq ide-skel-bottom-view-window-oper-in-progress t)
	    (unless buffer
	      (setq buffer
		    (or (and ide-skel--last-bottom-view-buffer-name (get-buffer ide-skel--last-bottom-view-buffer-name))
			(ide-skel--find-buffer-for-bottom-view-window))))
	    (let* ((left-view-window (ide-skel-get-left-view-window))
		   (left-view-window-bounds (and left-view-window
						 (window-edges left-view-window)))
		   (right-view-window (ide-skel-get-right-view-window))
		   (right-view-window-bounds (and right-view-window
						  (window-edges right-view-window)))
		   (root-win-node (ide-skel-win--analysis))
		   (window-bounds (window-edges (selected-window)))) ; bounds of maximized window (after analysis)
	      (when (and left-view-window-bounds (not ide-skel-bottom-view-on-left-view))
		(setf (nth 0 window-bounds) (nth 2 left-view-window-bounds)))
	      (when (and right-view-window-bounds (not ide-skel-bottom-view-on-right-view))
		(setf (nth 2 window-bounds) (nth 0 right-view-window-bounds)))
	      (ide-skel-win--synthesis (selected-window) root-win-node)
	      (let ((ide-skel-win--win2-switch (and (not (null left-view-window))
						    ide-skel-bottom-view-on-right-view))
		    (old ide-skel-ommited-windows))
		(when (and (not ide-skel-bottom-view-on-left-view)
			   (not ide-skel-bottom-view-on-right-view)
			   (ide-skel-get-left-view-window))
		  (push (ide-skel-get-left-view-window) ide-skel-ommited-windows))
		(ide-skel-win-add-window buffer window-bounds 'bottom ide-skel-bottom-view-window-size)
		(setq ide-skel-ommited-windows old))))
	(if (window-live-p (car saved-window))
	    (select-window (car saved-window))
	  (when (get-buffer-window (cdr saved-window))
	      (select-window (get-buffer-window (cdr saved-window)))))
	(setq ide-skel-bottom-view-window-oper-in-progress nil)))))

(defun ide-skel-hide-bottom-view-window ()
  (interactive)
  (unless ide-skel-bottom-view-window-oper-in-progress
    (setq ide-skel-bottom-view-window-oper-in-progress t)
    (let ((bottom-view-window (ide-skel-get-bottom-view-window)))
      (when bottom-view-window
	(let ((ide-skel-win--win2-switch nil)
	      (select-editor (eq bottom-view-window (selected-window))))
	  (ide-skel-remember-bottom-view-window)
	  (ide-skel-win-remove-window bottom-view-window)
	  (when select-editor (select-window (ide-skel-get-editor-window))))))
    (setq ide-skel-bottom-view-window-oper-in-progress nil)))

(defun ide-skel-toggle-bottom-view-window ()
  "Toggle bottom view window."
  (interactive)
  (if (ide-skel-get-bottom-view-window)
      (ide-skel-hide-bottom-view-window)
    (ide-skel-show-bottom-view-window)))

;;; Editor

(defun ide-skel-get-editor-window ()
  (let (best-window)
    (setq ide-skel--current-side-windows (cons nil nil))
    (dolist (win (copy-list (window-list nil 1)))
      (when (with-current-buffer (window-buffer win)
	      (when (equal ide-skel-tabset-name ide-skel-left-view-window-tabset-name)
		(setcar ide-skel--current-side-windows win))
	      (when (equal ide-skel-tabset-name ide-skel-right-view-window-tabset-name)
		(setcdr ide-skel--current-side-windows win))
	      (or (not ide-skel-tabset-name)
		  (equal ide-skel-tabset-name ide-skel-editor-window-tabset-name)))
        (if (null best-window)
            (setq best-window win)
          (let* ((best-window-coords (window-edges best-window))
                 (win-coords (window-edges win)))
            (when (or (< (cadr win-coords) (cadr best-window-coords))
                      (and (= (cadr win-coords) (cadr best-window-coords))
                           (< (car win-coords) (car best-window-coords))))
              (setq best-window win))))))
    best-window))

;;; Left view & Right view

(defun ide-skel-toggle-side-view-window (name &optional run-hooks)
  (if (funcall (intern (format "ide-skel-get-%s-view-window" name)))
      (funcall (intern (format "ide-skel-hide-%s-view-window" name)) run-hooks)
    (funcall (intern (format "ide-skel-show-%s-view-window" name)) run-hooks)))

(defun ide-skel-toggle-left-view-window ()
  (interactive)
  (ide-skel-toggle-side-view-window 'left (interactive-p)))

(defun ide-skel-toggle-right-view-window ()
  (interactive)
  (ide-skel-toggle-side-view-window 'right (interactive-p)))


(add-hook 'kill-buffer-hook (lambda ()
			      (when (eq ide-skel-current-editor-buffer (current-buffer))
				(let* ((context (gethash ide-skel-current-editor-buffer ide-skel-context-properties))
				       (imenu-buffer (cdr (assq :imenu-buffer context)))
				       (imenu-window (when imenu-buffer (get-buffer-window imenu-buffer))))
				  (when imenu-window
				    (set-window-dedicated-p imenu-window nil)
				    (set-window-buffer imenu-window ide-skel-default-right-view-buffer) 
				    (set-window-dedicated-p imenu-window t))
				  (remhash (current-buffer) ide-skel-context-properties)
				  (when imenu-buffer
				    (kill-buffer imenu-buffer))))))

(defun ide-skel-send-event (side-symbol event-type &rest params)
  (ide-skel-recalculate-view-cache)
  (cond ((eq event-type 'hide)
	 (run-hook-with-args-until-success 'ide-skel-side-view-window-functions side-symbol 'hide)
	 (ide-skel-disable-nonactual-side-view-tabs side-symbol 'disable-all))
	((eq event-type 'show)
	 (run-hook-with-args-until-success 'ide-skel-side-view-window-functions side-symbol 'show)
	 (ide-skel-side-window-switch-to-buffer (symbol-value (intern (format "ide-skel-current-%s-view-window" side-symbol))) nil))
	((eq event-type 'editor-buffer-changed)
	 (run-hooks 'ide-skel-editor-buffer-changed-hook)
	 (when ide-skel-current-left-view-window
	   (ide-skel-disable-nonactual-side-view-tabs 'left)
	   (run-hook-with-args-until-success 'ide-skel-side-view-window-functions
					     'left 'editor-buffer-changed
					     ide-skel-last-buffer-change-event ide-skel-current-editor-buffer)
	   (ide-skel-side-window-switch-to-buffer ide-skel-current-left-view-window nil))
	 (when ide-skel-current-right-view-window
	   (ide-skel-disable-nonactual-side-view-tabs 'right)
	   (run-hook-with-args-until-success 'ide-skel-side-view-window-functions
					     'right 'editor-buffer-changed
					     (car params) (cadr params))
	   (ide-skel-side-window-switch-to-buffer ide-skel-current-right-view-window nil))
	 (setq ide-skel-last-buffer-change-event ide-skel-current-editor-buffer))
	((eq event-type 'tab-change)
	 (run-hook-with-args-until-success 'ide-skel-side-view-window-functions side-symbol 'tab-change (car params) (cadr params)))))

(defun ide-skel-hide-side-view-window (name &optional run-hooks)
  (let* ((view-window (funcall (intern (format "ide-skel-get-%s-view-window" name))))
	 (select-editor (eq view-window (selected-window))))
    (when view-window
      (when (active-minibuffer-window)
	(error "Cannot remove side window while minibuffer is active"))
      (let* ((bottom-view-window (ide-skel-get-bottom-view-window))
	     (selected-bottom-view-window (and bottom-view-window (eq bottom-view-window (selected-window))))
	     (buffer (window-buffer view-window))
	     (second-side-window (funcall (intern (format "ide-skel-get-%s-view-window" (if (eq name 'left) 'right 'left))))))
	(set (intern (format "ide-skel-last-%s-view-buffer" name)) buffer)
	(when run-hooks
	  (ide-skel-send-event name 'hide))
 	(when bottom-view-window
 	  (ide-skel-hide-bottom-view-window))
	(when second-side-window
	  (push second-side-window ide-skel-ommited-windows))
	(let ((ide-skel-win--win2-switch (eq name 'left)))
	  (set (intern (format "ide-skel-%s-view-window-width" name)) (window-width view-window))
	  (ide-skel-win-remove-window view-window))
	(setq ide-skel-ommited-windows nil)
 	(when bottom-view-window
 	  (ide-skel-show-bottom-view-window)
	  (when selected-bottom-view-window
	    (select-window (ide-skel-get-bottom-view-window))))
	(ide-skel-recalculate-view-cache)
	(when select-editor (select-window (ide-skel-get-editor-window)))))))

(defun ide-skel-hide-left-view-window (&optional run-hooks)
  (interactive)
  (let ((right-view-window (ide-skel-get-right-view-window)))
    (when right-view-window
      (ide-skel-hide-right-view-window))
    (ide-skel-hide-side-view-window 'left (or run-hooks (interactive-p)))
    (when right-view-window
      (ide-skel-show-right-view-window))))

(defun ide-skel-hide-right-view-window (&optional run-hooks)
  (interactive)
  (ide-skel-hide-side-view-window 'right (or (interactive-p) run-hooks)))

(defun ide-skel-get-side-view-buffer-create (name side-sym tab-label help-string keep-condition-function)
  (let* ((was-buffer (get-buffer name))
	 (km (make-sparse-keymap))
	 (buffer (get-buffer-create name)))
    (unless was-buffer
      (with-current-buffer buffer
	(kill-all-local-variables)
	(remove-overlays)
	(define-key km [drag-mouse-1] 'ignore)
	(use-local-map km)
	(make-local-variable 'mouse-wheel-scroll-amount)
	(make-local-variable 'auto-hscroll-mode)
	(make-local-variable 'hscroll-step)
	(make-local-variable 'hscroll-margin)
	(setq ide-skel-tabset-name (if (eq side-sym 'left) ide-skel-left-view-window-tabset-name ide-skel-right-view-window-tabset-name)
	      ide-skel-tabbar-tab-label tab-label
	      ide-skel-tabbar-tab-help-string help-string
	      ide-skel-keep-condition-function keep-condition-function
	      auto-hscroll-mode nil
	      hscroll-step 0.0
	      hscroll-margin 0
;; 	      left-fringe-width 0
;; 	      right-fringe-width 0
	      buffer-read-only t
	      mode-line-format " "
	      mouse-wheel-scroll-amount '(1)
	      window-size-fixed 'width)
	;; (make-variable-buffer-local 'fringe-indicator-alist)
	(setq fringe-indicator-alist (copy-alist default-fringe-indicator-alist))
;; 	(when (>= emacs-major-version 22)
;; 	  (set 'indicate-buffer-boundaries '((up . left) (down . left))))
	(setcdr (assq 'truncation fringe-indicator-alist) nil)
	(set (make-local-variable 'scroll-conservatively) 1500) ; much greater than 0
	(when (and window-system
		   (not ide-skel-side-view-display-cursor))
	  (setq cursor-type nil))))
    buffer))

(defvar ide-skel-default-left-view-buffer
  (let ((buffer (ide-skel-get-side-view-buffer-create " Default Left View Buffer" 'left nil nil (lambda (buf) t))))
    (with-current-buffer buffer
      (setq header-line-format " "))
    buffer))
(defvar ide-skel-last-left-view-buffer ide-skel-default-left-view-buffer)
(defvar ide-skel-default-right-view-buffer
  (let ((buffer (ide-skel-get-side-view-buffer-create " Default Right View Buffer" 'right nil nil (lambda (buf) t))))
    (with-current-buffer buffer
      (setq header-line-format " "))
    buffer))
(defvar ide-skel-last-right-view-buffer ide-skel-default-right-view-buffer)

(defun ide-skel-show-side-view-window (name &optional run-hooks)
  (unless (funcall (intern (format "ide-skel-get-%s-view-window" name)))
    (let* ((current-buffer (window-buffer (selected-window)))
	   (bottom-view-window (ide-skel-get-bottom-view-window))
	   root-win-node
	   (bottom-view-window-bounds (and (or (symbol-value (intern (format "ide-skel-bottom-view-on-%s-view" name)))
					       (and ide-skel-bottom-view-on-left-view
						    (not ide-skel-bottom-view-on-right-view)))
					   bottom-view-window
					   (window-edges bottom-view-window)))
	   best-window-bounds)
      (when bottom-view-window-bounds
 	(ide-skel-hide-bottom-view-window))
      (let ((second-side-window (funcall (intern (format "ide-skel-get-%s-view-window" (if (eq name 'left) 'right 'left))))))
	(when second-side-window
	  (push second-side-window ide-skel-ommited-windows))
	(setq root-win-node (ide-skel-win--analysis))
	(setq best-window-bounds (window-edges (selected-window))) ; bounds of maximized window (after analysis)
	(ide-skel-win--synthesis (selected-window) root-win-node)
	(ide-skel-win-add-window
	 (symbol-value (intern (format (if run-hooks "ide-skel-default-%s-view-buffer" "ide-skel-last-%s-view-buffer") name)))
	 best-window-bounds name
	 (symbol-value (intern (format "ide-skel-%s-view-window-width" name))))
	(setq ide-skel-ommited-windows nil)
 	(when bottom-view-window-bounds
 	  (ide-skel-show-bottom-view-window))
	(set-window-dedicated-p (funcall (intern (format "ide-skel-get-%s-view-window" name))) t)
	(when run-hooks
	  (dolist (tab (tabbar-tabs (tabbar-get-tabset (symbol-value (intern (format "ide-skel-%s-view-window-tabset-name" name))))))
	    (tabbar-delete-tab tab))
	  (ide-skel-send-event name 'show))
	(some (lambda (win) (when (eq (window-buffer win) current-buffer) (select-window win) t)) (copy-list (window-list nil 1)))))))

;; Disables from view all buffers for which keep condition function
;; returns nil.  If a current buffer is there, select another enabled,
;; which implies tab-change event, then select any enabled buffer.
(defun ide-skel-disable-nonactual-side-view-tabs (name &optional disable-all)
  (let* ((tabset (tabbar-get-tabset (symbol-value (intern (format "ide-skel-%s-view-window-tabset-name" name)))))
	 (tabs (tabbar-tabs tabset))
	 (editor-buffer (window-buffer (ide-skel-get-editor-window)))
	 selected-deleted
	 (selected-tab (tabbar-selected-tab tabset)))
    (when tabs
      (dolist (tab tabs)
	(let ((buffer (tabbar-tab-value tab)))
	  (with-current-buffer buffer
	    (when (or disable-all
		      (not ide-skel-keep-condition-function)
		      (not (funcall ide-skel-keep-condition-function editor-buffer)))
	      (setq ide-skel-tabbar-enabled nil)
	      (when (eq tab selected-tab)
		(setq selected-deleted t))
	      (tabbar-delete-tab tab)))))
      (let ((selected-buffer (when (and (not selected-deleted)
					(tabbar-tabs tabset) (tabbar-selected-value tabset)))))
	(when (and (not disable-all)
		   (or selected-deleted
		       (not (eq (tabbar-selected-tab tabset) selected-tab))))
	  (unless selected-buffer
	    (setq selected-buffer (symbol-value (intern (format "ide-skel-default-%s-view-buffer" name)))))
	  (ide-skel-side-window-switch-to-buffer
	   (symbol-value (intern (format "ide-skel-current-%s-view-window" name)))
	   selected-buffer))))))
	
(defun ide-skel-show-left-view-window (&optional run-hooks)
  (interactive)
  (let ((right-view-window (ide-skel-get-right-view-window)))
    (when right-view-window
      (ide-skel-hide-right-view-window))
    (ide-skel-show-side-view-window 'left (or run-hooks (interactive-p)))
    (when right-view-window
      (ide-skel-show-right-view-window))))

(defun ide-skel-show-right-view-window (&optional run-hooks)
  (interactive)
  (ide-skel-show-side-view-window 'right (or run-hooks (interactive-p))))

(defun ide-skel-get-side-view-window (name)
  (let ((tabset-name (symbol-value (intern (format "ide-skel-%s-view-window-tabset-name" name)))))
    (some (lambda (win)
	    (when (with-current-buffer (window-buffer win)
		    (equal ide-skel-tabset-name tabset-name))
	      win))
	  (copy-list (window-list nil 1)))))
  
(defun ide-skel-get-left-view-window ()
  (ide-skel-get-side-view-window 'left))

(defun ide-skel-get-right-view-window ()
  (ide-skel-get-side-view-window 'right))

(defun ide-skel-get-side-view-windows ()
  (let (result
	(left-view-win (ide-skel-get-left-view-window))
	(right-view-win (ide-skel-get-right-view-window)))
    (when left-view-win (push left-view-win result))
    (when right-view-win (push right-view-win result))
    result))

(defun ide-skel-side-view-window-p (window)
  (ide-skel-side-view-buffer-p (window-buffer window)))

(defun ide-skel-side-view-buffer-p (buffer)
  (with-current-buffer buffer
    (or (equal ide-skel-tabset-name ide-skel-left-view-window-tabset-name)
	(equal ide-skel-tabset-name ide-skel-right-view-window-tabset-name))))

(defadvice delete-window (around delete-window-around-advice (&optional window))
  (let* ((target-window (if window window (selected-window)))
	 (editor-window (and (interactive-p) (ide-skel-get-editor-window))) ; for ide-skel--current-side-windows (side-effects)
	 (hide-view-windows (and (interactive-p)
				 (not (eq (car ide-skel--current-side-windows) target-window))
				 (not (eq (cdr ide-skel--current-side-windows) target-window))))
	 (hide-left-view-window (and hide-view-windows (car ide-skel--current-side-windows)))
	 (hide-right-view-window (and hide-view-windows (cdr ide-skel--current-side-windows)))
	 result)
    (when (interactive-p)
      (if (eq (car ide-skel--current-side-windows) target-window)
	  (ide-skel-send-event 'left 'hide)
	(when (eq (cdr ide-skel--current-side-windows) target-window)
	  (ide-skel-send-event 'right 'hide))))
    (let* ((edges (window-inside-edges window))
	   (buf (window-buffer window))
	   win
	   (center-position (cons (/ (+ (car edges) (caddr edges)) 2)
				  (/ (+ (cadr edges) (cadddr edges)) 2))))
      (when hide-left-view-window (ide-skel-hide-left-view-window))
      (when hide-right-view-window (ide-skel-hide-right-view-window))
      (setq win (window-at (car center-position) (cdr center-position)))
      (when (eq (window-buffer win) buf)
	(setq window (window-at (car center-position) (cdr center-position)))))
    (unwind-protect
	(setq result (progn ad-do-it))
      (when hide-left-view-window (ide-skel-show-left-view-window))
      (when hide-right-view-window (ide-skel-show-right-view-window)))
    result))
(ad-activate 'delete-window)

(defadvice delete-other-windows (around delete-other-windows-around-advice (&optional window))
  (ide-skel-assert-not-in-side-view-window)
  (let* ((editor-window (ide-skel-get-editor-window))
	 (dont-revert-after (and (interactive-p) (listp current-prefix-arg) (car current-prefix-arg))) ; C-u
	 (hide-left-view-window (and (interactive-p) (car ide-skel--current-side-windows)))
	 (hide-right-view-window (and (interactive-p) (cdr ide-skel--current-side-windows)))
	 result)
    (when hide-left-view-window (ide-skel-hide-left-view-window dont-revert-after))
    (when hide-right-view-window (ide-skel-hide-right-view-window dont-revert-after))
    (unwind-protect
	(setq result (progn ad-do-it))
      (when (not dont-revert-after)
	(when hide-left-view-window
	  (ide-skel-show-left-view-window))
	(when hide-right-view-window
	  (ide-skel-show-right-view-window))))
    result))
(ad-activate 'delete-other-windows)

(defun ide-skel-assert-not-in-side-view-window ()
  (when (and (interactive-p) (ide-skel-side-view-window-p (selected-window)))
    (error "Cannot do it")))

(defadvice kill-buffer (before kill-buffer-before-advice (buffer))
  (ide-skel-assert-not-in-side-view-window))
(ad-activate 'kill-buffer)

(defadvice split-window-vertically (before split-window-vertically-before-advice (&optional size))
  (ide-skel-assert-not-in-side-view-window))
(ad-activate 'split-window-vertically)

(defadvice split-window-horizontally (before split-window-horizontally-before-advice (&optional size))
  (ide-skel-assert-not-in-side-view-window))
(ad-activate 'split-window-horizontally)

(defadvice mouse-drag-vertical-line (around mouse-drag-vertical-line-around-advice (start-event))
  (let* ((editor-window (ide-skel-get-editor-window))
	 (left-view-window (car ide-skel--current-side-windows))
	 (right-view-window (cdr ide-skel--current-side-windows)))
    (when left-view-window (with-selected-window left-view-window (setq window-size-fixed nil)))
    (when right-view-window (with-selected-window right-view-window (setq window-size-fixed nil)))
    (unwind-protect
	(progn ad-do-it)
      (when left-view-window (with-selected-window left-view-window (setq window-size-fixed 'width)))
      (when right-view-window (with-selected-window right-view-window (setq window-size-fixed 'width))))))
(ad-activate 'mouse-drag-vertical-line)

(defadvice other-window (after other-window-after-advice (arg &optional all-frames))
  (if (memq (selected-window) (list ide-skel-current-left-view-window ide-skel-current-right-view-window))
      (other-window arg all-frames)
    ad-return-value))
(ad-activate 'other-window)

;; Buffer list buffer (left side view)

(define-derived-mode fundmental-mode
  fundamental-mode "Fundmental")

(setq default-major-mode 'fundmental-mode)

(defun ide-skel-recentf-closed-files-list ()
  "Lista ostatnio otwieranych, ale aktualnie zamknietych plikow"
  (let* ((open-file-paths (delq nil (mapcar (lambda (buffer) (buffer-file-name buffer)) (buffer-list)))))
    (if (featurep 'recentf)
	(sort (reverse (set-difference recentf-list open-file-paths :test 'string=))
	      (lambda (path1 path2)
		(string< (file-name-nondirectory path1) (file-name-nondirectory path2))))
      nil)))

(defun ide-skel-select-buffer (buffer-or-path &optional line-no)
  (let* ((window (ide-skel-get-last-selected-window))
	 (buffer (or (and (bufferp buffer-or-path) buffer-or-path)
		    (find-file-noselect buffer-or-path)))
	 (is-bottom-view-buffer (ide-skel-bottom-view-buffer-p buffer)))
    (when (not (buffer-live-p buffer))
      (error "Buffer %s is dead" buffer))
    (unless (get-buffer-window buffer)
      ;; (message "%S %S" window (ide-skel-get-bottom-view-window))
      (if (and window
	       (not (eq window (ide-skel-get-bottom-view-window)))
	       (not is-bottom-view-buffer))
	  (set-window-buffer window buffer)
	(let ((editor-window (ide-skel-get-editor-window)))
	  (select-window editor-window)
	  (if is-bottom-view-buffer
	      (switch-to-buffer-other-window buffer)
	    (set-window-buffer editor-window buffer)))))
    (setq ide-skel-last-selected-window-or-buffer (cons (get-buffer-window buffer) buffer))
    (select-window (car ide-skel-last-selected-window-or-buffer))
    (when line-no
      (with-current-buffer buffer
	(goto-line line-no)))))

(defun ide-skel-select-buffer-handler (event)
  (interactive "@e")
  ;; (message "EVENT: %S" event)
  (with-selected-window (posn-window (event-start event))
    (let* ((object (get-text-property (posn-point (event-start event)) 'object-to-display)))
      (beginning-of-line)
      (ide-skel-select-buffer object))))
    
(defun ide-skel-buffers-view-insert-buffer-list (label buffer-list)
  (setq label (propertize label 'face 'bold))
  (insert (format "%s\n" label))
  (dolist (object buffer-list)
    (let* ((label (format "  % -100s" (if (bufferp object) (buffer-name object) (file-name-nondirectory object))))
	   (km (make-sparse-keymap)))
      (define-key km [mouse-1] 'ide-skel-select-buffer-handler)
      (setq label (propertize label
			      'mouse-face 'ide-skel-highlight-face
			      'local-map km
			      'face 'variable-pitch
			      'pointer 'hand
			      'object-to-display object
			      'help-echo (if (bufferp object) (buffer-file-name object) object)))
      (insert label)
      (insert "\n"))))

(defun ide-skel-buffers-view-fill ()
  (when ide-skel-current-left-view-window
    (with-current-buffer ide-skel-buffer-list-buffer
      (let ((point (point))
	    (window-start (when (eq (window-buffer ide-skel-current-left-view-window) ide-skel-buffer-list-buffer)
			    (save-excursion
			      (goto-char (window-start ide-skel-current-left-view-window))
			      (cons (line-number-at-pos) (current-column))))))
	;; (message "%S" window-start)
	(let (asterisk-buffers
	      (inhibit-read-only t)
	      normal-buffers)
	  (erase-buffer)
	  (dolist (buffer (sort (buffer-list) (lambda (buf1 buf2) (string< (buffer-name buf1) (buffer-name buf2)))))
	    (let* ((name (buffer-name buffer))
		   (first-char (aref (buffer-name buffer) 0)))
	      (unless (char-equal ?\  first-char)
		(if (char-equal ?* first-char)
		    (push buffer asterisk-buffers)
		  (push buffer normal-buffers)))))
	  (ide-skel-buffers-view-insert-buffer-list "Normal Buffers:" normal-buffers)
	  (ide-skel-buffers-view-insert-buffer-list "Scratch Buffers:" asterisk-buffers)
	  (ide-skel-buffers-view-insert-buffer-list "Recent Files:" (ide-skel-recentf-closed-files-list)))
	(if window-start
	    (let ((pos (save-excursion
			 (goto-line (car window-start))
			 (beginning-of-line)
			 (forward-char (cdr window-start))
			 (point))))
	      (set-window-start ide-skel-current-left-view-window pos))
	  (goto-char point)
	  (beginning-of-line))))))

(defun ide-skel-some-view-window-buffer (side-symbol predicate)
  (some (lambda (buffer)
	  (and (buffer-live-p buffer)
	       (with-current-buffer buffer
		 (and (equal ide-skel-tabset-name (symbol-value (intern (format "ide-skel-%s-view-window-tabset-name" side-symbol))))
		      ide-skel-tabbar-enabled
		      (funcall predicate buffer)
		      buffer))))
	(buffer-list)))

(defun ide-skel-side-window-switch-to-buffer (side-window buffer)
  "If BUFFER is nil, then select any non-default buffer.  The
TAB-CHANGE event is send only if selected buffer changed."
  (unwind-protect
      (let* ((side-symbol (cond ((eq side-window ide-skel-current-left-view-window) 'left)
			       ((eq side-window ide-skel-current-right-view-window) 'right)
			       (t nil)))
	     (context (gethash ide-skel-current-editor-buffer ide-skel-context-properties))
	     (context-default-tab-label-symbol (intern (format "default-%s-tab-label" side-symbol))))
	(when side-symbol
	  (unless buffer
	    (let* ((default-empty-buffer (symbol-value (intern (format "ide-skel-default-%s-view-buffer" side-symbol))))
		   (context-default-tab-label (cdr (assq context-default-tab-label-symbol context)))
		   (last-view-window-tab-label (symbol-value (intern (format "ide-skel-last-%s-view-window-tab-label" side-symbol)))))
	      ;; first non-nil:
	      ;;  - selected before in this context
	      ;;  - selected in previous context
	      ;;  - current if other than default-empty
	      ;;  - first non default-empty
	      ;;  - default-empty
	      (setq buffer
		    (or (and context-default-tab-label
			     (ide-skel-some-view-window-buffer side-symbol (lambda (buffer)
									     (equal ide-skel-tabbar-tab-label context-default-tab-label))))
			(and last-view-window-tab-label
			     (ide-skel-some-view-window-buffer side-symbol (lambda (buffer)
									     (equal ide-skel-tabbar-tab-label last-view-window-tab-label))))
			(and (not (eq (window-buffer side-window) default-empty-buffer))
			     (window-buffer side-window))
			(ide-skel-some-view-window-buffer side-symbol (lambda (buffer) ide-skel-tabbar-tab-label))
			default-empty-buffer))))
	  (unless (eq (window-buffer side-window) buffer)
	    (set (intern (format "ide-skel-last-%s-view-window-tab-label" side-symbol)) (with-current-buffer buffer ide-skel-tabbar-tab-label))
	    (setq context (assq-delete-all context-default-tab-label-symbol context))
	    (puthash ide-skel-current-editor-buffer
		     (cons (cons context-default-tab-label-symbol (with-current-buffer buffer ide-skel-tabbar-tab-label)) context)
		     ide-skel-context-properties)
	    (ide-skel-send-event side-symbol 'tab-change (window-buffer side-window) buffer)))
	(set-window-dedicated-p side-window nil)
	(set-window-buffer side-window buffer))
    (set-window-dedicated-p side-window t)))

;; args: 'left/right 'show/editor-buffer-changed/hide/tab-change &rest buffer...
(defun ide-skel-default-side-view-window-function (side event &rest list)
  ;; (message "SIDE: %S, event: %S, rest: %S %S" side event list ide-skel-current-left-view-window)
  (when (and (eq side 'left) ide-skel-current-left-view-window)
    (cond ((eq event 'show)
	   (unless ide-skel-buffer-list-buffer
	     (setq ide-skel-buffer-list-buffer (ide-skel-get-side-view-buffer-create
						" Ide-Skel Buffer List Buffer" 'left "Bufs" "List of opened and recent files"
						(lambda (buf) t)))
	     (with-current-buffer ide-skel-buffer-list-buffer
	       (setq ide-skel-tabbar-enabled t)))
	   (ide-skel-buffers-view-fill)
	   (ide-skel-side-window-switch-to-buffer ide-skel-current-left-view-window ide-skel-buffer-list-buffer))))
  nil)
      
  ;; (message "SIDE: %S, event: %S, rest: %S" side event list)

(add-hook 'change-major-mode-hook (lambda () (setq ide-skel-buffer-list-tick t)))
(add-hook 'kill-buffer-hook (lambda () (setq ide-skel-buffer-list-tick t)))
(run-with-idle-timer 0.1 t (lambda ()
			   (when ide-skel-buffer-list-tick
			     (setq ide-skel-buffer-list-tick nil)
			     (ide-skel-buffers-view-fill))))

(add-hook 'ide-skel-side-view-window-functions 'ide-skel-default-side-view-window-function)

(define-key-after global-map [tool-bar ide-skel-toggle-left-view-window]
  (list 'menu-item "Left View Window" 'ide-skel-toggle-left-view-window :image ide-skel-left-view-window-image))
(define-key-after global-map [tool-bar ide-skel-toggle-bottom-view-window]
  (list 'menu-item "Bottom View Window" 'ide-skel-toggle-bottom-view-window :image ide-skel-bottom-view-window-image))
(define-key-after global-map [tool-bar ide-skel-toggle-right-view-window]
  (list 'menu-item "Right View Window" 'ide-skel-toggle-right-view-window :image ide-skel-right-view-window-image))

(eval-after-load "tabbar" '(ide-skel-tabbar-load-hook))

;;; Tree Widget

(defadvice tree-widget-lookup-image (around tree-widget-lookup-image-around-advice (name))
  (if (equal (tree-widget-theme-name) "small-folder")
      (setq ad-return-value (apply 'create-image (symbol-value (intern (format "ide-skel-tree-widget-%s-xpm" name))) 'xpm t (tree-widget-image-properties name)))
    ad-do-it))
(ad-activate 'tree-widget-lookup-image)



;;; Imenu

(require 'imenu)

(defun ide-skel-imenu-refresh ()
  (interactive)
  (ide-skel-imenu-side-view-draw-tree (window-buffer ide-skel-current-right-view-window) t))

(defun ide-skel-imenu-sort-change ()
  (interactive)
  (with-current-buffer (window-buffer ide-skel-current-right-view-window)
    (setq ide-skel-imenu-sorted (not ide-skel-imenu-sorted)))
  (ide-skel-imenu-side-view-draw-tree (window-buffer ide-skel-current-right-view-window) t))

(defun ide-skel-imenu-get-buffer-create (editor-buffer &optional dont-create)
  (let* ((context (gethash editor-buffer ide-skel-context-properties))
	 (buffer (cdr (assq :imenu-buffer context))))
    (when (and (not buffer) (not dont-create))
      (setq buffer (ide-skel-get-side-view-buffer-create (concat " " (buffer-name editor-buffer) " Ide Skel Imenu")
							 'right "Imenu" nil
							 (lambda (editor-buffer)
							   (eq ide-skel-imenu-editor-buffer ide-skel-current-editor-buffer))))
      (with-current-buffer buffer
	(setq ide-skel-tabbar-menu-function
	      (lambda ()
		(let ((is-outline-mode (with-current-buffer (window-buffer ide-skel-current-right-view-window)
					 (with-current-buffer ide-skel-imenu-editor-buffer
					   (or (eq major-mode 'outline-mode)
					       (and (boundp 'outline-minor-mode)
						    (symbol-value 'outline-minor-mode)))))))
		  (append
		   (list
		    (list 'ide-skel-imenu-refresh "Refresh" t)
		    (unless is-outline-mode
		      (list 'ide-skel-imenu-sort-change (if (with-current-buffer (window-buffer ide-skel-current-right-view-window)
							      ide-skel-imenu-sorted)
							    "Natural order"
							  "Sorted order") t))))))
	      ide-skel-imenu-editor-buffer editor-buffer
	      ide-skel-imenu-open-paths (make-hash-table :test 'equal))
	(add-hook 'tree-widget-after-toggle-functions (lambda (widget)
							(let ((path (widget-get widget :path)))
							  (when path
							    (if (widget-get widget :open)
								(puthash path t ide-skel-imenu-open-paths)
							      (remhash path ide-skel-imenu-open-paths)))))
		  nil t))
      (puthash editor-buffer (cons (cons :imenu-buffer buffer) context) ide-skel-context-properties))
    buffer))

(defun ide-skel-tree-node-notify (widget &rest rest)
  (let ((index-name (widget-get widget :index-name))
	(index-position (widget-get widget :index-position))
	(function (widget-get widget :function))
	(arguments (widget-get widget :arguments)))
    (select-window (ide-skel-get-editor-window))
    (if function
	(apply function index-name index-position arguments)
      (goto-char index-position))))

;; building hash
(defun ide-skel-imenu-analyze (hash prefix element)
  (when element
    (if (and (consp (cdr element))
	     (listp (cadr element)))
	(dolist (elem (cdr element))
	  (ide-skel-imenu-analyze hash (concat prefix "/" (car element)) elem))
      (puthash (concat prefix "/" (car element)) (list (cons :element element)) hash))))

;; logical linking, internal nodes creation
(defun ide-skel-imenu-analyze2 (hash prefix element)
  (when element
    (if (and (consp (cdr element))
	     (listp (cadr element)))
	(dolist (elem (cdr element))
	  (ide-skel-imenu-analyze2 hash (concat prefix "/" (car element)) elem))
      (let* ((index-name (car element))
	     (path (concat prefix "/" index-name))
	     (node (gethash path hash))
	     (reverse-separators (let ((index 0)
				       result)
				   (while (string-match "[*#:.]+" index-name index)
				     (push (cons (match-beginning 0) (match-end 0)) result)
				     (setq index (match-end 0)))
				   result))
	     found)
	(some (lambda (separator-pair)
		(let* ((begin (car separator-pair))
		       (end (cdr separator-pair))
		       (before-name (substring index-name 0 begin))
		       (after-name (substring index-name end))
		       (parent-path (concat prefix "/" before-name))
		       (parent-node (gethash parent-path hash)))
		  (when parent-node
		    (push (cons :parent parent-path) node)
		    (unless (assq :name node)
		      (push (cons :name after-name) node))
		    (puthash path node hash)
		    (unless (assq :widget parent-node)
		      (let* ((parent-element (cdr (assq :element parent-node)))
			     (parent-index-name (car parent-element))
			     (parent-index-position (if (consp (cdr parent-element)) (cadr parent-element) (cdr parent-element)))
			     (parent-function (when (consp (cdr parent-element)) (caddr parent-element)))
			     (open-status (gethash parent-path ide-skel-imenu-open-paths))
			     (parent-arguments (when (consp (cdr parent-element)) (cdddr parent-element))))
			(push (cons :widget
				    ;; internal node
				    (list 'ide-skel-imenu-internal-node-widget
					  :open open-status
					  :indent 0
					  :path parent-path
					  :notify 'ide-skel-tree-node-notify
					  :index-name parent-index-name
					  :index-position parent-index-position
					  :function parent-function
					  :arguments parent-arguments
					  :node (list 'push-button
						      :format "%[%t%]\n"
						      :button-face 'variable-pitch
						      :tag (or (cdr (assq :name parent-node))
							       before-name)
						      ;; :tag (cadr (assq :element parent-node))
						      )))
			      parent-node)
			(puthash parent-path parent-node hash)))
		    t)))
	      reverse-separators)))))

;; widget linking, leafs creation
(defun ide-skel-imenu-analyze3 (hash prefix element)
  (when element
    (if (and (consp (cdr element))
	     (listp (cadr element)))
	(dolist (elem (cdr element))
	  (ide-skel-imenu-analyze3 hash (concat prefix "/" (car element)) elem))
      (let* ((index-name (car element))
	     (index-position (if (consp (cdr element)) (cadr element) (cdr element)))
	     (function (when (consp (cdr element)) (caddr element)))
	     (arguments (when (consp (cdr element)) (cdddr element)))
	     (path (concat prefix "/" index-name))
	     (node (gethash path hash))
	     (widget (cdr (assq :widget node)))
	     (parent-path (cdr (assq :parent node)))
	     (parent-node (when parent-path (gethash parent-path hash)))
	     (parent-widget (when parent-node (cdr (assq :widget parent-node)))))
	;; create leaf if not exists
	(unless widget
	  ;; leaf node
	  (push (cons :widget (list 'ide-skel-imenu-leaf-widget
				    :notify 'ide-skel-tree-node-notify
				    :index-name index-name
				    :index-position index-position
				    :function function
				    :arguments arguments
				    :tag (or (cdr (assq :name node))
					     index-name)))
		node)
	  (puthash path node hash)
	  (setq widget (cdr (assq :widget node))))
	;; add to parent
	(when parent-widget
	  (setcdr (last parent-widget) (cons widget nil)))))))

(defun ide-skel-imenu-create-tree (hash prefix element)
  (when element 
    (if (and (consp (cdr element))
	     (listp (cadr element)))
	(let* ((menu-title (car element))
	       (sub-alist (cdr element))
	       (path (concat prefix "/" menu-title))
	       (open-status (gethash path ide-skel-imenu-open-paths)))
	  (append
	   (list 'ide-skel-imenu-internal-node-widget
		 :open open-status
		 :indent 0
		 :path path
		 :node (list 'push-button
			     :format "%[%t%]\n"
			     :button-face 'variable-pitch
			     :tag menu-title))
	   (delq nil (mapcar (lambda (elem)
			       (ide-skel-imenu-create-tree hash path elem))
			     sub-alist))))
      (let* ((index-name (car element))
	     (index-position (if (consp (cdr element)) (cadr element) (cdr element)))
	     (function (when (consp (cdr element)) (caddr element)))
	     (arguments (when (consp (cdr element)) (cdddr element)))
	     (path (concat prefix "/" index-name))
	     (node (gethash path hash))
	     (parent-path (cdr (assq :parent node)))
	     (widget (cdr (assq :widget node))))
	(unless parent-path
	  widget)))))

(defun ide-skel-imenu-compare (e1 e2)
  (let ((ce1 (and (consp (cdr e1)) (listp (cadr e1))))
	(ce2 (and (consp (cdr e2)) (listp (cadr e2)))))
    (when ce1
      (setcdr e1 (sort (cdr e1) 'ide-skel-imenu-compare)))
    (when ce2
      (setcdr e2 (sort (cdr e2) 'ide-skel-imenu-compare)))
    (if (or (and ce1 ce2)
	    (and (not ce1) (not ce2)))
	(string< (car e1) (car e2))
      (and ce1 (not ce2)))))

(defun ide-skel-outline-tree-create (index-alist)
  (let (stack
	node-list
	(current-depth 0))
    (dolist (element index-alist)
      (let ((index-name (car element))
	    (index-position (if (consp (cdr element)) (cadr element) (cdr element)))
	    (function (when (consp (cdr element)) (caddr element)))
	    (arguments (when (consp (cdr element)) (cdddr element))))
	;; (message "index-name: %S" index-name)
	(string-match "^\\([*]+\\)[ ]*\\(.*\\)$" index-name)
	(let* ((depth (length (match-string 1 index-name)))
	       (name (match-string 2 index-name))
	       parent-node
	       node)
	  (while (and stack
		      (>= (caar stack) depth))
	    (setq stack (cdr stack)))
	  (when stack
	    (setq parent-node (cdar stack))
	    (when (eq (car parent-node) 'ide-skel-imenu-leaf-widget)
	      (let ((path (plist-get (cdr parent-node) :path)))
		(setcar parent-node 'ide-skel-imenu-internal-node-widget)
		(setcdr parent-node (list :open (gethash path ide-skel-imenu-open-paths)
					  :indent 0
					  :notify 'ide-skel-tree-node-notify
					  :index-name (plist-get (cdr parent-node) :index-name)
					  :index-position (plist-get (cdr parent-node) :index-position)
					  :function (plist-get (cdr parent-node) :function)
					  :arguments (plist-get (cdr parent-node) :arguments)
					  :path path
					  :node (list 'push-button
						      :format "%[%t%]\n"
						      :button-face 'variable-pitch
						      :tag (plist-get (cdr parent-node) :tag)))))))
	  (setq node (list 'ide-skel-imenu-leaf-widget
			   :notify 'ide-skel-tree-node-notify
			   :index-name index-name
			   :index-position index-position
			   :function function
			   :path (concat (plist-get (cdr parent-node) :path) "/" index-name)
			   :arguments arguments
			   :tag name))
	  (push (cons depth node) stack)
	  (if parent-node
	      (setcdr (last parent-node) (cons node nil))
	    (push node node-list)))))
    (append
     (list 'ide-skel-imenu-internal-node-widget
	   :open t
	   :indent 0
	   :path ""
	   :tag "")
     (reverse node-list))))

(defun ide-skel-imenu-side-view-draw-tree (imenu-buffer &optional refresh)
  (with-current-buffer imenu-buffer
    (let ((index-alist (with-current-buffer ide-skel-imenu-editor-buffer
			(when refresh
			  (imenu--cleanup)
			  (setq imenu--index-alist nil))
			(cons "" (progn
				   (unless imenu--index-alist
				     (font-lock-default-fontify-buffer)
				     (condition-case err
					 (imenu--make-index-alist t)
				       (error nil)))
				   imenu--index-alist))))
	  (is-outline-mode (with-current-buffer ide-skel-imenu-editor-buffer
			     (or (eq major-mode 'outline-mode)
				 (and (boundp 'outline-minor-mode)
				      (symbol-value 'outline-minor-mode)))))
	  (inhibit-read-only t)
	  (hash (make-hash-table :test 'equal))
	  (start-line (save-excursion
			(goto-char (window-start ide-skel-current-right-view-window))
			(line-number-at-pos))))
      (unless is-outline-mode
	(when ide-skel-imenu-sorted
	  (setq index-alist (cons "" (sort (copy-tree (cdr index-alist)) 'ide-skel-imenu-compare))))
	(ide-skel-imenu-analyze hash "/" index-alist)
	(ide-skel-imenu-analyze2 hash "/" index-alist)
	(ide-skel-imenu-analyze3 hash "/" index-alist))
      (let ((tree (if is-outline-mode
		      (ide-skel-outline-tree-create (cdr index-alist))
		    (ide-skel-imenu-create-tree hash "/" index-alist))))
	(plist-put (cdr tree) :open t)
	(plist-put (cdr tree) :indent 0)
	(erase-buffer)
	(tree-widget-set-theme "small-folder")
	(widget-create tree)
	(set-keymap-parent (current-local-map) tree-widget-button-keymap)
	(widget-setup)
	(goto-line start-line)
	(beginning-of-line)
	(set-window-start ide-skel-current-right-view-window (point))))))

(defun ide-skel-imenu-side-view-window-function (side event &rest list)
  ;; (message "%S %S %S" side event list)
  (when (and (eq side 'right)
	     ide-skel-current-right-view-window)
    (let ((imenu-buffer (ide-skel-imenu-get-buffer-create ide-skel-current-editor-buffer t)))
      (when (memq event '(show editor-buffer-changed))
	(when (ide-skel-has-imenu ide-skel-current-editor-buffer)
	  (unless imenu-buffer
	    (setq imenu-buffer (ide-skel-imenu-get-buffer-create ide-skel-current-editor-buffer)))
	  (with-current-buffer imenu-buffer
	    (setq ide-skel-tabbar-enabled t))))
      (when (and imenu-buffer
		 (eq event 'tab-change)
		 (eq (cadr list) imenu-buffer))
	(with-current-buffer imenu-buffer
	  (when (= (buffer-size) 0)
	    (ide-skel-imenu-side-view-draw-tree imenu-buffer))))))
  nil)

(add-hook 'ide-skel-side-view-window-functions 'ide-skel-imenu-side-view-window-function)

;;; Info

(require 'info)

(defun ide-skel-info-get-buffer-create ()
  (let ((buffer (ide-skel-get-side-view-buffer-create " Ide Skel Info"
						      'left "Info" "Info browser"
						      (lambda (editor-buffer) t))))
    (with-current-buffer buffer
      (setq ide-skel-tabbar-menu-function
	    (lambda ()
	      (append
	       (list
		(list 'ide-skel-info-refresh "Refresh" t))))
	    ide-skel-info-open-paths (make-hash-table :test 'equal)
	    ide-skel-info-root-node (cons "Top" "(dir)top"))
      (add-hook 'tree-widget-after-toggle-functions (lambda (widget)
						      (let ((path (widget-get widget :path)))
							(when path
							  (if (widget-get widget :open)
							      (puthash path t ide-skel-info-open-paths)
							    (remhash path ide-skel-info-open-paths)))))
		nil t))
    buffer))

(defun ide-skel-info-file-open (widget &rest rest)
  (let ((path (widget-get widget :path)))
    (if (not (string-match "^(\\([^)]+\\))\\([^.]+\\)$" path))
	(error "Invalid node %s" path)
      (let ((filename (match-string 1 path))
	    (nodename (match-string 2 path))
	    (buffer (get-buffer "*info*"))
	    buffer-win)
	(unless buffer
	  (with-selected-window (ide-skel-get-last-selected-window)
	    (info)
	    (setq buffer (window-buffer (selected-window)))
	    (setq buffer-win (selected-window))))
	(unless buffer-win
	  (setq buffer-win (get-buffer-window buffer))
	  (unless buffer-win
	    (with-selected-window (ide-skel-get-last-selected-window)
	      (switch-to-buffer buffer)
	      (setq buffer-win (selected-window)))))
	(select-window buffer-win)
	(Info-find-node filename nodename)))))

(defun ide-skel-info-tree-expand-dir (tree)
  (let ((path (widget-get tree :path)))
    (condition-case err
	(mapcar 'ide-skel-info-tree-widget (Info-speedbar-fetch-file-nodes path))
      (error
       (message "%s" (error-message-string err))
       nil))))

(defun ide-skel-info-tree-widget (e)
  (let ((name (car e))
	(path (cdr e)))
    (if (condition-case err
	    (Info-speedbar-fetch-file-nodes path)
	  (error nil))
	(list 'ide-skel-info-tree-dir-widget
	      :path path
	      :help-echo name
	      :open (gethash path ide-skel-info-open-paths)
	      :node (list 'push-button
			  :tag name
			  :format "%[%t%]\n"
			  :notify 'ide-skel-info-file-open
			  :path path
			  :button-face 'variable-pitch
			  :help-echo name
			  :keymap tree-widget-button-keymap
			  ))
      (list 'ide-skel-info-tree-file-widget
	    :path path
	    :help-echo name
	    :keymap tree-widget-button-keymap
	    :tag name))))
	
(defun ide-skel-info-refresh (&optional show-top)
  (interactive)
  (with-current-buffer ide-skel-info-buffer
    (let ((inhibit-read-only t)
	  (start-line (save-excursion
			(goto-char (window-start ide-skel-current-left-view-window))
			(line-number-at-pos))))
      (erase-buffer)
      (tree-widget-set-theme "small-folder")
      (let ((tree (ide-skel-info-tree-widget ide-skel-info-root-node)))
	(plist-put (cdr tree) :open t)
	(widget-create tree))
      (set-keymap-parent (current-local-map) tree-widget-button-keymap)
      (widget-setup)
      (if show-top
	  (goto-char (point-min))
	(goto-line start-line))
      (beginning-of-line)
      (set-window-start ide-skel-current-right-view-window (point)))))

(defun ide-skel-info (root-node)
  (with-current-buffer ide-skel-info-buffer
    (clrhash ide-skel-info-open-paths)
    (setq ide-skel-info-root-node root-node)
    (ide-skel-info-refresh t)))

(defun ide-skel-info-side-view-window-function (side event &rest list)
  (when (and (eq side 'left) ide-skel-current-left-view-window)
    (cond ((eq event 'show)
	   (unless ide-skel-info-buffer
	     (setq ide-skel-info-buffer (ide-skel-info-get-buffer-create)))
	   (with-current-buffer ide-skel-info-buffer
	     (setq ide-skel-tabbar-enabled t)))
	  ((and (eq event 'tab-change)
		(eq (cadr list) ide-skel-info-buffer)
		(= (buffer-size ide-skel-info-buffer) 0))
	   (ide-skel-info-refresh))))
  nil)

(add-hook 'ide-skel-side-view-window-functions 'ide-skel-info-side-view-window-function)

;;; Dir tree

(defun ide-skel-dir-node-notify (widget &rest rest)
  (let ((path (widget-get widget :path)))
    (ide-skel-dir path)))

(defun ide-skel-file-open (widget &rest rest)
  (let ((path (widget-get widget :path)))
    (ide-skel-select-buffer path)))

(defun ide-skel-dir-tree-widget (e)
  "Return a widget to display file or directory E."
  (if (file-directory-p e)
      `(ide-skel-dir-tree-dir-widget
        :path ,e
	:help-echo ,e
	:open ,(gethash e ide-skel-dir-open-paths)
        :node (push-button
               :tag ,(file-name-as-directory
                      (file-name-nondirectory e))
               :format "%[%t%]\n"
               :notify ide-skel-dir-node-notify
	       :path ,e
	       :button-face (variable-pitch bold)
               :help-echo ,e
               :keymap        ,tree-widget-button-keymap  ; Emacs
               ))
    `(ide-skel-dir-tree-file-widget
      :path ,e
      :help-echo ,e
      :tag  ,(file-name-nondirectory e))))

(defun ide-skel-dir-get-buffer-create ()
  (let ((buffer (ide-skel-get-side-view-buffer-create " Ide Skel Dirs"
						      'left "Dirs" "Filesystem browser"
						      (lambda (editor-buffer) t))))
    (with-current-buffer buffer
      (setq ide-skel-tabbar-menu-function
	    (lambda ()
	      (append
	       (list
		(list 'ide-skel-dir-refresh "Refresh" t)
		(when (and (buffer-file-name ide-skel-current-editor-buffer)
			   (fboundp 'ide-skel-proj-get-project-create)
			   (funcall 'ide-skel-project-p (car (funcall 'ide-skel-proj-get-project-create (buffer-file-name ide-skel-current-editor-buffer)))))
		  (list 'ide-skel-dir-project "Show project tree" t))
		(list 'ide-skel-dir-home "Home" t)
		(list 'ide-skel-dir-filesystem-root "/" t)
		)))
	    ide-skel-dir-open-paths (make-hash-table :test 'equal)
	    ide-skel-dir-root-dir (file-truename (substitute-in-file-name "~")))
      (add-hook 'tree-widget-after-toggle-functions (lambda (widget)
						      (let ((path (widget-get widget :path)))
							(when path
							  (if (widget-get widget :open)
							      (puthash path t ide-skel-dir-open-paths)
							    (remhash path ide-skel-dir-open-paths)))))
		nil t))
    buffer))

(defun ide-skel-dir-tree-list (dir)
  "Return the content of the directory DIR.
Return the list of components found, with sub-directories at the
beginning of the list."
  (let (files dirs)
    (dolist (entry (directory-files dir 'full))
      (unless (string-equal (substring entry -1) ".")
        (if (file-directory-p entry)
            (push entry dirs)
          (push entry files))))
    (nreverse (nconc files dirs))))

(defun ide-skel-dir-tree-expand-dir (tree)
  "Expand the tree widget TREE.
Return a list of child widgets."
  (let ((dir (directory-file-name (widget-get tree :path))))
    (if (file-accessible-directory-p dir)
	(progn
	  (message "Reading directory %s..." dir)
	  (condition-case err
	      (prog1
		  (mapcar 'ide-skel-dir-tree-widget (ide-skel-dir-tree-list dir))
		(message "Reading directory %s...done" dir))
	    (error
	     (message "%s" (error-message-string err))
	     nil)))
      (error "This directory is inaccessible"))))

(defun ide-skel-select-dir-handler (event)
  (interactive "@e")
  (with-selected-window (posn-window (event-start event))
    (let* ((path (get-text-property (posn-point (event-start event)) 'path)))
      (ide-skel-dir path))))

(defun ide-skel-dir-refresh (&optional show-top)
  (interactive)
  (with-current-buffer ide-skel-dir-buffer
    (let ((inhibit-read-only t)
	  (start-line (save-excursion
			(goto-char (window-start ide-skel-current-left-view-window))
			(line-number-at-pos))))
      (erase-buffer)
      (let ((path-dirs (split-string (directory-file-name ide-skel-dir-root-dir) "[/\\]"))
	    (km (make-sparse-keymap))
	    path)
	(setq path-dirs (reverse (cdr (reverse path-dirs))))
	(define-key km [mouse-1] 'ide-skel-select-dir-handler)
	(while path-dirs
	  (let ((dir (car path-dirs)))
	    (when (and (> (current-column) 0)
		       (>= (+ (current-column) (length dir) 1) (window-width ide-skel-current-left-view-window)))
	      (insert "\n"))
	    (setq path (directory-file-name (concat path (format "/%s" dir))))
	    (unless (equal (char-before) ?/)
	      (insert "/"))
	    (insert (propertize dir
				'face 'bold
				'local-map km
				'mouse-face 'highlight
				'path path)))
	  (setq path-dirs (cdr path-dirs))))
      (insert "\n\n")
      (tree-widget-set-theme "small-folder")
      (let ((default-directory ide-skel-dir-root-dir)
	    (tree (ide-skel-dir-tree-widget (directory-file-name ide-skel-dir-root-dir))))
	(plist-put (cdr tree) :open t)
	(widget-create tree))
      (set-keymap-parent (current-local-map) tree-widget-button-keymap)
      (widget-setup)
      (if show-top
	  (goto-char (point-min))
	(goto-line start-line))
      (beginning-of-line)
      (set-window-start ide-skel-current-right-view-window (point))
      )))

(defun ide-skel-dir (root-dir)
  (with-current-buffer ide-skel-dir-buffer
    (clrhash ide-skel-dir-open-paths)
    (setq ide-skel-dir-root-dir (file-truename (substitute-in-file-name root-dir)))
    (ide-skel-dir-refresh t)))

(defun ide-skel-dir-project ()
  (interactive)
  (let ((root-dir (funcall 'ide-skel-project-root-path
			   (car (funcall 'ide-skel-proj-get-project-create (buffer-file-name ide-skel-current-editor-buffer))))))
    (message "Root dir: %S" root-dir)
    (ide-skel-dir root-dir)))

(defun ide-skel-dir-home ()
  (interactive)
  (ide-skel-dir "~"))

(defun ide-skel-dir-filesystem-root ()
  (interactive)
  (ide-skel-dir "/"))

(defun ide-skel-dirs-side-view-window-function (side event &rest list)
  (when (and (eq side 'left) ide-skel-current-left-view-window)
    (cond ((eq event 'show)
	   (unless ide-skel-dir-buffer
	     (setq ide-skel-dir-buffer (ide-skel-dir-get-buffer-create)))
	   (with-current-buffer ide-skel-dir-buffer
	     (setq ide-skel-tabbar-enabled t)))
	  ((and (eq event 'tab-change)
		(eq (cadr list) ide-skel-dir-buffer)
		(= (buffer-size ide-skel-dir-buffer) 0))
	   (ide-skel-dir-refresh))))
  nil)

(add-hook 'ide-skel-side-view-window-functions 'ide-skel-dirs-side-view-window-function)

(easy-menu-add-item nil nil ide-skel-project-menu t)

(defun ide-skel-proj-insert-with-face (string face)
  (let ((point (point)))
    (insert string)
    (let ((overlay (make-overlay point (point))))
      (overlay-put overlay 'face face))))

(defun ide-skel-mode-name-stringify (mode-name)
  (let ((name (format "%s" mode-name)))
    (replace-regexp-in-string "-" " "
			      (capitalize
			       (if (string-match "^\\(.*\\)-mode" name)
				   (match-string 1 name)
				 name)))))

(defun ide-skel-proj-get-all-dirs (root-dir)
  (condition-case err
      (split-string (shell-command-to-string (format "find %s -type d | grep -v '/CVS\\|/\\.svn'" root-dir))
		    "\n" t)
    (error nil)))

(defun ide-skel-shell ()
  (interactive)
  (when (fboundp 'ide-skel-show-bottom-view-window)
    (funcall 'ide-skel-show-bottom-view-window)
    (select-window (or (funcall 'ide-skel-get-bottom-view-window)
		       (selected-window)))
    (ansi-term (or (getenv "ESHELL") (getenv "SHELL")))))

(defun ide-skel-project-menu (menu)
  (let* ((curbuf-file (buffer-file-name (current-buffer)))
	 (curbuf-mode-name (when (and (buffer-file-name (current-buffer))
				      (ide-skel-mode-file-regexp-list (list major-mode)))
			     (ide-skel-mode-name-stringify major-mode))))
    (condition-case err
	(append
	 (when curbuf-mode-name
	   (list (vector (format "Search for %s file..." curbuf-mode-name) 'ide-skel-proj-find-files-by-regexp curbuf-mode-name)))
	 (list (vector "Search for file..." 'ide-skel-proj-find-text-files-by-regexp curbuf-mode-name))
	 (when curbuf-mode-name
	   (list (vector (format "Grep %s files..." curbuf-mode-name) 'ide-skel-proj-grep-files-by-regexp curbuf-mode-name)))
	 (list (vector "Grep files..." 'ide-skel-proj-grep-text-files-by-regexp curbuf-file))
	 (list (vector "Shell" 'ide-skel-shell t)))
      (error (message (error-message-string err))))))

;; (ide-skel-project . relative-path) jesli path nalezy do projektu,
;; (qdir . filename) wpp

(defun ide-skel-proj-get-project-create (path)
  (let ((path (file-truename (substitute-in-file-name path)))
	dir)
    (if (file-directory-p path)
	(progn
	  (setq path (file-name-as-directory path))
	  (setq dir path))
      (setq dir (file-name-as-directory (file-name-directory path))))
    ;; path - true, qualified file name (no environment variables, ~, links)
    (let ((project (some (lambda (project)
			   (let ((root-dir (ide-skel-project-root-path project)))
			     (when (string-match (concat "^" (regexp-quote root-dir)) path)
			       project)))
			 ide-skel-projects)))
      (when project
	(setq dir (ide-skel-project-root-path project)))
      ;; there is no such project
      (unless project
	(let ((last-project-dir dir)
	      (dir-list (split-string dir "/"))
	      is-project)
	  ;; there is no root dir
	  (while (directory-files dir t (concat "^" ide-skel-cvs-dir-regexp) t)
	    (setq is-project t
		  last-project-dir (file-name-as-directory dir)
		  dir (file-name-as-directory (file-name-directory (directory-file-name dir)))))
	  (when is-project
	    (let ((list (nthcdr (1- (length (split-string last-project-dir "/"))) dir-list)))
	      (cond ((equal (car list) "trunk")
		     (setq last-project-dir (concat last-project-dir "trunk/")))
		    ((member (car list) '("branches" "tags"))
		     (setq last-project-dir (concat last-project-dir (car list) "/" (when (cdr list) (concat (cadr list) "/")))))
		    (t)))
	    (setq project (make-ide-skel-project :root-path last-project-dir
						 :include-file-path (ide-skel-proj-get-all-dirs last-project-dir))
		  dir last-project-dir)
	    (push project ide-skel-projects))))
      (list (or project dir) (file-relative-name path dir) path))))

(defun ide-skel-proj-get-root (proj-or-dir)
  (when proj-or-dir
    (directory-file-name (file-truename (substitute-in-file-name 
					 (if (ide-skel-project-p proj-or-dir)
					     (ide-skel-project-root-path proj-or-dir)
					   proj-or-dir))))))
	  
(defun ide-skel-proj-find-files (dir file-predicate &optional dir-predicate)
  "Return list of all qualified file paths in tree dir with root
DIR, for which FILE-PREDICATE returns non-nil.  We will go into
directory only if DIR-PREDICATE returns non-nil or DIR-PREDICATE *is* nil."
  (setq dir (file-name-as-directory (file-truename (substitute-in-file-name dir))))
  (let (result-list)
    (mapcar (lambda (path)
	      (if (file-directory-p path)
		  (when (and (file-accessible-directory-p path)
			     (or (null dir-predicate)
				 (funcall dir-predicate path)))
		    (setq result-list (append result-list (ide-skel-proj-find-files path file-predicate dir-predicate))))
		(when (or (null file-predicate)
			  (funcall file-predicate path))
		  (push path result-list))))
	    (delete (concat (file-name-as-directory dir) ".") 
		    (delete (concat (file-name-as-directory dir) "..")
			    (directory-files dir t nil t))))
    result-list))

(defun ide-skel-root-dir-for-path (path)
  (let (root-dir)
    (setq root-dir (car (ide-skel-proj-get-project-create path)))
    (unless (stringp root-dir)
      (setq root-dir (ide-skel-project-root-path root-dir)))
    root-dir))

(defun ide-skel-has-imenu (&optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (or (and imenu-prev-index-position-function
	     imenu-extract-index-name-function)
	imenu-generic-expression
	(not (eq imenu-create-index-function
		 'imenu-default-create-index-function)))))

(defun ide-skel-mode-file-regexp-list (mode-symbol-list)
  (delq nil (mapcar (lambda (element)
		      (let ((fun-name (if (listp (cdr element)) (cadr element) (cdr element))))
			(when (memq fun-name mode-symbol-list) (cons (car element) fun-name))))
		    auto-mode-alist)))

(defun ide-skel-find-project-files (root-dir mode-symbol-list predicate)
  (let ((obj-file-regexp-list (delq nil (mapcar (lambda (element)
						  (let ((len (length element)))
						    (unless (and (> len 0)
								 (equal (elt element (1- len)) ?/))
						      (concat (regexp-quote element) "$"))))
						(append ide-skel-proj-ignored-extensions completion-ignored-extensions))))
	(mode-file-regexp-list (ide-skel-mode-file-regexp-list mode-symbol-list))) ; (file-path-regexp . major-mode-function-symbol)
    (when (and mode-symbol-list
	       (not mode-file-regexp-list))
      (error (format "No rules for %s major modes in auto-mode-alist." (mapconcat 'identity mode-symbol-list ", "))))
    (ide-skel-proj-find-files root-dir
			      (lambda (file-name)
				(and (not (string-match "#" file-name))
				     (not (string-match "semantic.cache" file-name))
				     (or (and (not mode-symbol-list)
					      (not (some (lambda (regexp)
								  (string-match regexp file-name))
								obj-file-regexp-list)))
					 (and mode-symbol-list
					      (some (lambda (element)
						      (let ((freg (if (string-match "[$]" (car element))
								      (car element)
								    (concat (car element) "$"))))
							(when (string-match freg file-name)
							  (cdr element))))
						    mode-file-regexp-list)))
				     (or (not predicate)
					 (funcall predicate file-name))))
			      (lambda (dir-path)
				(not (string-match (concat "/" ide-skel-cvs-dir-regexp) dir-path))))))

(defun ide-skel-proj-find-text-files-by-regexp ()
  (interactive)
  (unwind-protect
      (progn
	(setq ide-skel-all-text-files-flag t)
	(call-interactively 'ide-skel-proj-find-files-by-regexp))
    (setq ide-skel-all-text-files-flag nil)))

(defun ide-skel-proj-grep-text-files-by-regexp ()
  (interactive)
  (unwind-protect
      (progn
	(setq ide-skel-all-text-files-flag t)
	(call-interactively 'ide-skel-proj-grep-files-by-regexp))
    (setq ide-skel-all-text-files-flag nil)))

(defun ide-skel-proj-grep-files-by-regexp (root-dir mode-symbol-list regexp)
  (interactive (let* ((path (buffer-file-name (current-buffer)))
		      (all-text-files (or ide-skel-all-text-files-flag
					  (consp current-prefix-arg)))
		      (whatever (progn
				  (when (and (not all-text-files)
					     (not (ide-skel-mode-file-regexp-list (list major-mode))))
				    (error (format "No rules for %s major mode in auto-mode-alist" (symbol-name major-mode))))
				  (unless path
				    (error "Current buffer (%s) is not visiting any project file" (buffer-name (current-buffer))))))
		      (root-dir (when path (ide-skel-root-dir-for-path path)))
		      (thing (let ((res (thing-at-point 'symbol)))
			       (set-text-properties 0 (length res) nil res)
			       res))
		      (chunk (let ((result (read-string (concat (if root-dir (format "Root dir is %s. " root-dir) "")
								(format "Search in %s files. Regexp%s: "
									(if all-text-files
									    "all text"
									  (ide-skel-mode-name-stringify major-mode))
									(if thing (format " (default %s)" thing) "")))
							nil ide-skel-proj-grep-project-files-history thing)))
			       (if (and result (> (length result) 0))
				   result
				 (error "Regexp cannot be null")))))
		 (list root-dir (unless all-text-files (list major-mode)) chunk)))
  (let* ((paths (ide-skel-find-project-files root-dir mode-symbol-list (lambda (path) t)))
	 (temp-file-path (concat (file-name-as-directory temporary-file-directory) (make-temp-name "ide-"))))
    (unless paths
      (error "No files to grep"))
    ;; create temporary file with file paths to search
    (with-temp-file temp-file-path
      (dolist (path paths)
	;; save buffer if is open
	(let ((buffer (get-file-buffer path)))
	  (when (and buffer
		     (buffer-live-p buffer))
	    (with-current-buffer buffer
	      (save-buffer))))
	(setq path (concat "./" (file-relative-name path (file-name-as-directory root-dir))))
	(insert (concat "'" path "'\n"))))
    (let* ((default-directory root-dir)
	   (grep-command (format "cat %s | xargs grep -n %s" temp-file-path regexp)))
      (setq ide-skel-proj-grep-header (list root-dir
					    (if mode-symbol-list
						(mapconcat (lambda (sym) (ide-skel-mode-name-stringify sym)) mode-symbol-list ", ")
					      "all text")
					    regexp))
      (grep grep-command))
    ;; delete file after some time, because grep is executed as external process
    (run-with-idle-timer 5 nil (lambda (file-path)
				 (condition-case nil
				     nil ; (delete-file file-path)
				   (error nil)))
			 temp-file-path)))

(defun ide-skel-proj-find-files-by-regexp (root-dir mode-symbol-list name-regexp &optional case-sensitive)
  "Search directory tree with root in ROOT-DIR and returns
qualified paths to files which after open in Emacs would have one
of modes in MODE-SYMBOL-LIST (if list is empty, we will take all
text files) and their name (without dir) matches NAME-REGEXP."
  (interactive (let* ((path (buffer-file-name (current-buffer)))
		      (all-text-files (or ide-skel-all-text-files-flag
					  (consp current-prefix-arg)))
		      (whatever (progn
				  (when (and (not all-text-files)
					     (not (ide-skel-mode-file-regexp-list (list major-mode))))
				    (error (format "No rules for %s major mode in auto-mode-alist" (symbol-name major-mode))))
				  (unless path
				    (error "Current buffer (%s) is not visiting any project file" (buffer-name (current-buffer))))))
		      (root-dir (when path (ide-skel-root-dir-for-path path)))
		      (chunk (read-string (concat (if root-dir (format "Root dir is %s. " root-dir) "")
						  (if all-text-files
						      "F"
						    (concat (ide-skel-mode-name-stringify major-mode) " f"))
						  (format "ile name regexp: " ))
					  nil ide-skel-proj-find-project-files-history nil)))
		 (list root-dir (unless all-text-files (list major-mode)) chunk)))
  (let* ((paths (ide-skel-find-project-files root-dir mode-symbol-list
					    (lambda (path)
					      (let ((case-fold-search (not case-sensitive)))
						(or (not name-regexp)
						    (string-match name-regexp (file-name-nondirectory path)))))))
	 (buffer (get-buffer-create ide-skel-proj-find-results-buffer-name))
	 (saved-window (cons (selected-window) (window-buffer (selected-window)))))
    (if (= (length paths) 1)
	(find-file (car paths))
      (save-selected-window
	(save-excursion
	  (set-buffer buffer)
	  (setq buffer-read-only nil
		default-directory root-dir)
	  (erase-buffer)

	  (insert "Root dir: ")
	  (ide-skel-proj-insert-with-face root-dir 'font-lock-keyword-face)
	  (insert "; Range: ")
	  (ide-skel-proj-insert-with-face
	   (if mode-symbol-list
	       (mapconcat (lambda (sym) (ide-skel-mode-name-stringify sym)) mode-symbol-list ", ")
	     "all text")
	   'font-lock-keyword-face)
	  (insert " files; Regexp: ")
	  (ide-skel-proj-insert-with-face name-regexp 'font-lock-keyword-face)
	  (insert "; Case sensitive: ")
	  (ide-skel-proj-insert-with-face (if case-sensitive "Yes" "No") 'font-lock-keyword-face)
	  (insert "\n\n")
	  (compilation-minor-mode 1)
	  (let ((invisible-suffix ":1:1 s"))
	    (put-text-property 0 (length invisible-suffix) 'invisible t invisible-suffix)
	    (dolist (path paths)
	      (let ((relative-path (file-relative-name path root-dir)))
		(put-text-property 0 (length relative-path) 'mouse-face 'highlight relative-path)
		(insert relative-path)
		(insert invisible-suffix)
		(insert "\n"))))
	  (insert (format "\n%d files found." (length paths)))
	  (goto-char (point-min))
	  (setq buffer-read-only t)
	  (when (and paths (fboundp 'compile-reinitialize-errors) (funcall (symbol-function 'compile-reinitialize-errors) t)))
	  (switch-to-buffer-other-window buffer)
	  (goto-line 1)
	  (goto-line 3)))
      (if (window-live-p (car saved-window))
	  (select-window (car saved-window))
	(when (get-buffer-window (cdr saved-window))
	  (select-window (get-buffer-window (cdr saved-window))))))))

(unless ide-skel-proj-grep-mode-map
  (setq ide-skel-proj-grep-mode-map (make-sparse-keymap))
  (define-key ide-skel-proj-grep-mode-map "r" 'ide-skel-proj-grep-replace))

(defun ide-skel-proj-grep-replace ()
  (interactive)
  (let ((replace-to (read-string "Replace to: " nil 'ide-skel-proj-grep-replace-history))
	(current-pos 1)
	begin end
	buffers-to-revert
	replace-info)
    (save-excursion
      (while current-pos
	(setq current-pos (next-single-property-change current-pos 'font-lock-face (current-buffer)))
	(when (and current-pos
		   (eq (get-text-property current-pos 'font-lock-face) 'match))
	  (setq begin current-pos)
	  (setq current-pos (next-single-property-change current-pos 'font-lock-face (current-buffer)))
	  (setq end current-pos)
	  (save-excursion
	    (goto-char begin)
	    (beginning-of-line)
	    (let ((begline (point)))
	      (re-search-forward "^\\(.*\\):\\([0-9]+\\):" nil t)
	      (let ((len (length (match-string 0)))
		    (file-path (expand-file-name (substring-no-properties (match-string 1)) default-directory)))
		(when (get-file-buffer file-path)
		  (push (get-file-buffer file-path) buffers-to-revert))
		(push (list file-path
			    (string-to-number (match-string 2))
			    (- begin begline len)
			    (- end begline len))
		      replace-info)))))))
    (dolist (replacement replace-info)
      (let ((file-path (nth 0 replacement))
	    (line-no (nth 1 replacement))
	    (from-column-no (nth 2 replacement))
	    (to-column-no (nth 3 replacement)))
	(condition-case err
	    (with-temp-file file-path
	      (insert-file-contents file-path)
	      (goto-line line-no)
	      (forward-char from-column-no)
	      (delete-region (point) (+ (point) (- to-column-no from-column-no)))
	      (insert replace-to))
	  (error (message "%s" (error-message-string err))))))
    (dolist (buffer buffers-to-revert)
      (when (buffer-live-p buffer)
	(with-current-buffer buffer
	  (revert-buffer t t t)))) ; ignore-auto, nonconfirm, preserve-modes
    (message "Done.")))

(define-minor-mode ide-skel-proj-grep-mode
  ""
  nil ; init value
  nil ; mode indicator
  ide-skel-proj-grep-mode-map ; keymap
  ;; body
  (unless (assq 'ide-skel-proj-grep-mode minor-mode-map-alist)
    (push (cons 'ide-skel-proj-grep-mode ide-skel-proj-grep-mode-map) minor-mode-map-alist)))

(add-hook 'grep-setup-hook (lambda ()
			     (when ide-skel-proj-grep-header
			       (ide-skel-proj-grep-mode 1)
			       (unwind-protect
				   (progn
				     (setq buffer-read-only nil)
				     (erase-buffer)
				     (remove-overlays)
				     (insert "Root dir: ")
				     (ide-skel-proj-insert-with-face (car ide-skel-proj-grep-header) 'font-lock-keyword-face)
				     (insert "; Range: ")
				     (ide-skel-proj-insert-with-face (cadr ide-skel-proj-grep-header) 'font-lock-keyword-face)
				     (insert " files; Regexp: ")
				     (ide-skel-proj-insert-with-face (caddr ide-skel-proj-grep-header) 'font-lock-keyword-face)
				     (insert "\n")
				     (insert "mouse-1 toggle match; r replace matches")
				     (insert "\n\n"))
				 (setq buffer-read-only t
				       ide-skel-proj-grep-header nil)
				 (setq ide-skel-proj-old-compilation-exit-message-function (symbol-value 'compilation-exit-message-function))
				 (set 'compilation-exit-message-function
				       (lambda (status code msg)
					 (let ((result (if ide-skel-proj-old-compilation-exit-message-function
							   (funcall ide-skel-proj-old-compilation-exit-message-function
								    status code msg)
							 (cons msg code))))
					   (save-excursion
					     (goto-char (point-min))
					     (let (begin
						   end
						   (km (make-sparse-keymap))
						   (inhibit-read-only t))
					       (define-key km [down-mouse-1] 'ignore)
					       (define-key km [mouse-1] 'ide-skel-proj-grep-click)
					       (while (setq begin (next-single-property-change (point) 'font-lock-face (current-buffer) nil))
						 (setq end (next-single-property-change begin 'font-lock-face (current-buffer) nil))
						 (put-text-property begin end 'pointer 'hand)
						 (put-text-property begin end 'local-map km)
						 (goto-char end))))
					   result)))))))

(defun ide-skel-proj-grep-click (event)
  (interactive "@e")
  (with-selected-window (posn-window (event-start event))
    (let* ((posn-point (posn-point (event-start event)))
	   (begin (or (and (not (get-text-property (1- posn-point) 'font-lock-face))
			   posn-point)
		      (previous-single-property-change posn-point 'font-lock-face (current-buffer) nil)))
	   (end (next-single-property-change posn-point 'font-lock-face (current-buffer) nil))
	   (font-lock-face (get-text-property posn-point 'font-lock-face))
	   (inhibit-read-only t))
      (put-text-property begin end 'font-lock-face (if (eq font-lock-face 'match) 'widget-field 'match)))))

(defun ide-skel-proj-change-buffer-hook-function ()
  (let ((path (buffer-file-name)))
    (when path
      (condition-case err
	  (let ((project-list (ide-skel-proj-get-project-create path)))
	    (when (ide-skel-project-p (car project-list))
	      (setq PC-include-file-path (ide-skel-project-include-file-path (car project-list)))))
	(error nil)))))

(add-hook 'ide-skel-editor-buffer-changed-hook 'ide-skel-proj-change-buffer-hook-function)

(tabbar-mode 1)

(provide 'ide-skel)

