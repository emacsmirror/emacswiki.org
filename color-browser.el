;;; color-browser.el --- A utility for designing Emacs color themes.

;; Copyright (C) 2002, 2003 by Free Software Foundation, Inc.

;; Author: Kahlil Hodgson <dorge@tpg.com.au>
;; X-URL: http://www.emacswiki.org/elisp/
;; Keywords: convenience
;; Created on: <2002-10-29 07:34:36 kahlil>
;; Time-stamp: <2002-12-03 17:20:40 kahlil>
;; Version: 0.3

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; This file is not yet part of GNU Emacs.

;;;_ * Commentary

;; A utility for designing Emacs color themes.

;; Designing a good (or even good enough) color theme for Emacs can be
;; extremely time consuming, since we typically have to consider
;; hundreds of colors, hundreds of faces, and a number variations in
;; weight, slant and size.  The `color-theme' package takes a
;; community based approach to this problem: investing lots of time
;; and effort is worthwhile if everybody gets to share the results.
;; Unfortunately, good quality color themes are still a little thin on
;; the ground.

;; This package provides a tool that can (hopefully) aid in the
;; production of quality color themes.  Basically it allows the user
;; to

;; (1) develop sets of colors (palettes) that work well together,

;; (2) use those palettes to quickly select and set the properties of
;; key face groups, and

;; (3) save and manipulate themes and palettes under development.

;; To use the tool simply load this file (which may take a while) and
;; type

;;     M-x color-browser

;; This command places the color browser in "Theme Builder" mode and
;; generates a frame containing two buffers: the "Face Group" buffer
;; and the "Color Palette" buffer.  (The dimensions of this frame are
;; determined by the customizable variables `cb-frame-width' and
;; `cb-frame-height').


;; The "Face Group" buffer
;; -----------------------

;; This buffer contains a collection of face names each presented in
;; their corresponding face.  Each of these names is active -- either
;; press [mouse-2] over (or [return] on) any of these names and the
;; value of `cb-active-face' will be set to the face under the point.
;; This is the primary function of this buffer -- active text in other
;; buffers and menus act on the the face corresponding to the value of
;; `cb-active-face'.  The currently active face name will be enclosed
;; in square brackets. (Also if you have click and hold over the face
;; name name, the current color properties of that face will be
;; displayed in the mode-line).

;; The mode-line of the "Face Group" buffer also contains three buttons
;; (active text). Clicking with [mouse-2] on each of these will
;; produce one of the following pop up menus:

;; (1) <Props>: a menu of common font properties (bold, italic,
;; underline for the moment) that can be applied to the current active
;; face.

;; (2) <Group>: a menu of available face groups. Selecting any of
;; these will display the face names in that group in the
;; "Face Group" buffer.

;; (3) <Theme>: a menu of available themes, and a provision to save
;; the current face settings as a theme (see `color-theme').
;; Selecting any of the themes will change the current face settings
;; (as per `color-theme'). Theme files are all stored under the
;; directory `cb-themes-dir' (customizable variable) and have a
;; "-theme.el" suffix.


;; The "Color Palette" buffer
;; --------------------------
;;
;; This buffer contains a list of colors displayed in either
;; foreground and background form.  Pressing [mouse-2] over (or
;; [return] on) any of the any of the foreground (or background) color
;; names will set the `:foreground' or `:background' color property of
;; the currently active face respectively.

;; Note that the face names in the "Face Group" buffer will reflect
;; these changes, as will any text in other buffers, and any such
;; change will be indicated by a message in the header line of the
;; "Face Group" buffer.

;; Preceding each set of color is a "[DEL]" button, selecting which
;; will remove that color set from the display.  Following each set of
;; color is an "[UP]" button, selecting which will move that color set
;; up one in the display.  Selecting "[UP]" for the first color set in
;; the buffer will move that color set to the bottom of the
;; buffer. (If you make a mistake, you can undo any previous deletions
;; with [C-/]).  The "unspecified" fields at the top of this buffer
;; allow you to unset the respective color properties.

;; If any of the colors in the displayed palette have been deleted or
;; rearranged then a message indicating this will be displayed in that
;; buffers header line.

;; The mode-line of the "Color Palette" buffer contains the following
;; buttons (active text).  Clicking with [mouse-2] on each of these will
;; produce one of the following effects:

;; (1) <Palette>: pops up a menu of available color palettes with the
;; currently displayed palette indicated.  Selecting any of these will
;; change the "Color Palette" buffer to display the colors in that
;; palette.  The last entry in the color palette menu ("Save Palette")
;; allows you to save the currently displayed set of colors as a
;; palette that will be loaded every time the color browser is
;; started.  These palettes are stored in files beneath the directory
;; `cb-palettes-dir' (a customizable variable).

;; (2) <Foreground> or <Background>: toggle the color display between
;; foreground and background layers.

;; (3) <TB> or <PB>: toggle between "Theme Builder" mode (described
;; above) and "Palette Builder" mode (described bellow).


;; Palette Builder mode
;; --------------------
;;
;; In this mode the "Face Group" and "Color Palette" buffers are
;; replaced by the "New Palette" and "Old Palette" buffers. These
;; buffers will be initially displaying "No-Colors" and "All-Colors"
;; palettes respectively.  These two new buffers are just like the
;; "Color Palette" buffer except that selecting any of the color names
;; in the "New Palette" buffer simply causes that name to be printed
;; in the echo area, and selecting any of the color names in the
;; "Color Palette" buffer causes the corresponding line to be copied
;; to the "New Palette" buffer.  This way new palettes can be
;; generated from one or more existing palettes and save for future
;; use.


;; Finally to quit from the Color Browser simply type "q" when the
;; frame is selected.  Also note that all buffers allow you to "undo"
;; the previous change just like other Emacs buffers although changes
;; are restricted to those generated by the active text.


;; Phew! Enjoy!
;;
;; Kal;-)

;;;_ * Installation:..........................................

;; Simply place the following somewhere in your .emacs file:

;;    (autoload 'color-browser "color-browser"
;;         "A utility for designing Emacs color themes" 'interactive)

;; or load this file and run the command
;;
;;    M-x color-browser
;;
;; Note, however, that calling `color-browser' will load a large
;; number of dependent packages, bloating your emacs session somewhat,
;; so consider restarting emacs after you have finished using the
;; tool.

;; Also see the customizable variables `cb-palettes-dir',
;; `cb-themes-dir', and `cb-frame-width'.

;;;_ * To Do:

;; 0. Get color-theme to provide `cb-current-theme'
;; 0. Get fix for resizing of frame whenever color-theme-install is called

;; 1. Setting font sizes? Say under props menu.
;; 2. Save this theme/palette, save theme/palette as,
;; updating other palette buffers after a save (do a quick walk)

;; I originally tried to implemented this using the `widget' package
;; but was unable attain the same level of the functionality.  After
;; much scouring of the `widget' code I came to the realization that I
;; would probably have to design a whole new set of widgets to get
;; this to work.  Perhaps another "free weekend" project:-)

;; 4. XEmacs compatibility. Eeeck!

;;;_ * Known Bugs:............................................

;; 1. Occasional problems with mouse in mode-line.
;; 2. Probably not XEmacs compatible.

;;;_ * Credits:

;; Initially based on the `list-colors-display' code.  Inspired by
;; `color-theme', coffee, sore eyes and frustration:-)

;;;_ * Code:

(require 'color-theme)

(unless window-system
  (error "Sorry, you need a window system to run color-browser"))


(defcustom cb-palettes-dir "~/.emacs/palettes/"
  "Directory in which new palette files should be placed.
Usually best if this is different from `cb-themes-dir'. Must end in /.
Will be created if it does not already exist."
  :type 'string
  :group 'color-browser)

(defcustom cb-themes-dir "~/.emacs/themes/"
  "Directory in which new color theme files will be placed and searched
for.  Will be created if it does not already exist."
  :type 'string
  :group 'color-browser)

(defcustom cb-frame-width 24
  "Desired width of the Color Browser tool.  Resizing the frame will
usually produce an ugly result.  Change this variable and reload
instead."
  :type 'number
  :group 'color-browser)

(defcustom cb-frame-height 47
  "Desired height of the Color Browser tool.  Resizing the frame will
usually produce an ugly result.  Change this variable and reload
instead."
  :type 'number
  :group 'color-browser)

;;;_  + Utilities

(eval-and-compile
  ;; These variables are referenced during load
  (defvar cb-active-face 'default
    "The face that the next color/property setting will act on.
Referenced by most key-map bindings")

  (defvar cb-current-group "Basic"
    "The name of the current face group displayed."))

(defvar cb-save-palette-hist nil
  "Symbol used to store history of recently saved palettes.")

(defvar cb-save-theme-hist nil
  "Symbol used to store history of recently saved themes.")

(defun cb-read-name (prompt alist init-input history)
  "Temporally raise the minibuffer frame and read a string (one that
can be interned as a symbol) prompting with PROMPT, completing against
ALIST, using INIT-INPUT as the initial input string, and dereferencing
history events using HISTORY.  This is used by functions normally called
via a mouse event."
  (interactive)
  (raise-frame (window-frame (minibuffer-window)))
  (let ((frame (selected-frame))
	(result (completing-read prompt alist nil nil init-input history)))
    (raise-frame frame)
    result))

;; The following variables are used to hold the Color Browser buffers
;; and are referenced by a variety of functions.  The values are set
;; by `cb-initialize'.

(defvar cb-face-group-buffer  nil)
(defvar cb-current-palette-buffer nil)
(defvar cb-old-palette-buffer nil)
(defvar cb-new-palette-buffer nil)

(defvar cb-theme-builder-p nil
  "Non-nil if Color Browser is in Theme Builder mode.  Otherwise the
Color Browser is in Palette Builder mode.")

;; Here's a shot at some OO design.  Each buffer knows how to redraw
;; itself etc
(defvar cb-name ""
  "Buffer local variable holding holding the name of the currently
displayed palette or face group.  This is used by the dynamic
mode-line.")
(make-variable-buffer-local 'cb-name)

(defvar cb-list nil
  "Buffer local variable holding the list of objects displayed in the
current buffer.  This is used by the `cb-redraw-function' to draw the
buffer.")
(make-variable-buffer-local 'cb-list)

(defvar cb-default nil
  "Buffer local variable holding the default list of objects to be
displayed in the current buffer.  This is used by the
`cb-redraw-function' to unconditionally redraw the buffer.")
(make-variable-buffer-local 'cb-default)

(defvar cb-map nil
  "Buffer local variable holding the key-map used by the buffer.")
(make-variable-buffer-local 'cb-map)

(defvar cb-action nil
  "Buffer local variable holding the function called by events in the buffer.")
(make-variable-buffer-local 'cb-action)

(defvar cb-layer nil
  "Buffer local variable holding the name of the layer displayed in
the buffer.")
(make-variable-buffer-local 'cb-layer)

(defvar cb-layer-map nil
  "Buffer local variable holding the key-map that toggles the layer
displayed in the buffer.")
(make-variable-buffer-local 'cb-layer-map)

;;;_  + Face Group Tool.......................................

;; For convenience we split our collection of faces up into various
;; face groups.  Each of these groups (except for the "Minor" group)
;; contains a set of faces that are likely to occur in a particular
;; buffer at the same time.  A listing of these faces is displayed in
;; the "Face Group" buffer.

;; To implement this we create a separate variable for each face group
;; which holds the details of that group -- a list of all the faces in
;; that group, any libraries that may need to be loaded for those
;; faces to be defined, and some details that can be used to
;; abbreviate those face names. NB: we include both Emacs and XEmacs
;; variants in the face lists.

;; We switch between face groups via a pop-menu which is activated
;; from the mode-line of the "Face Groups" buffer. This pop-up menu is
;; implemented using a prefix-map `cb-face-groups-menu' which is set
;; as the key-map property of the relevant text in the mode-line.

(defvar cb-face-groups-menu (make-sparse-keymap "Face Groups")
 "Prefix keymap used to generate the \"Group:\" mode-line menu.")

;;;_   - Face Groups

(defun cb-define-if-required (name plist &optional package-list)
  "Define a face group plist for NAME and PLIST, and a corresponding
  key in the prefix-map `cb-face-groups-menu'.  If PACKAGE-LIST is
  non-nil, do this only if we can load each package in that list.
  Returns the associated plist symbol.  See
  `cb-build-face-group-buffer' for details of how this plist is used."

  (when (or (null package-list)
	    ;; load all packages in list or return nil
	    (condition-case nil
		(dolist (package package-list 'return-value)
		  (require package))
	      (error nil)))

    (let ((symbol (intern (concat "cb-" name "-faces-plist"))))

      ;; define the plist the hard way:-)
      (set symbol plist)
      (put symbol 'variable-documentation
	   (concat "A plist of " (upcase name)
		   " faces arranged according to level."))

      ;; define a key in the prefix map
      (define-key cb-face-groups-menu (vector symbol)
	`(menu-item ,name
		    (lambda ()
		      (interactive)
		      (cb-change-group (quote ,symbol)))
		    :button (:radio
			     . (string-equal cb-current-group ,name))
		    ))
      symbol
      )))

;; The order in which keys in the prefix-map are defined will be the
;; reverse of the order in which they will appear in the corresponding
;; menu, so we define the less interesting ones first.
;; NB: we include both Emacs and XEmacs variants in the face lists.

(cb-define-if-required
 "RPM Spec"
 (list 'pre "rpm-spec-" 'post "-face"
       'faces '(rpm-spec-tag-face
		rpm-spec-macro-face
		rpm-spec-doc-face
		rpm-spec-dir-face
		rpm-spec-package-face
		rpm-spec-ghost-face))
 '(rpm-spec-mode))

;; (cb-define-if-required
;;  "Viper"
;;  (list 'pre "viper-" 'post "-face"
;;        'faces '(viper-search-face
;;		viper-replace-overlay-face
;;		viper-minibuffer-emacs-face
;;		viper-minibuffer-insert-face
;;		viper-minibuffer-vi-face))
;;  '(viper))

;; (cb-define-if-required
;;  "EBrowse"
;;  (list 'pre "ebrowse-" 'post "-face"
;;        'faces  '(ebrowse-tree-mark-face
;;		 ebrowse-root-class-face
;;		 ebrowse-file-name-face
;;		 ebrowse-default-face
;;		 ebrowse-member-attribute-face
;;		 ebrowse-member-class-face
;;		 ebrowse-progress-face))
;;  '(ebrowse))

;; (cb-define-if-required
;;  "Antlr"
;;  (list 'pre "antlr-font-lock-" 'post "-face"
;;        'faces   '(antlr-font-lock-keyword-face
;;		  antlr-font-lock-ruledef-face
;;		  antlr-font-lock-tokendef-face
;;		  antlr-font-lock-ruleref-face
;;		  antlr-font-lock-tokenref-face
;;		  antlr-font-lock-literal-face))
;;  '(antlr-mode))

;; (cb-define-if-required
;;  "VHDL-sb"
;;  (list 'pre "vhdl-speedbar-" 'post "-face"
;;        'faces   '(vhdl-speedbar-entity-face
;;		  vhdl-speedbar-architecture-face
;;		  vhdl-speedbar-configuration-face
;;		  vhdl-speedbar-package-face
;;		  vhdl-speedbar-instantiation-face
;;		  vhdl-speedbar-entity-selected-face
;;		  vhdl-speedbar-architecture-selected-face
;;		  vhdl-speedbar-configuration-selected-face
;;		  vhdl-speedbar-package-selected-face
;;		  vhdl-speedbar-instantiation-selected-face))
;;  '(vhdl-mode speedbar))

;; (cb-define-if-required
;;  "VHDL"
;;  (list 'pre "vhdl-font-lock-" 'post "-face"
;;        'faces    '(vhdl-font-lock-prompt-face
;;		   vhdl-font-lock-attribute-face
;;		   vhdl-font-lock-enumvalue-face
;;		   vhdl-font-lock-function-face
;;		   vhdl-font-lock-directive-face
;;		   vhdl-font-lock-reserved-words-face
;;		   vhdl-font-lock-translate-off-face))
;;  '(vhdl-mode))

(cb-define-if-required
 "Widget"
 (list 'pre "widget-" 'post "-face"
       'faces '(widget-documentation-face
		widget-single-line-field-face
		widget-field-face widget-button-face))
 '(widget))

;; (cb-define-if-required
;;  "Custom"
;;  (list 'pre "custom-" 'post "-face"
;;        'faces '(custom-documentation-face
;;		custom-comment-face
;;		custom-state-face custom-invalid-face
;;		custom-rogue-face custom-modified-face
;;		custom-set-face custom-changed-face
;;		custom-saved-face
;;		custom-button-face custom-face-tag-face
;;		custom-button-pressed-face
;;		custom-variable-button-face
;;		custom-comment-tag-face
;;		custom-variable-tag-face
;;		custom-group-tag-face
;;		custom-group-tag-face-1))
;;  '(custom))

(cb-define-if-required
 "W3M"
 (list 'pre "w3m-" 'post "-face"
       'faces '(w3m-form-face
		w3m-header-line-location-title-face
		w3m-header-line-location-content-face
		w3m-image-face w3m-anchor-face
		w3m-history-current-url-face
		w3m-arrived-anchor-face
		w3m-current-anchor-face
		w3m-bold-face w3m-underline-face
		w3m-tab-unselected-face
		w3m-tab-unselected-retrieving-face
		w3m-tab-selected-face
		w3m-tab-selected-retrieving-face
		w3m-tab-background-face
		w3m-form-button-face
		w3m-form-button-mouse-face
		w3m-form-button-pressed-face))
 '(w3m w3m-form w3m-image))

(cb-define-if-required
 "Emacs Wiki"
 (list 'pre nil 'post "-face"
       'faces '(
		emacs-wiki-link-face
		emacs-wiki-bad-link-face
		))
 '(emacs-wiki))

(cb-define-if-required
 "PCVS"
 (list 'pre nil 'post "-face"
       'faces '(cvs-header-face
		cvs-msg-face
		log-view-message-face
		cvs-filename-face cvs-unknown-face
		cvs-handled-face cvs-need-action-face
		log-view-file-face
		cvs-marked-face))
 '(pcvs log-view))
;;
;; (cb-define-if-required
;;  "Hi-Lock"
;;  (list 'pre "hi-" 'post nil
;;        'faces  '(hi-yellow
;;		 hi-pink hi-green
;;		 hi-blue hi-black-b hi-blue-b
;;		 hi-green-b hi-red-b hi-black-hb))
;;  '(hi-lock))

(cb-define-if-required
 "EDiff"
 (list 'pre "ediff-" 'post nil
       'faces  '(ediff-current-diff-face-Ancestor
		 ediff-current-diff-face-A
		 ediff-current-diff-face-B
		 ediff-current-diff-face-C

		 ediff-fine-diff-face-Ancestor
		 ediff-fine-diff-face-C
		 ediff-fine-diff-face-B
		 ediff-fine-diff-face-A

		 ediff-odd-diff-face-Ancestor
		 ediff-odd-diff-face-A
		 ediff-odd-diff-face-B
		 ediff-odd-diff-face-C

		 ediff-even-diff-face-Ancestor
		 ediff-even-diff-face-A
		 ediff-even-diff-face-B
		 ediff-even-diff-face-C
		 ))
 '(ediff))

(cb-define-if-required
 "Diff"
 (list 'pre "diff-" 'post "-face"
       'faces  '(diff-removed-face
		 diff-added-face
		 diff-changed-face
		 diff-header-face diff-file-header-face
		 diff-hunk-header-face
		 diff-function-face diff-context-face
		 diff-nonexistent-face
		 diff-index-face))
 '(diff-mode))

(cb-define-if-required
 "Change-Log"
 (list 'pre "change-log-" 'post "-face"
       'faces '(change-log-acknowledgement-face
		change-log-conditionals-face
		change-log-function-face
		change-log-name-face change-log-email-face
		change-log-file-face change-log-list-face
		change-log-date-face))
 '(add-log))

;; (cb-define-if-required
;;  "Gnus"
;;  (list 'pre "gnus-" 'post "-face"
;;        'faces '(gnus-signature-face
;;		gnus-header-from-face
;;		gnus-header-subject-face
;;		gnus-header-newsgroups-face
;;		gnus-header-name-face
;;		gnus-header-content-face
;;		gnus-summary-selected-face
;;		gnus-summary-cancelled-face
;;		gnus-summary-high-ticked-face
;;		gnus-summary-low-ticked-face
;;		gnus-summary-normal-ticked-face
;;		gnus-summary-high-ancient-face
;;		gnus-summary-low-ancient-face
;;		gnus-summary-normal-ancient-face
;;		gnus-summary-high-unread-face
;;		gnus-summary-low-unread-face
;;		gnus-summary-normal-unread-face
;;		gnus-summary-high-read-face
;;		gnus-summary-low-read-face
;;		gnus-summary-normal-read-face))
;;  '(gnus))

;; (cb-define-if-required
;;  "Message"
;;  (list 'pre "message-" 'post  "-face"
;;        'faces '(message-cited-text-face
;;		message-header-to-face
;;		message-header-cc-face
;;		message-header-subject-face
;;		message-header-newsgroups-face
;;		message-header-other-face
;;		message-separator-face
;;		message-header-name-face
;;		message-header-xheader-face))
;;  '(message))

(cb-define-if-required
 "Speedbar"
 (list 'pre "speedbar-" 'post "-face"
       'faces '(speedbar-directory-face
		speedbar-highlight-face speedbar-tag-face
		speedbar-file-face speedbar-selected-face
		speedbar-button-face ))
 '(speedbar))

;; We construct the "Minor" group a little differently.  Here we don't
;; skip the whole group if we cannot load a package, we just skip the
;; faces that that package defines.

(defun cb-append-if-required (symbol list &optional package-list)
  "Append LIST to the faces property of the face group SYMBOL iff we
  can load each package in PACKAGE-LIST. This is a piece-wise version
  of what `cb-define-if-required' does."

  (when (or (null package-list)
	    (condition-case nil
		;; load all packages in list or return nil
		(dolist (package package-list 'return-value)
		  (require package))
	      (error nil)))

    ;; this is ugly:-(
    (set symbol (list 'pre (plist-get (symbol-value symbol) 'pre)
		      'post (plist-get (symbol-value symbol) 'post)
		      'faces (append (plist-get (symbol-value symbol)
						'faces)
				     list)
		      ))))

;; define a empty face group called Minor
(let ((symbol (cb-define-if-required "Minor" '())))
  (set symbol '(pre nil post "-face"))

  ;; maybe add face names to that group ...
  (cb-append-if-required symbol
			 '(flyspell-incorrect-face
			   flyspell-duplicate-face)
			 '(flyspell))

  (cb-append-if-required symbol
			 '(sh-heredoc-face)
			 '(sh-script))

  (cb-append-if-required symbol
			 '(show-tabs-tab-face show-tabs-space-face)
			 '(generic-x))

  (cb-append-if-required symbol
			 '(show-paren-match-face show-paren-mismatch-face)
			 '(paren))

  (cb-append-if-required symbol
			 '(highlight-changes-face highlight-changes-delete-face)
			 '(hilit-chg))

  (cb-append-if-required symbol
			 '(comint-highlight-input comint-highlight-prompt)
			 '(comint))

  (cb-append-if-required symbol
			 '(swbuff-default-face
			   swbuff-current-buffer-face
			   swbuff-special-buffers-face
			   swbuff-separator-face)
			 '(swbuff-x))

  (cb-append-if-required symbol
			 '(cperl-nonoverridable-face
			   cperl-hash-face
			   cperl-array-face)
			 '(cperl-mode))

  (cb-append-if-required symbol
			 '(woman-italic-face
			   woman-bold-face
			   woman-unknown-face
			   woman-addition-face)
			 '(woman))


  (cb-append-if-required symbol
			 '(makefile-space-face)
			 '(make-mode))

  (cb-append-if-required symbol
			 '(isearch
			   isearch-lazy-highlight-face
			   tooltip)
			 '())
  )


(cb-define-if-required
 "Info"
 (list 'pre "info-" 'post "-face" ;; case irrelevant
       'faces '(Info-title-1-face
		Info-title-2-face
		Info-title-3-face Info-title-4-face
		info-header-node info-menu-header
		info-header-xref info-node info-xref
		info-menu-5))
 '(info))

(cb-define-if-required
  "Latex"
  (list 'pre "font-latex-" 'post "-face"
	'faces '(tex-math-face
		 font-latex-bold-face
		 font-latex-italic-face
		 font-latex-math-face
		 font-latex-sedate-face
		 font-latex-warning-face))
  '(tex-mode tex-site font-latex))

;; (cb-define-if-required
;;  "EShell"
;;  (list 'pre "eshell-" 'post "-face"
;;        'faces '(eshell-prompt-face
;;		eshell-ls-directory-face
;;		eshell-ls-symlink-face
;;		eshell-ls-executable-face
;;		eshell-ls-readonly-face
;;		eshell-ls-unreadable-face
;;		eshell-ls-special-face eshell-ls-missing-face
;;		eshell-ls-archive-face eshell-ls-backup-face
;;		eshell-ls-product-face eshell-ls-clutter-face
;;		eshell-test-ok-face eshell-test-failed-face))
;;  '(eshell em-prompt em-ls esh-test))

(cb-define-if-required
 "Font-Lock"
 (list 'pre "font-lock-" 'post "-face"
       'faces '(font-lock-comment-face
		font-lock-string-face
		font-lock-doc-face
		font-lock-function-name-face
		font-lock-variable-name-face
		font-lock-constant-face
		font-lock-type-face
		font-lock-builtin-face
		font-lock-warning-face
		font-lock-keyword-face))
 '(font-lock))

(cb-define-if-required
 "Basic"
 (list 'pre nil 'post nil
       'faces '(default
		 region secondary-selection
		 menu tool-bar header-line mode-line
		 fringe scroll-bar border
		 underline italic bold bold-italic
		 highlight fixed-pitch variable-pitch
		 mouse cursor trailing-whitespace)))

;;;_   - Face Group Mode Line.................................

;; The model-line of the face groups buffer contains regions of active
;; text that have various prefix maps as there keymap property.  These
;; prefix maps are bound to mouse-down-2 events and when activated
;; generate pop-up menus.

;;;_    . Group Menu

;; All keys in the prefix keymap `cb-face-groups-menu' have all been
;; defined by now.  These all call the `cb-change-group' function.

(defun cb-change-group (group)
  "Redraw the \"Face Group\" buffer to display GROUP.
 Called by keys in the prefix key-map `cb-face-groups-menu'."
  (when group
    (setq cb-active-face (car (plist-get (eval group) 'faces)))
    (cb-redraw-buffer
     cb-face-group-buffer
     (list 'cb-list group))
    (setq cb-current-group (save-excursion
			     (set-buffer cb-face-group-buffer)
			     cb-name))
    (set-window-text-height (selected-window) (/ (frame-height) 2))
    (shrink-window-if-larger-than-buffer)))

;;;_    . Props Menu

;; Since want to change some non-color properties of our
;; currently active face we offer a facility to do so for some of the
;; more common properties.

;; We toggle properties via a pop-menu which is activated from the
;; mode-line of the "Face Groups" buffer. This pop-up menu is
;; implemented using a prefix-map `cb-props-menu' which is set as the
;; key-map property of the relevant text in the mode-line.

(defvar cb-props-menu (make-sparse-keymap "Props" )
  "Prefix keymap used to generate the \"Props\" mode-line menu.")

(define-key cb-props-menu [cb-underline]
  '(menu-item
    "underline" cb-toggle-underline
    :button (:toggle . (eq t (face-attribute cb-active-face :underline)))))

(define-key cb-props-menu [cb-italic]
  '(menu-item
    "italic" cb-toggle-italic
    :button (:toggle . (eq 'italic (face-attribute cb-active-face :slant)))))

(define-key cb-props-menu [cb-bold]
  '(menu-item
    "bold" cb-toggle-bold
    :button (:toggle . (eq 'bold (face-attribute cb-active-face :weight)))))

(defun cb-toggle-bold ()
  (interactive)
  "Toggle the bold attribute of the face corresponding to `cb-active-face'."
  (save-window-excursion
    (if (equal 'bold (face-attribute cb-active-face :weight))
	(set-face-attribute cb-active-face nil :weight 'normal)
      (set-face-attribute cb-active-face nil :weight 'bold))))

(defun cb-toggle-italic ()
  (interactive)
  "Toggle the italic attribute of the face corresponding to `cb-active-face'."
  (save-window-excursion
    (if (equal 'italic (face-attribute cb-active-face :slant))
	(set-face-attribute cb-active-face nil :slant 'normal)
      (set-face-attribute cb-active-face nil :slant 'italic))))

(defun cb-toggle-underline ()
  (interactive)
  "Toggle the underline attribute of the face corresponding to `cb-active-face'."
  (save-window-excursion
    (let ((underlined (face-attribute cb-active-face :underline)))
      (if (or (null underlined) (equal underlined 'unspecified))
	  (set-face-attribute cb-active-face nil :underline t)
	(set-face-attribute cb-active-face nil :underline 'unspecified)))))

;; This doesn't quite work yet

;; (define-key cb-props-menu [cb-plus-50]
;;   '(menu-item "height + 50%" cb-height-plus-50))
;; (define-key cb-props-menu [cb-plus-20]
;;   '(menu-item "height + 20%" cb-height-plus-20))
;; (define-key cb-props-menu [cb-plus-10]
;;   '(menu-item "height + 10%" cb-height-plus-10))
;;
;; (define-key cb-props-menu [cb-plus-0]
;;   '(menu-item "height + 0%" cb-height-plus-0))
;;
;; (define-key cb-props-menu [cb-minus-10]
;;   '(menu-item "height - 10%" cb-height-minus-10))
;; (define-key cb-props-menu [cb-minus-12]
;;   '(menu-item "height - 20%" cb-height-minus-20))
;; (define-key cb-props-menu [cb-minus-50]
;;   '(menu-item "height - 50%" cb-height-minus-50))

(defun cb-height-plus-0 ()
  (interactive)
  "Increase the height attribute of the `cb-active-face' by 0%."
  (save-window-excursion
    (set-face-attribute cb-active-face nil :height 1.0)))

(defun cb-height-plus-10 ()
  (interactive)
  "Increase the height attribute of the `cb-active-face' by 10%."
  (save-window-excursion
    (set-face-attribute cb-active-face nil :height 1.1)))

(defun cb-height-plus-20 ()
  (interactive)
  "Increase the height attribute of the `cb-active-face' by 20%."
  (save-window-excursion
    (set-face-attribute cb-active-face nil :height 1.2)))

(defun cb-height-plus-50 ()
  (interactive)
  "Increase the height attribute of the `cb-active-face' by 50%."
  (save-window-excursion
    (set-face-attribute cb-active-face nil :height 1.5)))

(defun cb-height-minus-10 ()
  (interactive)
  "Decrease the height attribute of the `cb-active-face' by 50%."
  (save-window-excursion
    (let ((height (face-attribute cb-active-face :height)))
      (set-face-attribute cb-active-face nil :height .1))))

(defun cb-height-minus-20 ()
  (interactive)
  "Decrease the height attribute of the `cb-active-face' by 50%."
  (save-window-excursion
    (let ((height (face-attribute cb-active-face :height)))
      (set-face-attribute cb-active-face nil :height .8))))

(defun cb-height-minus-50 ()
  (interactive)
  "Decrease the height attribute of the `cb-active-face' by 50%."
  (save-window-excursion
    (let ((height (face-attribute cb-active-face :height)))
      (set-face-attribute cb-active-face nil :height .5))))

;;;_    . Themes Menu

;; Since this tool is intended for color-theme design, we allow the
;; user to switch between various color-themes defined in similarly
;; named files under `cb-themes-dir'.  We also allow saving of newly
;; created theme file to that directory.  This is implemented via a
;; pop-menu which is activated from the mode-line of the "Face Groups"
;; buffer. This pop-up menu is implemented using a prefix-map
;; `cb-themes-menu' which is set as the key-map property of the
;; relevant text in the mode-line.

(defvar cb-themes-menu nil
  "Prefix keymap used to generate the \"Theme:\" mode-line menu.")

;; Would be good if color-theme provided this
(defvar cb-current-theme "Default"
  "The current theme selected using the color browser.")

(defvar cb-theme-list nil
  "List used to track the set of currently defined themes and to allow
  completion on existing theme names when a new them is saved.")

(defun cb-change-theme (theme)
  "Switch to a theme from `cb-theme-list'"
  (interactive)

  ;; construct the symbol
  (let ((symbol (intern (concat theme "-theme"))))
    ;; load the file
    (require symbol (concat cb-themes-dir theme "-theme.el"))
    ;; call the theme function
    (funcall symbol))
  (save-excursion
    (set-buffer cb-face-group-buffer)
    (setq header-line-format nil)))

(defun cb-add-theme (name)
  "Add theme called NAME to `cb-theme-list' and an appropriately name
symbol to cb-themes-menu."
  (define-key cb-themes-menu
	(vector (intern (concat "cb-" name "-theme")))
	`(menu-item ,name
		    (lambda () (interactive) (cb-change-theme ,name))
		    :button (:radio
			     . (string-equal cb-current-theme ,name))
		    ))
  (setq cb-theme-list
	(add-to-list 'cb-theme-list (list name))))

(defun cb-build-theme-list ()
  "Set `cb-theme-list' to the names of themes saved in `cb-themes-dir'
  and define corresponding keys in the prefix map `cb-themes-menu'.
Only theme files suffixed with \"-theme.el\" are considered."
  (setq cb-theme-list nil)
  (setq cb-themes-menu (make-sparse-keymap "Themes" ))

  (define-key cb-themes-menu [cb-save-theme-as]
    '(menu-item "Save Theme As ..." cb-save-theme-as))

  (define-key cb-themes-menu [cb-save-theme]
    '(menu-item "Save Theme" cb-save-theme))

  (define-key cb-themes-menu [cb-separator]
    '(menu-item "--shadow-etched-in"))

  (unless (file-exists-p cb-themes-dir)
    (make-directory cb-themes-dir 'and-parents))

  (let ((files (directory-files cb-themes-dir nil "-theme.el$")))
    (dolist (file files)
      (cb-add-theme (substring file 0 -9)))))

(defun cb-save-theme-as ()
  "Wrapper to cause `cb-save-theme' to prompt for a new theme name."
  (interactive)
  (cb-save-theme t))

(defun cb-save-theme (&optional as)
  "Saving the current colors settings as a color theme.
\(With a small change: the theme function also sets the value of
`cb-current-theme'.) If AS is non-nil them prompt for a new theme name
othwise just save under the current theme."
  (interactive)
  (save-window-excursion
    (save-excursion
      (let* ((theme (if as
			(cb-read-name "New theme name: "
				      cb-theme-list
				      cb-current-theme
				      'cb-save-theme-hist)
		      cb-current-theme
		      ))
	     (symbol (concat theme "-theme"))
	     (file-name  (concat cb-themes-dir symbol ".el")))

	;; make a backup
	(when (file-exists-p file-name)
	  (copy-file file-name (concat file-name ".bak") 'overwite))

	(save-excursion
	  (set-buffer (find-file-noselect file-name 'nowarn 'raw))
	  (erase-buffer)

	  (color-theme-print (current-buffer))
	  ;; make some adjustments

	  ;; use SYMBOL instead of my-color-theme
	  (beginning-of-buffer)
	  (insert "(require 'color-theme)\n\n")
	  (search-forward "my-color-theme" nil t)
	  (delete-region (match-beginning 0) (match-end 0))
	  (insert symbol)

	  ;; set `cb-current-theme' as well
	  (search-forward "(interactive)")
	  (insert "\n  (setq cb-current-theme \"" theme "\")")

	  ;; use SYMBOL instead of my-color-theme
	  (search-forward "my-color-theme" nil t)
	  (delete-region (match-beginning 0) (match-end 0))
	  (insert symbol)

	  ;; add a provide
	  (end-of-buffer)
	  (insert "\n\n(provide '" symbol ")\n")
	  (save-buffer)
	  (kill-this-buffer))

	;; Load so that functions and keymaps are defined
	(load-file file-name)
	(message "wrote file: %s" file-name)
	(cb-add-theme theme)
	(save-excursion
	  (set-buffer cb-face-group-buffer)
	  (setq cb-current-theme theme)
	  (setq header-line-format nil)
	  (force-mode-line-update))))))

;;;_    . Mode Line...........................................

;; Note that the contents of the mode-line also displays the current
;; Face Group and Theme setting.

;; Mode-line keymaps which call the above prefix keymaps
;; these can't be buffer local
(defvar cb-group-map (make-sparse-keymap))
(defvar cb-theme-map (make-sparse-keymap))
(defvar cb-props-map (make-sparse-keymap))

(defun cb-build-face-group-maps ()
  "This must be called after `cb-build-theme-list'"
  ;; Quiet normal event because they screw up the modeline
  (define-key cb-group-map [(mode-line) (mouse-1)] 'ignore)
  (define-key cb-props-map [(mode-line) (mouse-1)] 'ignore)
  (define-key cb-theme-map [(mode-line) (mouse-1)] 'ignore)

  (define-key cb-group-map [(mode-line) (mouse-3)] 'ignore)
  (define-key cb-props-map [(mode-line) (mouse-3)] 'ignore)
  (define-key cb-theme-map [(mode-line) (mouse-3)] 'ignore)

  (define-key cb-group-map [(mode-line) (mouse-2)]
    (or cb-face-groups-menu 'ignore))
  (define-key cb-props-map [(mode-line) (mouse-2)]
    (or cb-props-menu 'ignore))
  (define-key cb-theme-map [(mode-line) (mouse-2)]
    (or cb-themes-menu 'ignore))

  ;; force popup on mouse down
  (define-key cb-group-map [(mode-line) (down-mouse-2)]
    (or cb-face-groups-menu 'ignore))
  (define-key cb-props-map [(mode-line) (down-mouse-2)]
    (or cb-props-menu 'ignore))
  (define-key cb-theme-map [(mode-line) (down-mouse-2)]
    (or cb-themes-menu 'ignore)))

;;;_   - Face Group Buffer

;; The face group buffer displays the (abbreviated) names of the faces
;; in a particular group and allows us to select any one of them as
;; the currently active face.  Each face name is display using the
;; corresponding face and is active -- a mouse-2 event over any one
;; will generate a call to `cb-set-active-face'.  The currently active
;; face name will be surrounded by [] and is the value of
;; `cb-active-face'.  This active face will be the target of any color
;; or property changes made by this tool (via the Props menu or the
;; Color Palette buffer).

(defun cb-echo-face (&optional click)
  "Echo the properties of the face of the text under point or mouse."

  (interactive "@e")
  (save-excursion
    (let ((point (point))
	  (marker (make-marker))
	  (inhibit-read-only t)
	  face)

      ;; select the window that we clicked on first
      (if (eventp click)
	  (setq point (posn-point (event-start click)))
	(select-window (posn-window (event-start click))))

      (setq face (get-char-property point 'face))
      (let ((foreground (face-attribute face :foreground))
	    (background (face-attribute face :background)))

	(if (eq foreground 'unspecified) (setq foreground nil))
	(if (eq background 'unspecified) (setq background nil))
	(message (concat "(" (symbol-name face) ")"
			 (and foreground " fg: ") foreground
			 (and background " bg: ") background))))))

(defun cb-set-active-face (&optional click)
  "Set the `cb-active-face' to the face of the text under point or mouse.
Mark the face name under point by surrounding it in []."
  (interactive "@e")
  (save-excursion
    (let ((point (point))
	  (marker (make-marker))
	  (inhibit-read-only t))

      ;; select the window that we clicked on first
      (if (eventp click)
	  (setq point (posn-point (event-start click)))
	(select-window (posn-window (event-start click))))

      (setq cb-active-face (get-char-property point 'face))

      (set-marker marker point)
      ;; un-mark the previous current face
      (save-excursion
	(beginning-of-buffer)
	(and (search-forward "[" nil t) (delete-char -1))
	(and (search-forward "]" nil t) (delete-char -1)))

      ;; mark the face under point as current
      (goto-char marker)
      (skip-chars-backward "^ \t\n\r")
      (insert (propertize "[" 'face 'bold))
      (skip-chars-forward "^ \t\n\r")
      (insert (propertize "]" 'face 'bold))

      (let ((foreground (face-attribute cb-active-face :foreground))
	    (background (face-attribute cb-active-face :background)))
	(if (eq foreground 'unspecified) (setq foreground nil))
	(if (eq background 'unspecified) (setq background nil))
	(message (concat "(" (symbol-name cb-active-face) ")"
			 (and foreground " fg: ") foreground
			 (and background " bg: ") background)))
      )))


(defun cb-build-face-group-buffer ()
  "Insert a structured list of key faces in the current buffer.
Each face name is presented using the corresponding face.  Text properties
are arranged so that mouse-2 (or return) on any of the face names will set
the value of `cb-active-face' to the face under point.

The contents of this buffer is determined by the value of the buffer
local variable cb-list. This will be a plist consisting of three
fields. The `faces' field is simply a list of faces that are to be
display. The `pre-chop' field is a prefix of the face name that may be
omitted from the display without generating any ambiguity. Similarly
`post-chop' is a face name suffix that may be also omitted. By
omitting these superfluous prefixes and suffixes we can keep the
\"Face Group\" buffer relatively compact.  Note that prefix/suffix
chopping is only performed on those faces that have a matching
prefix/suffix."

  ;; Set local variable
  (setq cb-name (substring (substring (symbol-name cb-list) 3) 0 -12))

  (let ((face-map (make-sparse-keymap))
	;; decompose the buffer-local variable cb-list
	(face-list (plist-get (eval cb-list) 'faces))
	(pre-chop  (plist-get (eval cb-list) 'pre))
	(post-chop (plist-get (eval cb-list) 'post))
	(inhibit-read-only t)
	face-name start)

    ;;    (define-key face-map [(mouse-2)] 'cb-echo-face)
    (define-key face-map [(down-mouse-2)] 'cb-set-active-face)
    (define-key face-map [(return)] 'cb-set-active-face)

    ;; insert each elt of face list ...
    (dolist (face face-list)

      ;; abbreviate the face name using pre-chop and post-chop
      (let ((case-fold-search t))
	(setq face-name (symbol-name face))
	(if (and pre-chop (string-match pre-chop face-name))
	    (setq face-name (substring face-name (match-end 0))))
	(if (and post-chop (string-match post-chop face-name))
	    (setq face-name (substring face-name 0 (match-beginning 0)))))

      ;; try to fit as many face names on the same line as possible
      ;; reordering the face names may help here
      (cond ((= (current-column) 0)
	     nil)
	    ((>= (+ (current-column) 1 1 (length face-name) 1)
		 (window-width))
	     (insert "\n"))
	    (t
	     (insert " ")))

      ;; mark the name if it refers to the currently active face
      (when (equal face cb-active-face)
	(insert (propertize "[" 'face 'bold)))

      ;; insert face name with all its special properties
      (setq start (point))
      (insert face-name)
      (let ((foreground (face-attribute face :foreground))
	    (background (face-attribute face :background)))
	(if (eq foreground 'unspecified) (setq foreground nil))
	(if (eq background 'unspecified) (setq background nil))

	(set-text-properties start (point)
			     (list
			      'face face
			      'keymap face-map
			      'mouse-face 'highlight)))

      ;; mark the name if it refers to the currently active face
      (when (equal face cb-active-face)
	(insert (propertize "]" 'face 'bold)))
;;      (insert "\n")
      ) ;; do list


    ;; And set up a reall sexy mode-line
    (setq mode-line-format
	  (list
	   (propertize  "<Props> "
			'face 'menu
			'local-map cb-props-map
			'help-echo "mouse-2: select face property")

	   (propertize (concat "<Group> ")
		       'face 'menu
		       'local-map cb-group-map
		       'help-echo "mouse-2: select face group"
		       )

	   (propertize  "<Theme>"
			'face 'menu
			'local-map cb-theme-map
			'help-echo "mouse-2: select/save theme"
			)))

    (setq header-line-format nil)
    (force-mode-line-update)
    ))

;;;_  + Color Palette Tool

;; The purpose this tool is to allow you to quickly change the color
;; properties of the currently active face.  The "Color Palette"
;; buffer contains a listing of color properties form the current
;; palette, selecting any one of these will set the corresponding
;; property of the currently active face (`cb-active-face').
;; Different palettes may be selected via a pop-up menu that is
;; accessed from the mode-line.  Palettes may also be edited and
;; saved, and new palettes generated from existing palettes by using
;; the "Palette Builder" mode.

;;;_   ; Color Palettes

;; A palette is simply a list of defined color names.  We allow the
;; user to switch between palettes via a pop-menu which is activated
;; from the mode-line of the "Color Palette" buffer. This pop-up menu
;; is implemented using a prefix-map `cb-current-palettes-menu' which is set
;; as the key-map property of the relevant text in the mode-line.

;; In Palette Builder mode there are two palette buffers:  One for
;; diplaying an existing palette, the other for constructing a new
;; Palette.  The mode-line of this buffer The mode-lines of these
;; buffers uses the prefix-maps `cb-new-palettes-menu'
;; `cb-old-palettes-menu' so the displayed palettes can be selected
;; and saved independently.

(defvar cb-current-palettes-menu nil
"Prefix keymap used to generate the \"Palette:\" mode-line menu in the
Color Palette buffer.")

(defvar cb-old-palettes-menu nil
"Prefix keymap used to generate the \"Palette:\" mode-line menu in the
Old Palette buffer.")

(defvar cb-new-palettes-menu nil
"Prefix keymap used to generate the \"Palette:\" mode-line menu in the
New Palette buffer.")

(defvar cb-All-Colors-palette nil
  "A list of all colors supported by the current frame.")
;; evaluate this on load
(setq cb-All-Colors-palette (defined-colors))

(defvar cb-No-Colors-palette '()
  "An empty list used as a place holder for new palettes.")

(defvar cb-Seaweed-palette
  '(
    "black" "dark slate gray" "SkyBlue4" "LightBlue4" "PaleTurquoise4"
    "cadet blue" "LightBlue3" "dark turquoise" "aquamarine3"
    "dark sea green" "light sea green" "medium sea green" "dark khaki"
    "burlywood" "RosyBrown3" "plum3" "azure4" "thistle4"
    )
  "A sample color palette created by Kahlil HODGSON on Tue Dec 3 11:53:20
2002")

(defvar cb-palette-list nil
  "Names of defined palettes stored under `cb-palettes-dir'.  This is
  used for completion.")

(defun cb-build-palette-list ()
  "Set `cb-palette-list' to contain the default palettes and the names
of themes saved in `cb-palettes-dir'.  Then load those palettes
and add them to both palette prefix-maps.  Only default palettes
and palette files suffixed with \"-palette.el\" are considered."

  (setq cb-palette-list nil)
  (setq cb-current-palettes-menu (make-sparse-keymap "Color Palettes" ))

  (define-key cb-current-palettes-menu [cb-delete-palette]
    '(menu-item "Delete this palette"
		(lambda ()
		  (interactive)
		  (cb-delete-palette
		   cb-current-palette-buffer))))

  (define-key cb-current-palettes-menu [cb-separator-1]
    '(menu-item "--shadow-etched-in"))

  ;; Save option is last in the list
  (define-key cb-current-palettes-menu [cb-save-palette-as]
    '(menu-item "Save Palette As ..."
		(lambda ()
		  (interactive)
		  (cb-save-palette
		   cb-current-palette-buffer t))))

  (define-key cb-current-palettes-menu [cb-save-current-palette]
    '(menu-item "Save Palette"
		(lambda ()
		  (interactive)
		  (cb-save-palette
		   cb-current-palette-buffer))))

  (define-key cb-current-palettes-menu [cb-separator-2]
    '(menu-item "--shadow-etched-in"))

  ;; add the two defaults up front
  (define-key cb-current-palettes-menu [cb-No-colors-palette]
    '(menu-item "No-Colors"
		(lambda ()
		  (interactive)
		  (cb-redraw-buffer
		   cb-current-palette-buffer
		   (list 'cb-list 'cb-No-Colors-palette)))
		:button (:radio
			 . (save-excursion
			     (set-buffer cb-current-palette-buffer)
			     (string-equal cb-name "No-Colors"))
			 )))

  (define-key cb-current-palettes-menu [cb-All-colors-palette]
    '(menu-item "All-Colors"
		(lambda ()
		  (interactive)
		  (cb-redraw-buffer
		   cb-current-palette-buffer
		   (list 'cb-list 'cb-All-Colors-palette)))
		:button (:radio
			 . (save-excursion
			     (set-buffer cb-current-palette-buffer)
			     (string-equal cb-name "All-Colors"))
			 )))

  (define-key cb-current-palettes-menu [cb-Seaweed-palette]
    '(menu-item "Seaweed"
		(lambda ()
		  (interactive)
		  (cb-redraw-buffer
		   cb-current-palette-buffer
		   (list 'cb-list 'cb-Seaweed-palette)))
		:button (:radio
			 . (save-excursion
			     (set-buffer cb-current-palette-buffer)
			     (string-equal cb-name "Seaweed"))
			 )))

  ;; Have to change all these definitions in the palette buffers:-(

  ;; load all palettes defined under `cb-palettes-dir'
  (unless (file-exists-p cb-palettes-dir)
    (make-directory cb-palettes-dir 'and-parents))

  (setq cb-palette-list nil)
  (let ((files (directory-files cb-palettes-dir nil "-palette.el$"))
	file)
    (dolist (file files)
      ;; this file includes a key definition whose function targets
      ;; the "Color Palette" buffer
      (load-file (concat cb-palettes-dir file))
      (setq cb-palette-list
	    (add-to-list 'cb-palette-list (list (substring file 0 -11))))))

  ;; make a copies of the prefix-map `cb-current-palette-menu' but
  ;; with the target of the keymap functions set to the new and old
  ;; palette buffers.
  (setq cb-old-palettes-menu
	(cb-swap-keymap-functions
	 (copy-keymap cb-current-palettes-menu)
	 'cb-old-palette-buffer))

  (setq cb-new-palettes-menu
	(cb-swap-keymap-functions
	 (copy-keymap cb-current-palettes-menu)
	 'cb-new-palette-buffer))
  )

;; The keymaps are small shallow trees so recursion is safe here.

(defun cb-swap-keymap-functions (tree buffer-symbol)
  "Return a copy of the list TREE with instances of the symbol
`cb-current-palette-buffer' replaced by BUFFER-SYMBOL. This
function calls itself recursively."
  (cond
   ((eq nil tree) nil) ;; special terminal condition
   ((listp tree) ;; recursive condition
    (cons (cb-swap-keymap-functions (car tree) buffer-symbol)
	  (cb-swap-keymap-functions (cdr tree) buffer-symbol)))
   ;; change this terminals
   ((eq 'cb-current-palette-buffer tree) buffer-symbol)
   ;; default terminal
   (t tree)))

;; test
;;(cb-swap-keymap-functions cb-current-palettes-menu)


(defun cb-save-palette (buffer &optional as)
  "Save palette displayed in BUFFER to an appropriately named file.
This generates Elisp code that, when loaded, defines the requisite
symbols and keymaps for a functional Color Browser palette."

  ;; ensure we can save the file under `cb-palettes-dir'
  (unless (file-directory-p cb-palettes-dir)
    (make-directory cb-palettes-dir 'parents))

  (save-excursion
    (set-buffer buffer)
      ;; create symbol and file names
    (let* ((palette (if as (cb-read-name "Palette name: "
					 cb-palette-list
					 cb-name
					 'cb-save-palette-hist)
		      cb-name
		      ))
	   (symbol (concat "cb-" palette "-palette"))
	   (file-name (concat cb-palettes-dir palette "-palette.el"))
	   color-names)

      (when (or (string-equal palette "All-Colors")
		(string-equal palette "No-Colors"))
	(error "The palette name \"%s\" is reserved for internal use."
	       palette))

      ;; parse the buffer to get a list of color-names
      (beginning-of-buffer)
      (while (search-forward "[DEL]" nil t)
	(forward-char 2)
	(setq color-names
	      (append color-names
		      (list (get-char-property (point) 'color)))))

      ;; make a backup
      (when (file-exists-p file-name)
	(copy-file file-name (concat file-name ".bak") 'overwrite))

      ;; generate and save the code to recreate the palette
      (save-excursion
	(set-buffer (find-file-noselect file-name 'nowarn 'raw))
	(erase-buffer)

	(insert (concat "\n(defvar " symbol "'()"))
	(insert "\n\"Color palette created by " (user-full-name)
		" on " (current-time-string) "\")\n")
	(insert ";; Ensure that the value changes when is the file reloaded\n")
	(insert "(setq " symbol " '(")
	(dolist (color color-names) (insert "\"" color "\" "))
	(insert "))\n")

	(insert
	 "\n(define-key cb-current-palettes-menu [" symbol "]"
	 "\n   '(menu-item \"" palette "\""
	 "\n      (lambda () (interactive)"
	 "\n         (cb-redraw-buffer cb-current-palette-buffer"
	 "\n                           (list 'cb-list '" symbol " )))"
	 "\n      :button (:radio . (save-excursion "
	 "\n                           (set-buffer cb-current-palette-buffer)"
	 "\n                            (string-equal cb-name \"" palette "\")"
	 "))))")

	(save-buffer)
	(kill-this-buffer))

      ;; load so that functions and keymaps are defined (this also
      ;; cross checks that the save worked)
      (load-file file-name)

      ;; make a copies of the prefix-map `cb-current-palette-menu' but
      ;; with the target of the keymap functions set to the "New/Old Palette"
      ;; buffer.
      (setq cb-old-palettes-menu
	    (cb-swap-keymap-functions
	     (copy-keymap cb-current-palettes-menu)
	     'cb-old-palette-buffer))

      (setq cb-new-palettes-menu
	    (cb-swap-keymap-functions
	     (copy-keymap cb-current-palettes-menu)
	     'cb-new-palette-buffer))

      (cb-build-palette-maps) ;; else it wont show up in the menu

      ;; incorporate palette into current state
      (setq cb-palette-list (add-to-list 'cb-palette-list palette))
      (setq header-line-format nil)
      (setq cb-name palette) ;; buffer local setting
      (force-mode-line-update)
      (message "wrote file: %s" file-name)

      ;; walk other buffers to see if we need a redraw them
      (unless (equal buffer cb-new-palette-buffer)
	(set-buffer cb-new-palette-buffer)
	(when (string-equal cb-name palette)
	  (cb-redraw-buffer cb-new-palette-buffer)))

      (unless (equal buffer cb-old-palette-buffer)
	(set-buffer cb-old-palette-buffer)
	(when (string-equal cb-name palette)
	  (cb-redraw-buffer cb-old-palette-buffer)))

      (unless (equal buffer cb-current-palette-buffer)
	(set-buffer cb-current-palette-buffer)
	(when (string-equal cb-name palette)
	  (cb-redraw-buffer cb-current-palette-buffer)))
      )))

(defun cb-delete-palette (buffer)
  "Delete the palette file for the palette displayed in BUFFER."
  (save-excursion
    (set-buffer buffer)

    (when (or (string-equal cb-name "All-Colors")
	      (string-equal cb-name "No-Colors"))
      (error "The palette name \"%s\" is reserved for internal use."
	     cb-name))

    (let ((symbol (concat "cb-" cb-name "-palette"))
	  (file (concat cb-palettes-dir cb-name "-palette.el")))

      (when (not (file-exists-p file))
	(error "Could not find palette file for palette \"%s\"" cb-name))
      (when (yes-or-no-p (concat "Delete " file "? "))
	(delete-file file)
	;; this could be simplified
	(cb-build-palette-list)
	(cb-build-palette-maps)
	(cb-redraw-buffer buffer (list 'cb-list cb-default))
	))))

(defun cb-toggle-layer (buffer)
  "Toggle the layer of the palette displayed in BUFFER."
  (let* ((layer (save-excursion (set-buffer buffer) cb-layer))
	 (layer (if (string-equal layer "foreground" )
		    "background" "foreground")))
    (cb-redraw-buffer buffer (list 'cb-layer layer))
    ))

;;;_   ; Color Palette Mode-line..............................

;; The color palette mode-line contains regions of active text with
;; prefix maps as there keymap property.  These prefix maps are bound
;; to mouse-down-2 events and when activated generate pop-up menus.

;; For the Palette buffers this is primarily to allow the user to
;; change or save the currently displayed palette.  This mode-line
;; also displays the name of the palette is being displayed in the
;; buffer and possibly which mode the Color Browser is operating
;; under.

(defvar cb-current-palettes-map     (make-sparse-keymap))
(defvar cb-old-palettes-map         (make-sparse-keymap))
(defvar cb-new-palettes-map         (make-sparse-keymap))
(defvar cb-toggle-mode-map          (make-sparse-keymap))
(defvar cb-toggle-current-layer-map (make-sparse-keymap))
(defvar cb-toggle-old-layer-map     (make-sparse-keymap))
(defvar cb-toggle-new-layer-map     (make-sparse-keymap))

(defun cb-build-palette-maps ()
  "Thsi must be called after cb-build-palette-list."
  (define-key cb-current-palettes-map [(mode-line) (mouse-1)] 'ignore)
  (define-key cb-current-palettes-map [(mode-line) (mouse-2)] cb-current-palettes-menu)
  (define-key cb-current-palettes-map [(mode-line) (down-mouse-2)] cb-current-palettes-menu)
  (define-key cb-current-palettes-map [(mode-line) (mouse-3)] 'ignore)

  (define-key cb-old-palettes-map [(mode-line) (mouse-1)] 'ignore)
  (define-key cb-old-palettes-map [(mode-line) (mouse-2)] cb-old-palettes-menu)
  (define-key cb-old-palettes-map [(mode-line) (down-mouse-2)] cb-old-palettes-menu)
  (define-key cb-old-palettes-map [(mode-line) (mouse-3)] 'ignore)

  (define-key cb-new-palettes-map [(mode-line) (mouse-1)] 'ignore)
  (define-key cb-new-palettes-map [(mode-line) (mouse-2)] cb-new-palettes-menu)
  (define-key cb-new-palettes-map [(mode-line) (mouse-3)] 'ignore)
  (define-key cb-new-palettes-map [(mode-line) (down-mouse-2)] cb-new-palettes-menu)

  (define-key cb-toggle-mode-map [(mode-line) (mouse-1)] 'ignore)
  (define-key cb-toggle-mode-map [(mode-line) (mouse-2)] 'cb-toggle-mode)
  (define-key cb-toggle-mode-map [(mode-line) (mouse-3)] 'ignore)

  (define-key cb-toggle-current-layer-map [(mode-line) (mouse-1)] 'ignore)
  (define-key cb-toggle-current-layer-map [(mode-line) (mouse-2)]
    (lambda () (interactive) (cb-toggle-layer cb-current-palette-buffer)))
  (define-key cb-toggle-current-layer-map [(mode-line) (mouse-3)] 'ignore)

  (define-key cb-toggle-old-layer-map [(mode-line) (mouse-1)] 'ignore)
  (define-key cb-toggle-old-layer-map [(mode-line) (mouse-2)]
    (lambda () (interactive) (cb-toggle-layer cb-old-palette-buffer)))
  (define-key cb-toggle-old-layer-map [(mode-line) (mouse-3)] 'ignore)

  (define-key cb-toggle-new-layer-map [(mode-line) (mouse-1)] 'ignore)
  (define-key cb-toggle-new-layer-map [(mode-line) (mouse-2)]
    (lambda () (interactive) (cb-toggle-layer cb-new-palette-buffer)))
  (define-key cb-toggle-new-layer-map [(mode-line) (mouse-3)] 'ignore)
  )

;;;_   ; Color Palette Buffer

;; In "Theme Builder" mode the "Color Palette" buffer allows to user
;; to set the foreground or background color property of the currently
;; active face by simply selecting (mouse-2) the displayed property
;; (the color name display in either the foreground or the
;; background).

;; The displayed palette can also be edited using the [DEL] (to
;; delete) and [UP] (to reorder) buttons.  The edited palette can then
;; be saved using one of the options from the mode-line pop-up menu.

;; In "Palette Builder" mode the functionality is a little different.
;; Selecting a color property in the "Color Palette" buffer causes
;; that line to be copied to the "New Palette" buffer.  Selecting a
;; color property in the "New Palette" buffer causes that property to
;; be displayed in the echo area.  Beyond that the two buffers behave
;; in the same way.  We implement these differences by passing
;; different mouse actions to the function that generates the buffer
;; contents.

(defun cb-delete-this-color (click)
  "Delete the color line under point.  Action bound to [DEL] buttons."
  (interactive "@e")
  (save-excursion
    (when (eventp click)
      (select-window (posn-window (event-start click)))
      (goto-char (posn-point (event-start click))))
    (beginning-of-line)
    (let ((inhibit-read-only t))
      (kill-line 1))
    (setq header-line-format "Palette Modified")
    (force-mode-line-update)))

(defun cb-this-color-up (click)
  "Move the color line under point up one line.  Action bound to [UP] buttons."
  (interactive "@e")
  (save-excursion
    (when (eventp click)
      (select-window (posn-window (event-start click)))
      (goto-char (posn-point (event-start click))))
    (let ((inhibit-read-only t)
	  (kill-ring kill-ring))
      (beginning-of-line)
      (kill-line 1)
      (forward-line -1)
      ;; skip top line
      (when (bobp)
	(end-of-buffer)
	(forward-line -1)) ;; skip last line
      (yank)
      (setq header-line-format "Palette Modified")
      (force-mode-line-update)
      )))

(defun cb-last-color-down (click)
  "Move the last color line in the buffer to the top of the list.
This action is bound to the [DOWN] button."
  (interactive "@e")
  (save-excursion
    (when (eventp click)
      (select-window (posn-window (event-start click))))
    (let ((inhibit-read-only t)
	  (kill-ring kill-ring))
      (end-of-buffer)
      (forward-line -2) ;; skip last line
      (beginning-of-line)
      (kill-line 1)
      (beginning-of-buffer)
      (forward-line 1)  ;; skip top line
      (yank)
      (setq header-line-format "Palette Modified")
      (force-mode-line-update)
      )))

(defun cb-add-color (&optional click)
  "Copy the color line under point to the \"New Palette\" buffer.
This function should only be called by active text in the \"Old Palettes\"
buffer."
  (interactive "@e")
  (save-excursion
    (let ((inhibit-read-only t)
	  (echo-map (make-sparse-keymap))
	  start)

      ;; build new definition for mouse action
      (define-key echo-map  [(mouse-2)] 'cb-echo-color)
      (define-key echo-map  [(down-mouse-2)] 'cb-echo-color)
      (define-key echo-map  [(return)] 'cb-echo-color)

      (when (eventp click)
	(select-window (posn-window (event-start click)))
	(goto-char (posn-point (event-start click))))
      (beginning-of-line)
      (copy-region-as-kill (point) (save-excursion (forward-line 1) (point)))

      (set-buffer cb-new-palette-buffer)

      (beginning-of-line)
      ;; catch point on or after last line
      (unless (looking-at "\\[DEL\\]") (beginning-of-buffer))
      ;; catch point on first line
      (when (bobp) (forward-line 1))
      (setq start (point))
      (yank 1)
      (add-text-properties (+ start 6) (- (point) 5)
			   (list 'keymap echo-map))
      (setq header-line-format "Palette Modified")
      (force-mode-line-update)
      )))

(defun cb-echo-color (&optional click)
  "Echo the color of the text under point or mouse.
Uses the text property `layer' to determine whether the foreground or
background is echoed.  This function  should only be called by active
text in the \"*New Palette*\" buffer."
  (interactive "@e")
  (save-excursion
    (let* ((point (point)))

      (when (eventp click)
	(set-buffer (window-buffer (posn-window (event-start click))))
	(setq point (posn-point (event-start click))))

      (message "layer: %s, color: %s, weight: %s"
	       (get-char-property point 'layer)
	       (get-char-property point 'color)
	       (or (get-char-property point 'weight) 'normal)
	       ))))

(defun cb-change-color (&optional click)
  "Change the default text color to that under point or mouse.
Uses the text property `layer' to determine whether the foreground or
background is changed.  This function should only be called by active
text in the \"*Current Palette*\" buffer."

  (interactive "@e")
  (let* ((point (point)))
    (save-excursion

      (when (eventp click)
	(set-buffer (window-buffer (posn-window (event-start click))))
	(setq point (posn-point (event-start click))))

      (set-face-attribute cb-active-face nil
			  (get-char-property point 'layer)
			  (get-char-property point 'color)
;;			  :weight (or (get-char-property point 'weight) 'normal)
			  )))

  ;; indicate change in the mode-line
  (save-excursion
    (set-buffer cb-face-group-buffer)
    (setq header-line-format "Theme Modified")
    (force-mode-line-update)))


(defun cb-build-color-palette-buffer ()
  "Insert a structured list of colors from the list PALETTE into the
current buffer.  Colors are presented one per line in both foreground
and background forms.  Text properties are arranged so that mouse-2
\(or return) on any of the color names will run ACTION which is
intended to use the value of `color' and `layer' properties under the
mouse (or point).  The first line contains an buttons that let you set
the face property to an unspecified value.  Each line is preceded a
\[DEL] button that deletes the line, and is followed by a [UP] button
that swaps the current line with the one above.  The first line of the
buffer allows you to set the color properties to the `unspecified'
value."

  ;; Set buffer local variable
  (setq cb-name (substring (substring (symbol-name cb-list) 3) 0 -8))

  (let* ((list (eval cb-list))
	 (mouse-map (make-sparse-keymap))
	 (del-map   (make-sparse-keymap))
	 (up-map    (make-sparse-keymap))
	 (down-map  (make-sparse-keymap))
	 (width (- cb-frame-width 6 5 1))
	 (width-str (number-to-string width))
	 (background-p (string-equal cb-layer "background"))
	 start)

    ;; override defaults
    (define-key mouse-map  [(mouse-2)] cb-action)
    (define-key del-map    [(mouse-2)] 'cb-delete-this-color)
    (define-key up-map     [(mouse-2)] 'cb-this-color-up)
    (define-key down-map   [(mouse-2)] 'cb-last-color-down)

    ;; force action on down events
    (define-key mouse-map  [(down-mouse-2)] cb-action)
    (define-key del-map    [(down-mouse-2)] 'cb-delete-this-color)
    (define-key up-map     [(down-mouse-2)] 'cb-this-color-up)
    (define-key down-map   [(down-mouse-2)] 'cb-last-color-down)

    ;; and keyboard events as well
    (define-key mouse-map  [(return)] cb-action)
    (define-key del-map    [(return)] 'cb-delete-this-color)
    (define-key up-map     [(return)] 'cb-this-color-up)
    (define-key down-map   [(return)] 'cb-last-color-down)

    ;; first the top line which is special
    (insert "      ")
    (setq start (point))
    (if background-p
	(progn
	  (insert (substring (format (concat "%" width-str "s")
				     "unspecified")
			     0 width))
	  (set-text-properties start (point)
			       (list 'layer ':background
				     'weight 'normal
				     'color 'unspecified
				     'mouse-face 'highlight
				     'keymap mouse-map)))

      (insert (substring (format (concat "%" width-str "s")
				 "unspecified")
			 0 width))

      (set-text-properties start (point)
			   (list 'layer ':foreground
				 'weight 'normal
				 'color 'unspecified
				 'mouse-face 'highlight
				 'keymap mouse-map)))
    (insert "\n")

    ;; Delete duplicate colors from our list
    (let ((l list))
      (while (cdr l)
	(if (facemenu-color-equal (car l) (car (cdr l)))
	    (setcdr l (cdr (cdr l)))
	  (setq l (cdr l)))))

    ;; generate the listing
    (while list
      ;; delete button
      (setq start (point))
      (insert "[DEL]")
      (set-text-properties start (point)
			   (list 'face 'widget-button-face
				 'mouse-face 'highlight
				 'keymap del-map))
      (insert " ")
      (setq start (point))
      (let ((start start))
	(if background-p
	    (progn
	      ;; background color
	      (insert (substring (format (concat "%" width-str "s")
					 (concat (car list)
						 (car list)
						 (car list)
						 (car list)))
				 0 width))
	      (set-text-properties start (point)
				   (list 'layer ':background
					 'weight 'normal
					 'face `(:background ,(car list))
					 'mouse-face 'highlight)))
	  ;; foreground color
	  (setq start (point))
	  (insert (substring (format (concat "%" width-str "s")
				     (concat (car list)
					     (car list)
					     (car list)
					     (car list)))
			     0 width))
	  (set-text-properties start (point)
			       (list 'layer ':foreground
				     'weight 'normal
				     'face `(:foreground ,(car list))
				     'mouse-face 'highlight))
	  ))
      (add-text-properties start (point)
			   (list 'color (car list)
				 'keymap mouse-map))
      ;; up button
      (insert " ")
      (setq start (point))
      (insert "[UP]")
      (set-text-properties start (point)
			   (list 'face widget-button-face
				 'mouse-face 'highlight
				 'keymap up-map))
      (insert "\n")
      (setq list (cdr list)))

    ;; down button
    (indent-to (+ 5 width))
    (setq start (point))
    (insert "[DOWN]")
    (set-text-properties start (point)
			 (list 'face widget-button-face
			       'mouse-face 'highlight
			       'keymap down-map))
    (insert "\n")
    (buffer-enable-undo)

    ;; now for the sexy mode-line
    (setq mode-line-format
	  (list
	   (propertize "<Palette>"
		       'face 'menu
		       'local-map (eval cb-map)
		       'help-echo "mouse: select palette")

	   (propertize (if background-p " <Foreground>" " <Background>")
		       'face 'menu
		       'local-map (eval cb-layer-map)
		       'help-echo "mouse: change layer")

	   '(:eval (propertize (if cb-theme-builder-p " <PB>" " <TB>")
			       'face 'menu
			       'local-map cb-toggle-mode-map
			       'help-echo "mouse: change mode"))
	  ))
    (setq header-line-format nil)
    (force-mode-line-update)
    ))

;;;_  + Color Browser.........................................

;; Now we define the Color Browser which is made up of the buffers
;; mode-lines and keymaps defined above.  This tool has a frame of its
;; own and displays two of three distinct buffers at any one time.  It
;; has two modes of operation: Theme Builder and Palette Builder.

(defun cb-quit ()
  "Delete all frames and buffers associated with the color browser."
  (interactive)
  (select-frame-by-name "Color Browser")
  (condition-case nil (kill-buffer cb-current-palette-buffer) (error nil))
  (condition-case nil (kill-buffer cb-new-palette-buffer)     (error nil))
  (condition-case nil (kill-buffer cb-old-palette-buffer)     (error nil))
  (condition-case nil (kill-buffer cb-face-group-buffer)      (error nil))
  (delete-frame))

(defun cb-read-only-undo ()
  "Version of undo that works in read-only buffers.  Since the buffer
  is read-only hits will undo special edit that also set
  `inhibit-read-only'."
  (interactive)
  (let ((inhibit-read-only t))
    (undo)))

(defvar cb-redraw-function nil
  "Buffer local variable holding the function used to redraw the buffer.")
(make-variable-buffer-local 'cb-redraw-function)

(defun cb-redraw-buffer (buffer &rest settings)
  "Set buffer local variables in BUFFER from the list of variable /value
pairs in SETTINGS then redraw using the function held in the variable
cb-redraw-function."
  (save-window-excursion
    (save-excursion

      ;; make sure we are looking at the right window first
      ;; (for the special case point is in other window and initiating
      ;; a change from the mode-line)
      (dolist (window (window-list))
	(when (equal (window-buffer window) buffer)
	  (select-window window)))

      (set-buffer buffer)

      ;; set the local variables
      (dolist (pair settings)
	(set (car pair) (cadr pair)))

      ;; do the redraw
      (let ((inhibit-read-only t))
	(erase-buffer)
	(funcall cb-redraw-function))
      )))

(defun cb-buffer-prepare (name)
  "Create an empty a view-mode buffer called NAME
that initally ignores mouse-2 events, uses a special quit command
`cb-quit', and allow the keyboard undo command. That buffer selected
and the BODY form evaluated."

  (set-buffer (get-buffer-create name))
  (view-mode 1)
  (use-local-map view-mode-map)
  ;; XEmacs?
  (local-set-key [(mouse-2)] 'ignore)	;; quiet other bindings
  (local-set-key [(q)] 'cb-quit)
  (local-set-key [(control ?/)] 'cb-read-only-undo)
  (local-set-key [(control ?_)] 'cb-read-only-undo)
  (current-buffer))

(defun cb-toggle-mode ()
  "Toggle between Theme Builder and Palette Builder modes.  This is
  normally called via a keymap bound to text in the mode-line."
  (interactive)
  (if cb-theme-builder-p
      (color-browser t)
    (color-browser nil)))

(defun cb-initialize ()
  "Setup initial color browser frame, buffers, and menu maps."
  (interactive)

  (select-frame
   (make-frame
    `((name . "Color Browser")
      (width . ,cb-frame-width)
      (height . ,cb-frame-height)
      (minibuffer . nil)
      (menu-bar-lines . nil)
      (tool-bar-lines . nil)
      (unsplittable. t)
      )))

  (cb-build-theme-list)
  (cb-build-face-group-maps)

  (cb-build-palette-list)
  (cb-build-palette-maps)

  (cb-redraw-buffer
   (setq cb-face-group-buffer (cb-buffer-prepare " *Face Group*"))
   (list 'cb-list 'cb-Basic-faces-plist)
   (list 'cb-default 'cb-Basic-faces-plist)
   (list 'cb-redraw-function 'cb-build-face-group-buffer))

  (cb-redraw-buffer
   (setq cb-current-palette-buffer (cb-buffer-prepare " *Current Palette*"))
   (list 'cb-map 'cb-current-palettes-map)
   (list 'cb-list 'cb-All-Colors-palette)
   (list 'cb-default 'cb-All-Colors-palette)
   (list 'cb-action 'cb-change-color)
   (list 'cb-layer "foreground")
   (list 'cb-layer-map 'cb-toggle-current-layer-map )
   (list 'cb-redraw-function 'cb-build-color-palette-buffer))

  (cb-redraw-buffer
   (setq cb-old-palette-buffer (cb-buffer-prepare " *Old Palette*"))
   (list 'cb-map 'cb-old-palettes-map)
   (list 'cb-list 'cb-All-Colors-palette)
   (list 'cb-default 'cb-All-Colors-palette)
   (list 'cb-action 'cb-add-color)
   (list 'cb-layer "foreground")
   (list 'cb-layer-map 'cb-toggle-old-layer-map )
   (list 'cb-redraw-function 'cb-build-color-palette-buffer))

 (cb-redraw-buffer
   (setq cb-new-palette-buffer (cb-buffer-prepare " *New Palette*"))
   (list 'cb-map 'cb-new-palettes-map)
   (list 'cb-list 'cb-No-Colors-palette)
   (list 'cb-default 'cb-No-Colors-palette)
   (list 'cb-action 'cb-echo-color)
   (list 'cb-layer "foreground")
   (list 'cb-layer-map 'cb-toggle-new-layer-map )
   (list 'cb-redraw-function 'cb-build-color-palette-buffer))
  )

;;;###autoload
(defun color-browser (&optional palette-builder)
  "*Pop up a frame containing a color theme design tool.
If PALETTE-BUILDER is t, this tool consists of two color palette
buffers \(see `cb-build-color-palette-buffer').  Otherwise it consists
of on Color Palette buffer and one face group buffer \(see
`cb-build-face-group-buffer')"

  (interactive) (save-excursion

    ;; get/make an the Color Browser frame
    (condition-case nil
	(progn
	  (select-frame-by-name "Color Browser")
	  (raise-frame))
      (error (cb-initialize)))

    ;; put the pointer somewhere sensible
    (set-mouse-position
     (selected-frame)
     (frame-width) (- (frame-height) 1))

    (delete-other-windows)
    (cond (palette-builder
	   (setq cb-theme-builder-p nil)
	   (modify-frame-parameters nil '((title . "CB - Palette Builder")))
	   (switch-to-buffer cb-old-palette-buffer)
	   (split-window-vertically)
	   (switch-to-buffer cb-new-palette-buffer))
	  (t
	   (setq cb-theme-builder-p t)
	   (modify-frame-parameters nil '((title . "CB - Theme Builder")))
	   (switch-to-buffer cb-current-palette-buffer)
	   (split-window-vertically)
	   (switch-to-buffer cb-face-group-buffer)
	   ;; try to leave as much screen space as possible for the
	   ;; Palettes buffer
	   (set-window-text-height (selected-window) (/ (frame-height) 2))
	   (shrink-window-if-larger-than-buffer)
	   ))
    ))

(provide 'color-browser)

;;; color-browser.el ends here
