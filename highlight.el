;;; highlight.el --- Highlighting commands.
;;
;; Filename: highlight.el
;; Description: Highlighting commands.
;; Author: Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 1995-2025, Drew Adams, all rights reserved.
;; Created: Wed Oct 11 15:07:46 1995
;; Version: 0
;; Package-Requires: ()
;; Last-Updated: Thu Jul 24 12:52:56 2025 (-0700)
;;           By: dradams
;;     Update #: 4440
;; URL: https://www.emacswiki.org/emacs/download/highlight.el
;; URL (GIT mirror): https://framagit.org/steckerhalter/highlight.el
;; Doc URL: https://www.emacswiki.org/emacs/HighlightLibrary
;; Keywords: faces, help, local
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x, 24.x, 25.x, 26.x
;;
;; Features that might be required by this library:
;;
;;   `apropos', `apropos+', `auth-source', `avoid', `backquote',
;;   `bookmark', `bookmark+', `bookmark+-1', `bookmark+-bmu',
;;   `bookmark+-key', `bookmark+-lit', `button', `bytecomp', `cconv',
;;   `cl-generic', `cl-lib', `cl-macs', `cmds-menu', `col-highlight',
;;   `crosshairs', `custom', `doremi', `doremi-frm', `easymenu',
;;   `eieio', `eieio-core', `eieio-loaddefs', `epg-config',
;;   `facemenu', `facemenu+', `faces', `faces+', `fit-frame',
;;   `font-lock', `font-lock+', `font-lock-menus', `frame-cmds',
;;   `frame-fns', `gv', `help+', `help-fns', `help-fns+',
;;   `help-macro', `help-macro+', `help-mode', `hexrgb', `hl-line',
;;   `hl-line+', `info', `info+', `kmacro', `macroexp', `menu-bar',
;;   `menu-bar+', `misc-cmds', `misc-fns', `mwheel', `nadvice',
;;   `naked', `package', `palette', `password-cache', `pp', `pp+',
;;   `radix-tree', `rect', `replace', `ring', `second-sel', `seq',
;;   `strings', `syntax', `tabulated-list', `text-mode', `thingatpt',
;;   `thingatpt+', `timer', `url-handlers', `url-parse', `url-vars',
;;   `vline', `w32browser-dlgopen', `wid-edit', `wid-edit+',
;;   `widget', `zones'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Highlighting commands.
;;
;;    More description below.
 
;;(@> "Index")
;;
;;  Index
;;  -----
;;
;;  If you have library `linkd.el' and Emacs 22 or later, load
;;  `linkd.el' and turn on `linkd-mode' now.  It lets you easily
;;  navigate around the sections of this doc.  Linkd mode will
;;  highlight this Index, as well as the cross-references and section
;;  headings throughout this file.  You can get `linkd.el' here:
;;  https://www.emacswiki.org/emacs/download/linkd.el.
;;
;;  (@> "Things Defined Here")
;;  (@> "Documentation")
;;    (@> "Libraries `facemenu+.el' and `mouse3.el' put Highlight on the Menu")
;;    (@> "User Options `hlt-use-overlays-flag' and `hlt-overlays-priority'")
;;    (@> "Temporary or Permanent Highlighting")
;;    (@> "Commands")
;;    (@> "Copy and Yank (Paste) Text Properties")
;;    (@> "User Option `hlt-act-on-any-face-flag'")
;;    (@> "Hiding and Showing Text")
;;      (@> "Hiding and Showing Text - Icicles Multi-Commands")
;;    (@> "What Gets Highlighted: Region, Buffer, New Text You Type")
;;    (@> "Interaction with Font Lock")
;;    (@> "Suggested Bindings")
;;    (@> "See Also")
;;    (@> "Commands That Won't Work in Emacs 20")
;;    (@> "To Do")
;;  (@> "Change log")
;;  (@> "Macros")
;;  (@> "Key Bindings")
;;  (@> "Menus")
;;  (@> "Variables and Faces")
;;  (@> "Misc Functions - Emacs 20+")
;;  (@> "Misc Functions - Emacs 21+")
;;  (@> "Functions for Highlighting Propertized Text - Emacs 21+")
;;  (@> "Functions for Highlighting Isearch Matches - Emacs 23+")
;;  (@> "General and Utility Functions")
 
;;(@* "Things Defined Here")
;;
;;  Things Defined Here
;;  -------------------
;;
;;  Macros defined here:
;;
;;    `hlt-user-error'.
;;
;;  Commands defined here:
;;
;;    `hlt-choose-default-face', `hlt-copy-props', `hlt-eraser',
;;    `hlt-eraser-mouse', `hlt-hide-default-face', `hlt-highlight',
;;    `hlt-highlight-all-prop', `hlt-highlight-enclosing-list',
;;    `hlt-highlighter', `hlt-highlighter-mouse',
;;    `hlt-highlight-isearch-matches',
;;    `hlt-highlight-line-dups-region', `hlt-highlight-lines',
;;    `hlt-highlight-property-with-value',
;;    `hlt-highlight-regexp-groups-region',
;;    `hlt-highlight-regexp-region',
;;    `hlt-highlight-regexp-region-in-buffers',
;;    `hlt-highlight-regexp-to-end', `hlt-highlight-region',
;;    `hlt-highlight-region-in-buffers',
;;    `hlt-highlight-single-quotations', `hlt-highlight-symbol',
;;    `hlt-highlight-zones', `hlt-highlight-zones-in-buffers',
;;    `hlt-mouse-copy-props', `hlt-mouse-face-each-line',
;;    `hlt-next-face', `hlt-next-highlight', `hlt-paste-props',
;;    `hlt-previous-face', `hlt-previous-highlight',
;;    `hlt-replace-highlight-face',
;;    `hlt-replace-highlight-face-in-buffers',
;;    `hlt-show-default-face', `hlt-toggle-act-on-any-face-flag',
;;    `hlt-toggle-link-highlighting',
;;    `hlt-toggle-property-highlighting',
;;    `hlt-toggle-use-overlays-flag', `hlt-unhighlight-all-prop',
;;    `hlt-unhighlight-isearch-matches',
;;    `hlt-unhighlight-regexp-groups-region',
;;    `hlt-unhighlight-regexp-region',
;;    `hlt-unhighlight-regexp-region-in-buffers',
;;    `hlt-unhighlight-regexp-to-end', `hlt-unhighlight-region',
;;    `hlt-unhighlight-region-for-face',
;;    `hlt-unhighlight-region-for-face-in-buffers',
;;    `hlt-unhighlight-region-in-buffers', `hlt-unhighlight-symbol',
;;    `hlt-unhighlight-zones', `hlt-unhighlight-zones-in-buffers',
;;    `hlt-yank-props'.
;;
;;  User options (variables) defined here:
;;
;;    `hlt-act-on-any-face-flag', `hlt-auto-face-backgrounds',
;;    `hlt-auto-face-foreground', `hlt-auto-faces-flag',
;;    `hlt-default-copy/yank-props', `hlt-face-prop',
;;    `hlt-line-dups-ignore-regexp', `hlt-max-region-no-warning',
;;    `hlt-overlays-priority', `hlt-use-overlays-flag'.
;;
;;  Faces defined here:
;;
;;    `hlt-property-highlight', `hlt-regexp-level-1',
;;    `hlt-regexp-level-2', `hlt-regexp-level-3',
;;    `hlt-regexp-level-4', `hlt-regexp-level-5',
;;    `hlt-regexp-level-6', `hlt-regexp-level-7',
;;    `hlt-regexp-level-8', `minibuffer-prompt' (for Emacs 20).
;;
;;  Non-interactive functions defined here:
;;
;;    `hlt-+/--highlight-regexp-read-args',
;;    `hlt-+/--highlight-regexp-region', `hlt-+/--read-regexp',
;;    `hlt-+/--read-bufs', `hlt-add-listifying',
;;    `hlt-add-to-invisibility-spec', `hlt-delete-highlight-overlay',
;;    `hlt-highlight-faces-in-buffer', `hlt-flat-list',
;;    `hlt-highlight-faces-in-buffer', `hlt-highlight-regexp-groups',
;;    `hlt-listify-invisibility-spec',
;;    `hlt-mouse-toggle-link-highlighting',
;;    `hlt-mouse-toggle-property-highlighting',
;;    `hlt-nonempty-region-p', `hlt-props-to-copy/yank',
;;    `hlt-read-bg/face-name', `hlt-read-props-completing',
;;    `hlt-region-or-buffer-limits', `hlt-remove-if-not',
;;    `hlt-set-intersection', `hlt-set-union', `hlt-string-match-p',
;;    `hlt-subplist', `hlt-tty-colors', `hlt-unhighlight-for-overlay'.
;;
;;  Internal variables defined here:
;;
;;    `hlt-copied-props', `hlt-face-nb', `hlt-last-face',
;;    `hlt-last-regexp', `hlt-map',
;;    `hlt-previous-use-overlays-flag-value',
;;    `hlt-prop-highlighting-state'.
  
;;(@* "Documentation")
;;
;;  Documentation
;;  -------------
;;
;;(@* "Libraries `facemenu+.el' and `mouse3.el' put Highlight on the Menu")
;;  ** Libraries `facemenu+.el' and `mouse3.el' put Highlight on the Menu **
;;
;;  If you load library `facemenu+.el' after you load library
;;  `highlight.el' then commands defined here are also available on a
;;  `Highlight' submenu in the Text Properties menus.
;;
;;  If you load library `mouse3.el' after you load library
;;  `highlight.el' then:
;;
;;    * Commands defined here are also available on a `Highlight'
;;      submenu of the `Region' right-click popup menu.
;;
;;    * Commands `hlt-highlight-symbol' and `hlt-unhighlight-symbol'
;;      are available on the `Thing at Pointer' submenu of the `No
;;      Region' right-click popup menu.
;;
;;(@* "User Options `hlt-use-overlays-flag' and `hlt-overlays-priority'")
;;  ** User Options `hlt-use-overlays-flag' and `hlt-overlays-priority'
;;
;;  You can highlight text in two ways using this library, depending
;;  on the value of user option `hlt-use-overlays-flag':
;;
;;   - non-nil means to highlight using overlays
;;   - nil means to highlight using text properties
;;
;;  Overlays are independent from the text itself.  They are not
;;  picked up when you copy and paste text.  By default, highlighting
;;  uses overlays.
;;
;;  Although highlighting recognizes only nil and non-nil values for
;;  `hlt-use-overlays-flag', other actions can have different
;;  behavior, depending on the non-nil value.  If it is `only' (the
;;  default value), then only overlay highlighting is affected.  If it
;;  is any other non-nil value, then both overlay highlighting and
;;  text-property highlighting are effected.  This is the case, for
;;  instance, for unhighlighting and for navigating among highlights.
;;
;;  For example, for unhighlighting, if `hlt-use-overlays-flag' is
;;  non-nil, then overlay highlighting is removed.  If it is not
;;  `only', then text-property highlighting is removed.  A value of
;;  nil thus removes both overlays and text properties.
;;
;;  Keep this sensitivity to the value of `hlt-use-overlays-flag' in
;;  mind.  For example, if you change the value after adding some
;;  highlighting, then that highlighting might not be removed by
;;  unhighlighting, unless you change the value back again.
;;
;;  You can toggle the value of `hlt-use-overlays-flag' at any time
;;  between nil and its previous non-nil value, using command
;;  `hlt-toggle-use-overlays-flag'.
;;
;;  Option `hlt-overlays-priority' is the priority assigned to
;;  overlays created by `hlt-* functions.  A higher priority makes an
;;  overlay seem to be "on top of" lower priority overlays.  The
;;  default value is a zero priority.
;;
;;(@* "Temporary or Permanent Highlighting")
;; ** "Temporary or Permanent Highlighting" **
;;
;;  Generally, highlighting you add is temporary: it is not saved when
;;  you write your buffer to disk.  However, Emacs has a curious and
;;  unfamiliar feature called "formatted" or "enriched" text mode,
;;  which does record highlighting permanently.  See the Emacs manual,
;;  node `Requesting Formatted Text'.
;;
;;  To save highlighting permanently, do the following:
;;
;;  1. `M-x enriched-mode', to put your file buffer in minor mode
;;     `enriched-mode'.  You see `Enriched' in the mode line.
;;
;;  2. Choose text-property highlighting, not overlay highlighting, by
;;     setting option `hlt-use-overlays-flag' to `nil'.  To do this
;;     using Customize, choose menu item `Highlight using text
;;     properties, not overlays'.
;;
;;  3. Choose the highlight face to use:
;;     `M-x hlt-choose-default-face'.
;;
;;  4. Highlight in any way provided by library `highlight.el'.  For
;;     example, use `hlt-highlighter' (personally, I bind it to `C-x
;;     mouse-2') to drag-highlight as if using a marker pen.
;;
;;  5. Save your file.
;;
;;     Note that, although highlighting in enriched-text mode modifies
;;     the buffer, it does not appear modified (check the beginning of
;;     the mode line), so if you make no other changes then using `C-x
;;     C-s' does not save your highlighting changes.  To remedy this,
;;     just do something besides highlighting - e.g., add a space and
;;     delete it - so that `C-x C-s' saves to disk.
;;
;;  When you reopen your file later, it is automatically in enriched
;;  mode, and your highlighting shows.  However, be aware that
;;  font-locking can interfere with enriched mode, so you might want
;;  to use it on files where you don't use font-locking.  But see also
;;  (@> "Interaction with Font Lock").
;;
;;(@* "Commands")
;;  ** Commands **
;;
;;  You can use any face to highlight, and you can apply a mouse face
;;  instead of a face, if you like.  A mouse face shows up only when
;;  the mouse pointer is over it.
;;
;;  The main command to choose a face to use for highlighting (or for
;;  unhighlighting) is `hlt-choose-default-face'.  It reads a face
;;  name, with completion.
;;
;;  But you can alternatively choose a color name instead of a face
;;  name.  The completion candidates are annotated in buffer
;;  `*Completions*' with `Face' or `Color', to help you identify them.
;;
;;  If you use library Icicles and option
;;  `icicle-WYSIWYG-Completions-flag' is non-nil, then candidate faces
;;  and colors are WYSIWYG: What You See Is What You Get.
;;
;;  If you choose a color instead of a face then an unnamed pseudo
;;  face is created and used.  It has the chosen color as background,
;;  and its foreground color is determined by the value of user option
;;  `hlt-auto-face-foreground'.  If that option is nil then
;;  highlighting does not change the existing foreground color.
;;  Otherwise, the option value is the foreground color used for
;;  highlighting.
;;
;;  Another way to choose the highlighting face is to use command
;;  `hlt-next-face' or `hlt-previous-face'.  These cycle among a
;;  smaller set of faces and background colors, the elements in the
;;  list value of option `hlt-auto-face-backgrounds'.  You can use a
;;  numeric prefix argument with these commands to choose any of the
;;  elements by its absolute position in the list.
;;
;;  Choosing the default highlighting face using
;;  `hlt-choose-default-face', `hlt-next-face', or `hlt-previous-face'
;;  affects the next highlighting or unhighlighting operation.  You
;;  can also choose to automatically cycle among the faces defined by
;;  `hlt-auto-face-backgrounds', with each (un)highlighting command
;;  using the next face in the list.  To choose this behavior,
;;  customize option `hlt-auto-faces-flag' to non-nil.
;;
;;  The commands with `region' in their name act on the text in the
;;  active region.  If the region is not active then they act on the
;;  text in the whole buffer.  The commands with `to-end' in their
;;  name act on the text from point to the end of the buffer.  See
;;  also (@> "What Gets Highlighted: Region, Buffer, New Text You Type").
;;
;;  The commands you will use the most often are perhaps
;;  `hlt-highlight', `hlt-highlighter', `hlt-highlight-symbol',
;;  `hlt-next-highlight', and `hlt-previous-highlight', as well as
;;  unhighlighting commands.  You might also often use the various
;;  commands to hide and show highlighted text.
;;
;;  You can use command `hlt-highlight' to highlight or unhighlight
;;  the region, or to highlight or unhighlight a regexp throughout the
;;  region, depending on the prefix argument.  It combines the
;;  behaviors of commands `hlt-highlight-region',
;;  `hlt-unhighlight-region', `hlt-highlight-regexp-region', and
;;  `hlt-highlight-regexp-region'.  I suggest that you bind
;;  `hlt-highlight' to a key - I use `C-x C-y'.
;;
;;  Commands `hlt-highlight-regexp-to-end' and
;;  `hlt-unhighlight-regexp-to-end' highlight and unhighlight a regexp
;;  from point to the end of the buffer, respectively.
;;
;;  Command `hlt-highlighter' lets you highlight text by simply
;;  dragging the mouse, just as you would use a highlighter (marker).
;;  You can thus highlight text the same way that you drag the mouse
;;  to define the region.
;;
;;  Command `hlt-eraser' lets you delete highlighting by dragging the
;;  mouse.  However, its behavior is different for overlays and text
;;  properties, and it is perhaps different from you expect.  If
;;  option `hlt-use-overlays-flag' is not `only' then it removes
;;  text-property highlighting for *ALL* faces (not just highlighting
;;  faces).
;;
;;  A prefix arg for `hlt-highlighter' and `hlt-eraser' acts the same
;;  as for `hlt-next-face': it lets you choose the face to use.  It
;;  has no effect for `hlt-eraser' unless `hlt-use-overlays-flag' is
;;  `only', in which case it erases the Nth face in
;;  `hlt-auto-face-backgrounds', where N is the prefix arg.
;;
;;  Command `hlt-highlight-regexp-groups-region', like command
;;  `hlt-highlight-regexp-region', highlights regexp matches.  But
;;  unlike the latter, it highlights the regexp groups (up to 8
;;  levels) using different faces - faces `hlt-regexp-level-1' through
;;  `hlt-regexp-level-8'.  Use it, for example, when you are trying
;;  out a complex regexp, to see what it is actually matching.
;;  Command `hlt-unhighlight-regexp-groups-region' unhighlights such
;;  highlighting.
;;
;;  Command `hlt-highlight-line-dups-region' highlights the sets of
;;  duplicate lines in the region (or the buffer, if the region is not
;;  active).  By default, leading and trailing whitespace are ignored
;;  when checking for duplicates, but this is controlled by option
;;  `hlt-line-dups-ignore-regexp'.  And with a prefix arg the behavior
;;  effectively acts opposite to the value of that option.  So if the
;;  option says not to ignore whitespace and you use a prefix arg then
;;  whitespace is ignored, and vice versa.
;;
;;  If you use Emacs 21 or later, you can use various functions that
;;  highlight and unhighlight text that has certain text properties
;;  with given values.  You can use them to highlight all text in the
;;  region or buffer that has a given property value.  An example is
;;  highlighting all links (text with property `mouse-face').  These
;;  functions are as follows (all except the `-mouse-' functions are
;;  commands):
;;
;;  `hlt-highlight-all-prop' - Highlight text that has a given
;;                             property with any (non-nil) value.
;;
;;  `hlt-highlight-property-with-value' - Highlight text that has a
;;                             given property with certain values.
;;
;;  `hlt-unhighlight-all-prop' - Unhighlight highlighted propertized
;;                             text.
;;
;;  `hlt-mouse-toggle-link-highlighting' - Alternately highlight and
;;                             unhighlight links with a mouse click.
;;                             (Use it on `post-command-hook'.)
;;
;;  `hlt-toggle-link-highlighting' - Alternately highlight and
;;                             unhighlight links.
;;
;;  `hlt-mouse-toggle-property-highlighting' - Alternately highlight
;;                             and unhighlight propertized text on a
;;                             mouse click.
;;                             (Use it on `post-command-hook'.)
;;
;;  `hlt-toggle-property-highlighting' - Alternately highlight and
;;                             unhighlight propertized text.
;;
;;  As always for library `highlight.el', this "highlighting" can use
;;  property `mouse-face' instead of `face'.  You could, for example,
;;  highlight, using `mouse-face', all text that has property `foo' -
;;  or that has property `face', for that matter.
;;
;;  If you use Emacs 21 or later, you can use commands
;;  `hlt-next-highlight' and `hlt-previous-highlight' to navigate
;;  among highlights of a given face.
;;
;;  You can unhighlight the region/buffer or a regexp in the
;;  region/buffer using command `hlt-unhighlight-region' or
;;  `hlt-unhighlight-regexp-region'.  If you use overlay highlighting
;;  then you can use command `hlt-unhighlight-region-for-face' to
;;  unhighlight the region/buffer for an individual highlighting face
;;  - other highlighting faces remain.
;;
;;  You can replace a highlighting face in the region/buffer by
;;  another, using command `hlt-replace-highlight-face'.  With a
;;  prefix argument, property `mouse-face' is used, not property
;;  `face'.
;;
;;  Command `hlt-highlight-single-quotations' highlights single-quoted
;;  text in the region.  For example, Emacs commands and keys between
;;  ` and ': `foobar'.
;;
;;  Command `hlt-mouse-face-each-line' puts a `mouse-face' property on
;;  each line of the region.
;;
;;  Command `hlt-highlight-lines' highlights all lines touched by the
;;  region, extending the highlighting to the window edges.
;;
;;  You can highlight and unhighlight multiple buffers at the same
;;  time.  Just as for a single buffer, there are commands for regexp
;;  (un)highlighting, and all of the multiple-buffer commands, whose
;;  names end in `-in-buffers', are sensitive to the region in each
;;  buffer, when active.  These are the multiple-buffer commands:
;;
;;  `hlt-highlight-region-in-buffers'
;;  `hlt-unhighlight-region-in-buffers'
;;  `hlt-highlight-regexp-region-in-buffers'
;;  `hlt-unhighlight-regexp-region-in-buffers'
;;  `hlt-unhighlight-region-for-face-in-buffers'
;;  `hlt-replace-highlight-face-in-buffers'
;;
;;  Normally, you are prompted for the names of the buffers, one at a
;;  time.  Use `C-g' when you are done entering buffer names.  But a
;;  non-positive prefix arg means act on all visible or iconified
;;  buffers.  (A non-negative prefix arg means use property
;;  `mouse-face', not `face'.)
;;
;;  If you also use library `zones.el' then narrowing and other
;;  operations record buffer zones (including narrowings) in (by
;;  default) buffer-local variable `zz-izones'.  Besides narrowing,
;;  you can use `C-x n a' (command `zz-add-zone') to add the current
;;  region to the same variable.
;;
;;  You can use command `hlt-highlight-zones' to highlight buffer
;;  zones, as defined by their limits (interactively, `zz-izones'),
;;  and you can use command `hlt-highlight-zones-in-buffers' to
;;  highlight all zones recorded for a given set of buffers.  You can
;;  use commands `hlt-unhighlight-zones' and
;;  `hlt-unhighlight-zones-in-buffers' to unhighlight them.  If option
;;  `hlt-auto-faces-flag' is non-nil then each zone gets a different
;;  face.  Otherwise, all of them are highlighted with the same face.
;;
;;  From Isearch you can highlight the search-pattern matches.  You
;;  can do this across multiple buffers being searched together.
;;  These keys are bound on the Isearch keymap for this:
;;
;;   `M-s h h' - `hlt-highlight-isearch-matches'
;;   `M-s h u' - `hlt-unhighlight-isearch-matches'
;;
;;  Unlike standard Isearch key `M-s h r' (`isearch-highlight-regexp')
;;  `M-s h h' and `M-s h u' use only highlighting created by library
;;  `highlight', and they do not exit Isearch.
;;
;;(@* "Copy and Yank (Paste) Text Properties")
;;  ** Copy and Yank (Paste) Text Properties **
;;
;;  You can highlight or unhighlight text by simply copying existing
;;  highlighting (or lack of any highlighting) from anywhere in Emacs
;;  and yanking (pasting) it anywhere else.
;;
;;  Put differently, you can copy and yank a set of text properties.
;;  You can use these commands to copy and yank any text properties,
;;  not just `face' or `mouse-face'.
;;
;;  To copy the text properties at a given position, use command
;;  `hlt-copy-props'.  You can then use command `hlt-yank-props' to
;;  yank those properties to the active region anywhere.  If the set
;;  of properties that you copy is empty, then yanking means
;;  effectively removing all text properties.
;;
;;  User option `hlt-default-copy/yank-props' controls which text
;;  properties to copy and yank, by default.  The default value of the
;;  option includes only `face', which means that only property `face'
;;  is copied and pasted.  That is typically what you want, for
;;  highlighting purposes.  A value of `t' for
;;  `hlt-default-copy/yank-props' means use all properties.
;;
;;  You can further control which text properties are copied or yanked
;;  when you use the commands, by using a prefix argument.  A plain or
;;  non-negative prefix arg means copy or yank all available text
;;  properties.  A negative prefix arg (e.g. `C--') means you are
;;  prompted for which text properties to use, among those available.
;;
;;  For copying, the available properties are those among
;;  `hlt-default-copy/yank-props' that are also present at the copy
;;  position.  For yanking, the available properties are those among
;;  `hlt-default-copy/yank-props' that have previously (last) been
;;  copied.
;;
;;(@* "User Option `hlt-act-on-any-face-flag'")
;;  ** User Option `hlt-act-on-any-face-flag' **
;;
;;  Library `highlight' generally acts only on faces that it controls,
;;  that is, faces that you have explicitly asked it to use for
;;  highlighting.  It sets the text property or overlay property
;;  `hlt-highlight' on such highlighted text, so that it can recognize
;;  which faces it has responsibility for.
;;
;;  Sometimes, you might want to hide and show text other than that
;;  controlled by library `highlight'.  Similarly, you might sometimes
;;  want to navigate among faces other than those used for
;;  highlighting.  You can control this using option
;;  `hlt-act-on-any-face-flag', which you can toggle at any time using
;;  command `hlt-toggle-act-on-any-face-flag'.
;;
;;(@* "Hiding and Showing Text")
;;  ** Hiding and Showing Text **
;;
;;  You can hide and show text that you have highlighted.  You will
;;  want to read the Emacs-Lisp manual (Elisp), section Invisible
;;  Text, to understand better what this entails.  In particular, you
;;  should understand that for library `highlight.el', hiding text
;;  means adding the symbol naming the face to be hidden to both:
;;
;;  1. a text or overlay `invisible' property, making the text or
;;     overlay susceptible to being hidden by buffer-local variable
;;     `buffer-invisibility-spec', and
;;
;;  2. the buffer's `buffer-invisibility-spec', so that it in fact
;;     becomes hidden.
;;
;;  After text has been hidden this way, and unless the highlighting
;;  has been removed completely by unhighlighting the text, the
;;  `invisible' property of that text keeps the names of the faces
;;  that have been applied to that text and hidden previously, even
;;  after you show that text again.  Showing a hidden face simply
;;  removes it from the `buffer-invisibility-spec'; it does not change
;;  any `invisible' properties.
;;
;;  For example, if you hide face `foo' at some buffer position:
;;
;;  1. The `invisible' property of the text or overlay at that
;;     position is updated to include `foo'.  If there are no other
;;     faces that have been applied to this text and then hidden, the
;;     `invisible' property is just `(foo)'.
;;
;;  2. `buffer-invisibility-spec' is also updated to include `foo'.
;;     This hides all text properties and overlay properties with
;;     `invisible' property `foo', throughout the buffer.  If there
;;     are no other invisible faces in the buffer, then
;;     `buffer-invisibility-spec' has value (foo).
;;
;;  If you then show face `foo' at that same buffer position, there is
;;  no change to the `invisible' property.  `buffer-invisibility-spec'
;;  is updated, by removing `foo': if it was (foo), it becomes ().
;;
;;  There are several commands for hiding and showing highlighted
;;  text.  The basic commands for hiding and showing are
;;  `hlt-hide-default-face' and `hlt-show-default-face', which you can
;;  use to hide and show the face last used for highlighting.  With a
;;  prefix argument, you are prompted for a different face to hide or
;;  show; it then becomes the default face for highlighting.  You can
;;  also change the default highlighting face at any time using
;;  command `hlt-choose-default-face'.
;;
;;(@* "Hiding and Showing Text - Icicles Multi-Commands")
;;  *** Hiding and Showing Text - Icicles Multi-Commands ***
;;
;;  The other hide and show commands depend on your also using
;;  Icicles, which is a set of libraries that offer enhanced
;;  completion.  Complete information about Icicles is here:
;;  `https://www.emacswiki.org/emacs/Icicles'.  You can obtain Icicles
;;  here: `https://www.emacswiki.org/emacs/Icicles_-_Libraries'.
;;
;;  The Icicles commands defined for `highlight.el' are the following:
;;
;;  `icicle-choose-faces', `icicle-choose-invisible-faces',
;;  `icicle-choose-visible-faces', `icicle-hide-faces',
;;  `icicle-hide-only-faces', `icicle-show-faces',
;;  `icicle-show-only-faces'.
;;
;;  These are all Icicles multi-commands, which means that they each
;;  let you choose multiple completion candidates or all candidates
;;  that match your current input (a regexp).  To use them you must
;;  also use Icicles.  You can use command `icicle-hide-faces' to hide
;;  any number of visible faces.  Any text is hidden that has that
;;  face as a text property or an overlay property, depending on the
;;  value of `hlt-use-overlays-flag'.
;;
;;  Command `icicle-show-faces' is the opposite of
;;  `icicle-hide-faces': it shows invisible text that has the faces
;;  you choose.  Neither `icicle-hide-faces' nor `icicle-show-faces'
;;  has any effect on other faces, besides those you choose to hide or
;;  show, respectively; they each do only one thing, hide or show.
;;
;;  Command `icicle-hide-only-faces' hides the faces you choose, and
;;  shows all other faces, and command `icicle-show-only-faces' does
;;  the opposite.  You can thus use these commands to specify exactly
;;  what faces should be invisible and visible.  Empty input means
;;  none: If you choose no faces to hide (that is, hit `RET' with an
;;  empty minibuffer), then all faces are made visible; if you choose
;;  no faces to show, then all are hidden.
;;
;;  Currently, face attributes for highlighting are combined when
;;  overlays overlap, but the same is not true for text properties.
;;  For example, if you highlight a word with face `foo', and then you
;;  highlight it with face `bar', only `bar' remains as the face for
;;  that word.  With overlays, the attributes of the two faces are
;;  composed.  When you hide or show faces, this behavior difference
;;  has an effect.
;;
;;  You can hide text using the commands in this library for any of
;;  the purposes that you might use invisible text in Emacs.  This
;;  gives you an easy, interactive way to control which sections of
;;  text are seen by search and other Emacs tools.  Use the regexp
;;  highlighting commands, for instance, to highlight text
;;  syntactically, and then hide that highlighted text.  Or use
;;  `hlt-highlighter' to sweep over text that you want to hide with
;;  the mouse.
;;
;;  Hiding and showing faces also provides a "conditional text"
;;  feature similar to that available in desktop publishing
;;  applications such as Adobe Framemaker.  Publishers often use such
;;  a feature to produce different output documents from the same
;;  source document ("single sourcing").  You can use this feature
;;  similarly, if you have an application (printing is one example)
;;  that is sensitive to whether text is visible or invisible.  One
;;  caveat: Emacs faces are not saved when you save your file.
;;
;;(@* "What Gets Highlighted: Region, Buffer, New Text You Type")
;;  ** What Gets Highlighted: Region, Buffer, New Text You Type **
;;
;;  Most mention of the "region" in this commentary should really say
;;  "active region or buffer".  If the region is active and non-empty,
;;  then only the text in the region is targeted by the commands in
;;  this library.  This lets you easily control the scope of
;;  operations.
;;
;;  If the region is not active or it is empty, then:
;;
;;  - If `hlt-use-overlays-flag' is nil and there is no prefix arg,
;;    then the face is applied to the next characters that you type.
;;
;;  - Otherwise, the face is applied to the entire buffer (or the
;;    current restriction, if the buffer is narrowed).
;;
;;(@* "Interaction with Font Lock")
;;  ** Interaction with Font Lock **
;;
;;  Any highlighting that uses text property `face' is overruled by
;;  font-lock highlighting - font-lock wants to win.  (This does not
;;  apply to highlighting that uses overlays - font-lock has no effect
;;  on overlays.)  In many cases you can still highlight text, but
;;  sooner or later font-lock erases that highlighting when it
;;  refontifies the buffer.
;;
;;  To prevent this interference of font-lock with other highlighting,
;;  the typical Emacs approach is to fool font-lock into thinking that
;;  it is font-lock highlighting, even when it does not involve
;;  `font-lock-keywords'.
;;
;;  But this has the effect that such highlighting is turned off when
;;  `font-lock-mode' is turned off.  Whether this is a good thing or
;;  bad depends on your use case.
;;
;;  In vanilla Emacs you have no choice about this.  Either the
;;  highlighting is not recognized by font-lock, which overrules it,
;;  or it is recognized as "one of its own", in which case it is
;;  turned off when font-lock highlighting is turned off.  With
;;  library `highlight.el' things are more flexible.
;;
;;  First, there is option `hlt-face-prop', whose value determines the
;;  highlighting property: Value `font-lock-face' means that the
;;  highlighting is controlled by font-lock.  Value `face' means that
;;  `font-lock' does not recognize the highlighting.
;;
;;  Second, for the case where the option value is `face', if you also
;;  use library `font-lock+.el' then there is no interference by
;;  font-lock - the highlighting is independent of font-lock.  Library
;;  `font-lock+.el' is loaded automatically by `highlight.el', if it
;;  is in your `load-path'.  It prevents font-locking from removing
;;  any highlighting face properties that you apply using the commands
;;  defined here.
;;
;;  Then font-lock does not override this highlighting with its own,
;;  and it does not turn this highlighting on and off.  Depending on
;;  your application, this can be quite important.
;;
;;  The default value of option `hlt-face-prop' is `font-lock-face'.
;;  If you want text-property highlighting that you add to be able to
;;  persist and be independent of font-locking, then change the value
;;  to `face' and put library `font-lock+.el' in your `load-path'.
;;
;;  [If you also load library `facemenu+.el', then the same applies to
;;  highlighting that you apply using the face menu: `font-lock+.el'
;;  also protects that highlighting from interference by font-lock.]
;;
;;(@* "Suggested Bindings")
;;  ** Suggested Bindings **
;;
;;  Library `highlight.el' binds many of its commands to keys on the
;;  prefix key `C-x X'.  It also adds menu items to the `Region'
;;  submenu of the `Edit' menu-bar menu, if you have a `Region'
;;  submenu.  To obtain this menu, load library `menu-bar+.el'.
;;
;;  Here are some additional, suggested key bindings (`C-x C-y', `C-x
;;  mouse-2', `C-x S-mouse-2', `C-S-p', and `C-S-n', respectively):
;;
;;   (define-key ctl-x-map [(control ?y)]     'hlt-highlight)
;;   (define-key ctl-x-map [(down-mouse-2)]   'hlt-highlighter)
;;   (define-key ctl-x-map [(S-down-mouse-2)] 'hlt-eraser)
;;   (global-set-key [(shift control ?p)]     'hlt-previous-highlight)
;;   (global-set-key [(shift control ?n)]     'hlt-next-highlight)
;;   (global-set-key [(control meta shift ?s)]
;;                   'hlt-highlight-enclosing-list)
;;
;;(@* "See Also")
;;  ** See Also **
;;
;;  * `highlight-chars.el' - Provides ways to highlight different sets
;;    of characters, including whitespace and Unicode characters.  It
;;    is available here:
;;    https://www.emacswiki.org/emacs/download/highlight-chars.el (code)
;;    https://www.emacswiki.org/emacs/ShowWhiteSpace#HighlightChars (doc)
;;
;;  * `hi-lock.el' - The features of `highlight.el' are complementary
;;    to those of vanilla Emacs library `hi-lock.el', so you can use
;;    the two libraries together.  See this page for a comparison:
;;    https://www.emacswiki.org/emacs/HighlightTemporarily.
;;
;;(@* "Commands That Won't Work in Emacs 20")
;;  ** Commands That Won't Work in Emacs 20 **
;;
;;  The following commands and options work only for Emacs versions
;;  more recent than Emacs 20:
;;
;;  `hlt-act-on-any-face-flag', `hlt-hide-default-face',
;;  `hlt-highlight-line-dups-region',
;;  `hlt-highlight-property-with-value', `hlt-next-highlight',
;;  `hlt-previous-highlight', `hlt-show-default-face',
;;  `hlt-toggle-act-on-any-face-flag'.
;;
;;(@* "To Do")
;;  ** To Do **
;;
;;  1. Add commands to show and hide boolean combinations of faces.
;;
;;  2. Faces are not accumulated as text properties.
;;     Highlighting with one face completely replaces the previous
;;     highlight.  Overlays don't have this limitation.  Text
;;     properties need not have it either, but they do, for now.
;;
;;(@* "Acknowledgement")
;;  **  Acknowledgement **
;;
;;  Some parts of this library were originally based on a library of
;;  the same name written and copyrighted by Dave Brennan,
;;  brennan@hal.com, in 1992.  I haven't been able to locate that
;;  file, so my change log is the only record I have of what our
;;  relative contributions are.
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;(@* "Change log")
;;
;; 2025/07/24 dadams
;;     Wrap zones highlighting with (when (> emacs-major-version 21)...).
;;     Don't use define-obsolete-function-alias, for compatibility with Emacs 20 byte-compiler.
;; 2025/07/21 dadams
;;     Renamed hlt-highlight-regions(-in-buffers) to hlt-highlight-zones(-in-buffers).  Replaced obsolete
;;      name zz-izone-limits(-in-bufs) with new name zz-basic-zones(in-bufs).
;;     hlt-highlight-zones: Unhighlight if non-positive prefix arg.
;;     hlt-(un)highlight-zones: Use zz-basic-zones, not zz-izone-limits.
;;     hlt-eraser: Remove property font-lock-face also.
;;     hlt-highlight-region: Misc. changes. Better msgs. When MOUSEP highlight normally as well as mouse-face.
;;     hlt-unhighlight-region-in-buffers: Corrected 5th arg to hlt-unhighlight-region.
;;     hlt-unhighlight-region: Remove property font-lock-face also.  Better msgs.
;;     hlt-highlight-property-with-value: Misc. changes. Better msgs.
;;     hlt-highlight-all-prop: Read PROPERTY in interactive spec.
;; 2023/01/10 dadams
;;     Added hlt-(un)highlight-isearch-matches bindings for search-map (Emacs 23+).
;;     Added key bindings for search-map.
;;     Changed key-binding suffix for hlt-highlight-region to h SPC from h r, because vanilla search-map
;;      uses h r for highlight-regexp.
;; 2022/03/20 dadams
;;     hlt-unhighlight-isearch-matches: Corrected defaulting STRING to isearch-string.
;;     hlt-(un)highlight-isearch-matches:
;;       If empty isearch-string then read STRING.
;;       Corrected order of args in interactive spec.
;;       Improved doc string.
;;       In Commentary, mention differences from standard isearch-highlight-regexp (M-s h r).
;; 2021/03/18 dadams
;;     Need to require facemenu.el now.  No longer preloaded for Emacs 28+.
;; 2019/07/10 dadams
;;     hlt-+/--highlight-regexp-region: Removed incrementing START in loop - useless.
;; 2019/07/09 dadams
;;     hlt-+/--highlight-regexp-region:
;;       forward-char in loop only when match empty text.  Reported by Simon Katz.
;; 2018/12/28 dadams
;;     hlt-(un)highlight-regions: Pass zz-izones as explicit arg to zz-izone-limits.
;; 2018/11/12 dadams
;;     hlt-(next|previous)-highlight: Added optional arg WRAP-P.  In general, just for interactive use.
;;                                    Use WRAP-P in while tests.
;; 2018/10/20 dadams
;;     hlt-highlight-isearch-matches: Fixed interactive spec: put STRING last, not first.
;; 2018/10/19 dadams
;;     hlt-next-highlight: Corrected wrapping.
;; 2018/10/18 dadams
;;     hlt-next-highlight: Do not wrap-around more than once.
;; 2018/09/18 dadams
;;     hlt-hide-default-face: If FACE is nil then set it to hlt-last-face (in body, not just interactive).
;; 2017/10/15 dadams
;;     Added: hlt-user-error.  Use it for user errors.
;;     hlt-highlighter, hlt-eraser: Better error message if drag out of window.
;; 2017/06/30 dadams
;;     hlt-(un)highlight-regions: Define even if zones.el is not loaded.  Uses zones.el only for interactive.
;; 2016/12/23 dadams
;;     Added: hlt-string-match-p.
;;     Faces hlt-regexp-level-*: Do not inherit.
;;     hlt-highlight-region: If FACE is hlt-regexp-level-* then do not update hlt-last-face.
;;     hlt-highlight-regexp-groups:
;;       Highlight non-group parts using hlt-last-face.  Use increasing overlay priorities.
;; 2016/12/19 dadams
;;     hlt-regexp-level-*: Use default background, not isearch-face (typo), as base for hexrgb changes.
;;                         Fix :group (typo).
;;     Require cl.el when compile, if Emacs 20.
;; 2016/12/18 dadams
;;     Added:
;;       hlt-highlight-regexp-groups-region, hlt-unhighlight-regexp-groups-region,
;;       hlt-highlight-regexp-groups, hlt-regexp-level-1, hlt-regexp-level-2, hlt-regexp-level-3,
;;       hlt-regexp-level-4, hlt-regexp-level-5, hlt-regexp-level-6, hlt-regexp-level-7, hlt-regexp-level-8.
;;     hlt-+/--highlight-regexp-region:
;;       If FACE is symbol :hlt-regexp-groups, then (un)highlight regexp groups and ignore NTH and BUFFERS.
;;     hlt-face-prop: For Emacs < 22, default value is face, not font-lock-face.
;; 2016/11/03 dadams
;;     hlt-(un)highlight-symbol: Added optional arg FACE.  Removed unused args START and END.
;; 2015/12/04 dadams
;;     Added: hlt-highlight-lines.
;; 2016/08/16 dadams
;;     Renamed wide-n.el stuff to zones.el stuff.
;; 2015/08/06 dadams
;;     hlt-read-bg/face-name: Bind icicle-unpropertize-completion-result-flag to t.
;;     hlt-choose-default-face: Use %S, not %s, in message, so color names in a cons are shown as strings.
;; 2015/07/31 dadams
;;     hlt-(un)highlight-regions: Use new wide-n-restrictions format: (NUM BEG END), not (NUM BEG . END).
;; 2015/07/11 dadams
;;     Added: hlt-highlight-regions, hlt-highlight-regions-in-buffers,
;;            hlt-unhighlight-regions, hlt-highlight-regions-in-buffers.
;;     hlt-highlight-region-in-buffers: Added missing t for MSGP in interactive spec.
;;     hlt-(un)highlight-region, hlt-+/--highlight-regexp-region, hlt-replace-highlight-face:
;;       Do not ignore explicit START and END if not interactive.
;;       Do nothing if START or END is a marker for a different buffer.
;;     hlt-highlight-line-dups-region: Added missing local binding of IGNORE.
;;     hlt-+/--read-bufs: Mention C-g in prompt, to end inputting.
;; 2015/06/17 dadams
;;     Added: hlt-highlight-line-dups-region, hlt-line-dups-ignore-regexp.
;; 2015/04/05 dadams
;;     Added: hlt-face-prop.
;;     hlt-highlighter, hlt-unhighlight-for-overlay, hlt-highlight-region, hlt-replace-highlight-face,
;;       hlt-yank-props, hlt-hide-default-face, hlt-next-highlight, hlt-highlight-faces-in-buffer:
;;         Use value of hlt-face-prop, not symbol face.
;;     hlt-highlighter, hlt-eraser, hlt-(un)highlight-region, hlt-yank-props:
;;       Add font-lock-ignore only when hlt-face-prop is the symbol face.
;;     hlt-overlays-priority: Changed default priority to 0, from 200.
;; 2015/04/03 dadams
;;     Added: hlt-highlight-isearch-matches, hlt-unhighlight-isearch-matches.
;;     Added: hlt-overlays-priority.
;;       Use it in hlt-highlighter, hlt-highlight-region, hlt-mouse-face-each-line.
;;     Bind isearch-mode-map keys M-s h h, M-s h u to hlt-(un)highlight-isearch-matches.
;; 2014/09/21 dadams
;;     Added: hlt-replace-highlight-face-in-buffers, hlt-highlight-region-in-buffers,
;;            hlt-highlight-region-in-buffers, hlt-replace-highlight-face-in-buffers,
;;            hlt-unhighlight-regexp-region-in-buffers, hlt-unhighlight-region-for-face-in-buffers,
;;            hlt-unhighlight-region-in-buffers, hlt-+/--read-regexp, hlt-+/--read-bufs.
;;     hlt-unhighlight-for-overlay: Added MOUSEP arg.  It no longer removes mouse-face without MOUSEP.
;;     hlt(-un)-highlight(-regexp)-region, hlt-+/--highlight-regexp-region, hlt-unhighlight-region-for-face,
;;       hlt-replace-highlight-face, hlt-region-or-buffer-limits:
;;         Added BUFFERS arg - can act on multiple buffers.
;;     hlt-(un)highlight-regexp-region: Corrected interactive spec.
;;     hlt-unhighlight-region-for-face: Added MSGP arg.
;;     hlt-+/--highlight-regexp-read-args: Use hlt-+/--read-regexp.
;;     hlt-read-bg/face-name: If in Icicle mode, use WYSIWYG completion candidates.
;; 2014/05/27 dadams
;;     hlt-(un)highlight-symbol: A prefix arg means act on all visible buffers.
;; 2014/02/25 dadams
;;     Added: hlt-highlight-symbol, hlt-unhighlight-symbol.
;;     hlt-+/--highlight-regexp-region: Prevent doing hlt-next-face more than once.
;;     hlt-next-highlight: Interactive spec uses face at point if hlt-auto-faces-flag.
;;                         Use get-char-property, not get-text-property.
;;                         Wrap around.
;;     Bind C-x X hs, C-x X us to hlt-(un)highlight-symbol.
;; 2014/02/19 dadams
;;     hlt-+/--highlight-regexp-region:
;;       If UNHIGHLIGHTP: Do not advance to hlt-next-face.
;;                        If FACE is nil, unhighlight for all faces.
;; 2014/02/06 dadams
;;     hlt-+/--highlight-regexp-region: When hlt-auto-faces-flag, cycle to next face.
;; 2013/11/28 dadams
;;     Renamed hlt-read-face-name to hlt-read-bg/face-name, and rewrote:
;;       Added optional DEFAULT arg, read also color names.  No longer use read-face-name.
;;       (So no longer soft-require faces+.el.)
;;     hlt-choose-default-face: Read color names also.
;;     hlt-eraser, hlt-highlighter: Added optional arg FACE-NB.  Show message.
;;     hlt-highlight-region: If hlt-auto-faces-flag, use hlt-next-face.
;; 2013/11/24 dadams
;;     Use equal, not eq, for face comparisons, since the value could be a property list.
;; 2013/11/15 dadams
;;     Added: hlt-unhighlight-regexp-region, hlt-unhighlight-regexp-to-end,
;;            hlt-+/--highlight-regexp-read-args, hlt-+/--highlight-regexp-region, hlt-read-face-name.
;;     hlt-highlight-regexp-(region|to-end):
;;       Use hlt-+/--highlight-regexp-read-args and hlt-+/--highlight-regexp-region.
;;     hlt-highlight: Change use of prefix arg.  Include hlt-unhighlight-regexp-region.
;; 2013/11/07 dadams
;;     Added: hlt-highlight-enclosing-list.
;; 2013/09/15 dadams
;;     Soft-require font-lock+.el (Emacs 22+).
;; 2013/07/24 dadams
;;     Added: hlt-nonempty-region-p.
;;     Renamed: Paste Text Properties to * to Region, except in menu-bar-edit-region-menu.
;;     Wherever hlt-yank-props is used in menus: Enable only if hlt-nonempty-region-p and hlt-copied-props.
;;     hlt-highlight-region, hlt-highlight-property-with-value:
;;       Use hlt-nonempty-region-p, not just mark-active.
;;     hlt-yank-props, hlt-region-or-buffer-limits: Use hlt-nonempty-region-p.
;; 2013/05/28 dadams
;;     Require easymenu.el.
;;     hlt-highlighter, hlt-eraser: Wrap with with-current-buffer.  Thx to Michael Heerdegen.
;; 2012/07/11 dadams
;;     hlt-(highlighter|eraser)-mouse: Skip over event of choosing menu item, for Emacs 20-21.
;; 2011/12/01 dadams
;;     hlt-eraser: Fixed so it works backwards too.  Thx to Michael Heerdegen.
;;     hlt-unhighlight-region, hlt-replace-highlight-face, hlt-eraser: Use dolist, not mapcar.
;; 2011/11/04 dadams
;;     hlt-default-copy/yank-props: Allow a value of t, for all props.
;;     hlt-props-to-copy/yank: Handle t value of hlt-default-copy/yank-props.
;; 2011/10/31 dadams
;;     hlt-highlight-regexp-region: No occurrences msg if no match, not msg how to unhighlight.
;; 2011/09/13 dadams
;;     hlt-highlight-property-with-value: Corrected interactive spec for VALUES.
;; 2011/07/24 dadams
;;     Moved to icicles-cmd2.el, renamed with prefix icicle- from hlt-, and corrected them:
;;       hlt-(hide|show)(-only)-faces, hlt-choose(-(in)visible)-faces.
;;     menu-bar-edit-menu, facemenu(-mouse)-menu: Added hlt-(copy|yank)-props.
;; 2011/07/23 dadams
;;     Added: hlt-((mouse-)copy|yank|paste)-props, hlt-copied-props, hlt-subplist,
;;            hlt-default-copy/yank-props, hlt-read-props-completing, hlt-props-to-copy/yank.
;;     Added defgroup.  Updated defcustom/defface to use :group highlight.
;;     menu-bar-edit-region-menu: Added hlt-yank-props, hlt-unhighlight-region-for-face.
;;                                Removed needing region for highlighting enablement.
;; 2011/05/05 dadams
;;     icicle-delete-if(-not) -> icicle-remove-if(-not).  Former are obsolete.
;;     hlt-hide-default-face, hlt-next-highlight: Use also memq, not just eq, to test for face.
;; 2011/01/04 dadams
;;     Removed autoload cookies from non def* sexps and non-interactive functions.
;;     Added some missing autoload cookies for commands.
;; 2010/11/26 dadams
;;     Added: hlt-unhighlight-for-overlay.
;;     hlt-eraser, hlt-unhighlight-region: Use hlt-unhighlight-for-overlay, not hlt-delete-highlight-overlay.
;;     hlt-eraser: Update doc string to reflect new behavior.
;; 2009/09/24 dadams
;;     Removed hlt-no-op - use function ignore instead.
;; 2009/08/02 dadams
;;     Added: hlt(-mouse)-toggle-(link|property)-highlighting, hlt-(un)highlight-all-prop,
;;            hlt-property-highlight, hlt-prop-highlighting-state.
;; 2009/07/31 dadams
;;     Added: hlt-highlight-property-with-value, hlt-flat-list, hlt-set-intersection.
;; 2009/04/26 dadams
;;     hlt-mouse-face-each-line: Bind inhibit-field-text-motion to  t, for real eol.
;; 2008/01/17 dadams
;;     Removed soft require of icicles.el.
;; 2007/11/27 dadams
;;     hlt-highlight-regexp-region: If available, use icicle-read-string-completing.
;; 2007/08/12 dadams
;;     Moved here from menu-bar+.el: Add to Edit>Region menu.  Soft require menu-bar.el.
;; 2007/06/07 dadams
;;     Use face-name-history or icicle-face-name-history, if defined, else face-name-history.
;; 2007/06/05 dadams
;;     Added: hlt-(highlighter|eraser)-mouse.
;; 2007/06/04 dadams
;;     Added: hlt-previous-use-overlays-flag-value.
;;     hlt-use-overlays-flag: 3 values now; default is only.
;;     hlt-eraser, hlt-unhighlight-region, hlt-hide-default-face, hlt-next-highlight:
;;       Treat non-only, non-nil hlt-use-overlays-flag.
;;     hlt-toggle-use-overlays-flag: Use hlt-previous-use-overlays-flag-value.
;;     Updated doc.
;; 2007/06/03 dadams
;;     Added: hlt-toggle-use-overlays-flag.
;;     Don't even define hlt-act-on-any-face-flag for Emacs 20.
;;     Renamed no-op to hlt-no-op. Removed soft require of misc-cmds.el.
;; 2007/06/02 dadams
;;     Added: hlt-act-on-any-face-flag, hlt-add-listifying, hlt-add-to-invisibility-spec,
;;            hlt-choose(-(in)visible)-faces, hlt-(hide|show)(-default-face|-only),
;;            hlt-highlight-faces-in-buffer, hlt-set-union, hlt-toggle-act-on-any-face-flag.
;;     Renamed: highlight-use-overlays-flag to hlt-use-overlays-flag,
;;              highlight-max-region-no-warning to hlt-max-region-no-warning,
;;              highlight-last-regexp to hlt-last-regexp, highlight-last-face to hlt-last-face,
;;              highlight-face to hlt-choose-default-face,
;;              highlight-highlighter to hlt-highlighter, highlight-eraser to hlt-eraser,
;;              mouse-face-each-line to hlt-mouse-face-each-line,
;;              unhighlight-region(-for-face) to hlt-unhighlight-region(-for-face).
;;     hlt-highlighter, hlt-highlight-region, hlt-mouse-face-each-line:
;;       Also put face as hlt-highlight property.
;;     hlt-eraser: Also remove hlt-highlight property.
;;     hlt-highlight-region, hlt-unhighlight-region, hlt-replace-highlight-face, hlt-next-highlight,
;;       hlt-mouse-face-each-line, hlt-highlight-regexp-region:
;;         Made start, end, and regexp args optional too.  Default for non-interactive too.
;;     hlt-unhighlight-region-for-face: Made all args optional.  Default them.
;;     hlt-unhighlight-region: Only remove highlighting for FACE, not all faces.
;;     hlt-highlight-single-quotations: Update hlt-last-face.
;;     hlt-next-highlight:
;;       Respect hlt-act-on-any-face-flag.  Return a cons of the limits.  Added no-error-p arg.
;;     hlt-previous-highlight: Added no-error-p arg.
;;     Added soft require of Icicles.
;; 2007/04/02 dadams
;;     Renamed highlight-region-beg-end to highlight-region-or-buffer-limits.
;; 2007/03/25 dadams
;;     highlight-highlighter, highlight-eraser, highlight-region, unhighlight-region: Use font-lock-ignore.
;;     highlight-regexp-*: Use hi-lock-regexp-history or regexp-history.
;; 2007/03/23 dadams
;;     highlight-region:
;;       If no region and no overlay, apply face to next char typed & add to facemenu menu.
;;     highlight-highlighter: Don't create overlay unless highlight-use-overlays-flag.
;;     highlight-highlighter, highlight-region, highlight-eraser:
;;       Don't bother to call font-lock-after-fontify-buffer.
;;     highlight-highlighter, highlight-region: Prepare for possible font-lock-ignore prop.
;;     highlight: Removed message.
;; 2007/03/20 dadams
;;     highlight-face: Add face as arg.  Added final message.
;; 2007/03/17 dadams
;;     Added: highlight-(next|previous)-highlight, highlight-region-beg-end, highlight-eraser.
;;     highlight-region, highlight-regexp-to-end, highlight-regexp-region:
;;       Interactively, use highlight-last-face as the face.
;;     highlight-single-quotations: Added prefix arg, meaning prompt for face.
;;     highlight-region, highlight-regexp-region, unhighlight-region(-for-face),
;;     *-replace-face, *-single-quotations: If no region, then use whole buffer.
;;     highlight-single-quotations:
;;       Use highlight-regexp-region, not highlight-regexp-to-end.  Msg if interactive.
;;     highlight-regexp-region: Ensure move past match in loop.  Face is optional now.
;;     mouse-face-each-line: Added args START, END, FACE, MSGP. Restrict to region.
;;     Removed: mouse-face-following-lines.
;;     highlight-region: Added MSGP arg and progress message.
;;     unhighlight-region, highlight-replace-face: Simple message, no where part.
;;     unhighlight-region: Changed order of optional args, for consistency.
;;     highlight-highlighter: Make overlay once, and move it.  Initialize end-point to start-point.
;; 2007/03/16 dadams
;;     Renamed highlight-regexp to highlight-regexp-to-end, because Emacs now uses that name.
;;     Renamed max-highlight-w-o-warning to highlight-max-region-no-warning.
;;     Added: highlight-use-overlays-flag, highlight-last-face, highlight-face, highlight-highlighter,
;;            unhighlight-region-for-face, highlight-replace-face, highlight-delete-overlay.
;;     highlight-single-quotations: Read the face name.
;;     highlight-single-quotations, highlight-region, highlight-regexp-to-end,
;;     highlight-regexp-region: Set highlight-last-face.
;;     unhighlight-region, highlight-region, mouse-face-following-lines,
;;     mouse-face-each-line: Respect highlight-use-overlays-flag.
;;     unhighlight-region, mouse-face-*: Added optional face arg.
;;     highlight-max-region-no-warning: defvar -> defcustom.
;;     highlight-regexp-region: Use MOUSEP when call highlight-region.
;; 2006/03/31 dadams
;;     No longer use display-in-minibuffer.
;; 2005/12/18 dadams
;;     Use minibuffer-prompt face.  Removed require of def-face-const.el.
;;     highlight-single-quotations: defsubst -> defun.
;; 2004/10/13 dadams
;;     Updated for Emacs 21: highlight-region: Bind inhibit-modification-hooks to non-nil to prevent
;;       Emacs 21 font-lock from refontifying (removing highlighting)
;; 2004/10/12 dadams
;;     highlight-region: Use font-lock-after-fontify-buffer instead of lazy-lock-after-fontify-buffer.
;; 2004/03/16 dadams
;;     highlight-region: Prevent put-text-property from removing highlighting
;; 1996/04/26  dadams
;;     Put escaped newlines on long-line strings.
;; 1996/04/25  dadams
;;     1. Added highlight-single-quotations.
;;     2. highlight-regexp, highlight-regexp-region: Added new optional arg NTH.
;; 1996/04/25  dadams
;;     Added mouse-face-following-lines.
;; 1996/04/04  dadams
;;     1. highlight: Removed RAW-PREFIX, DISPLAY-MSGS args.  Made PREFIX optional.
;;        Set current-prefix-arg to nil so called fns don't use it as MOUSEP.
;;     2. highlight-regexp, highlight-regexp-region: Added MOUSEP arg.
;; 1996/02/27  dadams
;;     Added mouse-face-each-line.
;; 1996/02/26  dadams
;;     unhighlight-region: Added new arg MOUSEP.
;; 1996/02/12  dadams
;;     highlight-region: Added optional arg MOUSEP.
;; 1996/02/06  dadams
;;     Put variable-interactive property on appropriate user option vars.
;; 1996/02/01  dadams
;;     highlight: Just call subfunctions interactively.
;;     highlight-region, highlight-regexp, highlight-regexp-region: Use read-face-name.
;; 1996/01/08  dadams
;;     highlight-regexp, highlight-regexp-region: message -> display-in-minibuffer.
;; 1995/11/09  dadams
;;     highlight-region: FACE arg is optional.
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

(eval-when-compile (unless (fboundp 'dolist) (require 'cl))) ;; dolist
(require 'facemenu) ;; facemenu-add-face, facemenu-add-new-face, facemenu-menu
(require 'easymenu) ;; easy-menu-add-item
(require 'frame-fns nil t) ;; (no error if not found): flash-ding
(require 'menu-bar+ nil t) ;; (no error if not found): menu-bar-edit-region-menu
(when (> emacs-major-version 21) (require 'font-lock+ nil t)) ;; (no error if not found)
(require 'facemenu+ nil t) ;; (no error if not found)

;; Quiet the byte-compiler for Emacs 20
(defvar facemenu-mouse-menu)            ; In `facemenu+.el'
(defvar hi-lock-mode)
(defvar hlt-act-on-any-face-flag)
(defvar icicle-mode)                    ; In `icicles-mode.el'
(defvar multi-isearch-buffer-list)      ; In `misearch.el'
(defvar search-map)                     ; In `bindings.el', Emacs 23+

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 
;;(@* "Macros")

;;; Macros -----------------------------------------------------------

(defmacro hlt-user-error (&rest args)
  "`user-error' if defined, otherwise `error'."
  `(if (fboundp 'user-error) (user-error ,@args) (error ,@args)))
 
;;(@* "Key Bindings")

;;; Key Bindings -----------------------------------------------------

(defvar hlt-map nil "Keymap containing bindings for highlighting commands.")

(define-prefix-command 'hlt-map)
(define-key ctl-x-map "X" hlt-map)      ; `C-x X' prefix key

(define-key hlt-map [(down-mouse-2)]            'hlt-highlighter)
(define-key hlt-map [(S-down-mouse-2)]          'hlt-eraser)
(define-key hlt-map [(C-S-down-mouse-2)]        'hlt-copy-props)
(define-key hlt-map [(C-S-mouse-2)]             'ignore)
(define-key hlt-map [(C-down-mouse-2)]          'hlt-yank-props)
(define-key hlt-map [(C-mouse-2)]               'ignore)
(define-key hlt-map [(down-mouse-1)]            'hlt-mouse-copy-props)
(define-key hlt-map "\C-\M-s"                   'hlt-highlight-enclosing-list)
(define-key hlt-map "\M-w"                      'hlt-copy-props)
(define-key hlt-map "\C-y"                      'hlt-yank-props)
(define-key hlt-map "c"                         'hlt-choose-default-face)
(define-key hlt-map "n"                         'hlt-next-face)
(define-key hlt-map "p"                         'hlt-previous-face)
(define-key hlt-map "r"                         'hlt-replace-highlight-face)

(define-key hlt-map "h"                         nil) ; Prefix key
(define-key hlt-map "hh"                        'hlt-highlight)
(define-key hlt-map "h "                        'hlt-highlight-region)
(define-key hlt-map "hs"                        'hlt-highlight-symbol)
(define-key hlt-map "hx"                        'hlt-highlight-regexp-region)
(define-key hlt-map "he"                        'hlt-highlight-regexp-to-end)
(define-key hlt-map "u"                         nil) ; Prefix key
(define-key hlt-map "ur"                        'hlt-unhighlight-region)
(define-key hlt-map "us"                        'hlt-unhighlight-symbol)
(define-key hlt-map "ux"                        'hlt-unhighlight-regexp-region)
(define-key hlt-map "ue"                        'hlt-unhighlight-regexp-to-end)
(define-key hlt-map "uf"                        'hlt-unhighlight-region-for-face)

(when (boundp 'search-map)              ; Emacs 23+
  (define-key search-map "\C-\M-s"              'hlt-highlight-enclosing-list)
  (define-key search-map "hh"                   'hlt-highlight)
  (define-key search-map "h "                   'hlt-highlight-region)
  (define-key search-map "hs"                   'hlt-highlight-symbol)
  (define-key search-map "hx"                   'hlt-highlight-regexp-region)
  (define-key search-map "he"                   'hlt-highlight-regexp-to-end)
  (define-key search-map "ur"                   'hlt-unhighlight-region)
  (define-key search-map "us"                   'hlt-unhighlight-symbol)
  (define-key search-map "ux"                   'hlt-unhighlight-regexp-region)
  (define-key search-map "ue"                   'hlt-unhighlight-regexp-to-end)
  (define-key search-map "uf"                   'hlt-unhighlight-region-for-face)
  (define-key search-map [(shift control ?p)]   'hlt-previous-highlight)
  (define-key search-map [(shift control ?n)]   'hlt-next-highlight)
  (define-key search-map "hv"                   'hlt-highlight-property-with-value)
  (define-key search-map "hp"                   'hlt-highlight-all-prop)
  (define-key search-map "up"                   'hlt-unhighlight-all-prop))

(when (> emacs-major-version 20)        ; Emacs 21+
  (define-key hlt-map "-"                       'hlt-hide-default-face)
  (define-key hlt-map "+"                       'hlt-show-default-face)
  (define-key hlt-map "t"                       nil) ; Prefix key
  (define-key hlt-map "to"                      'hlt-toggle-use-overlays-flag)
  (define-key hlt-map "ta"                      'hlt-toggle-act-on-any-face-flag)
  (define-key hlt-map "tp"                      'hlt-toggle-property-highlighting)
  (define-key hlt-map "tl"                      'hlt-toggle-link-highlighting)
  (define-key hlt-map [(shift control ?p)]      'hlt-previous-highlight)
  (define-key hlt-map [(shift control ?n)]      'hlt-next-highlight)
  (define-key hlt-map "hv"                      'hlt-highlight-property-with-value)
  (define-key hlt-map "hp"                      'hlt-highlight-all-prop)
  (define-key hlt-map "up"                      'hlt-unhighlight-all-prop))
 
;;(@* "Menus")

;;; Menu-Bar `Edit' Menu ---------------------------------------------

(defun hlt-nonempty-region-p ()
  "Return non-nil if region is active and non-empty."
  (and transient-mark-mode  mark-active  (> (region-end) (region-beginning))))

(define-key-after menu-bar-edit-menu [hlt-copy-props]
  '(menu-item "Copy Text Properties" hlt-copy-props
    :help "Copy text properties at point, for subsequent pasting")
  'paste)
(define-key-after menu-bar-edit-menu [hlt-yank-props]
    '(menu-item "Paste Text Properties to Region" hlt-yank-props
      :help "Paste previously copied text properties to text in region"
      :enable (and (hlt-nonempty-region-p)  (not buffer-read-only)  hlt-copied-props))
    'hlt-copy-props)

;;; Menu-Bar `Edit' > `Region' Menu ----------------------------------

(when (boundp 'menu-bar-edit-region-menu) ; Defined in `menu-bar+.el'.
  (define-key menu-bar-edit-region-menu [separator-highlight] '("--"))
  (define-key menu-bar-edit-region-menu [hlt-yank-props]
    '(menu-item "Paste Text Properties" hlt-yank-props
      :help "Paste previously copied text properties to text in region"
      :enable hlt-copied-props))
  (define-key menu-bar-edit-region-menu [hlt-unhighlight-region-for-face]
    '(menu-item "Unhighlight for Face..." hlt-unhighlight-region-for-face
      :help "Remove highlighting for a given face in the region"))
  (define-key menu-bar-edit-region-menu [hlt-unhighlight-region]
    '(menu-item "Unhighlight" hlt-unhighlight-region
      :help "Remove highlighting (faces) in the region"))
  (define-key menu-bar-edit-region-menu [hlt-highlight-regexp-region]
    '(menu-item "Highlight Regexp..." hlt-highlight-regexp-region
      :help "Highlight parts of region that match a regexp"))
  (define-key menu-bar-edit-region-menu [hlt-highlight-region]
    '(menu-item "Highlight" hlt-highlight-region
      :help "Highlight all text in the region")))

;;; Facemenu `Text Properties' Menu ----------------------------------

(when (boundp 'facemenu-mouse-menu)     ; In `facemenu+.el'.
  (easy-menu-add-item facemenu-mouse-menu ()
                      ["Paste Text Properties to Region"
                       hlt-yank-props
                       (and (hlt-nonempty-region-p)  (not buffer-read-only)  hlt-copied-props)]
                      'dp)
  (easy-menu-add-item facemenu-mouse-menu () ["Copy Text Properties" hlt-copy-props t] 'dp))

(easy-menu-add-item facemenu-menu ()
                    ["Paste Text Properties to Region"
                     hlt-yank-props
                     (and (hlt-nonempty-region-p)  (not buffer-read-only)  hlt-copied-props)]
                    'dp)
(easy-menu-add-item facemenu-menu () ["Copy Text Properties" hlt-copy-props t] 'dp)
 
;;(@* "Variables and Faces")

;;; Variables and Faces ----------------------------------------------

(defgroup highlight nil
  "Highlighting."
  :prefix "hlt-" :group 'editing :group 'convenience :group 'wp :group 'faces
  :link `(url-link :tag "Send Bug Report"
          ,(concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=\
highlight.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and library versions."))
  :link '(url-link :tag "Download" "https://www.emacswiki.org/emacs/download/highlight.el")
  :link '(url-link :tag "Description" "https://www.emacswiki.org/emacs/HighLight")
  :link '(emacs-commentary-link :tag "Commentary" "highlight"))

(defface hlt-regexp-level-1 '((((background dark)) (:background "#071F473A0000")) ; a dark green
                              (t (:background "#FA6CC847FFFF"))) ; a light magenta
  "*Face used to highlight subgroup level 1 of a regexp match."
  :group 'highlight :group 'faces)

(defface hlt-regexp-level-2 '((((background dark)) (:background "#507400002839")) ; a dark red
                              (t (:background "#C847FFFFE423"))) ; a light cyan
  "*Face used to highlight subgroup level 2 of a regexp match."
  :group 'highlight :group 'faces)

(defface hlt-regexp-level-3 '((((background dark)) (:background "#4517305D0000")) ; a dark brown
                              (t (:background "#C847D8FEFFFF"))) ; a light blue
  "*Face used to highlight subgroup level 3 of a regexp match."
  :group 'highlight :group 'faces)

(defface hlt-regexp-level-4 '((((background dark)) (:background "#176900004E0A")) ; a dark blue
                              (t (:background "#EF47FFFFC847"))) ; a light yellow
  "*Face used to highlight subgroup level 4 of a regexp match."
  :group 'highlight :group 'faces)

(defface hlt-regexp-level-5 '((((background dark)) (:background "#04602BC00000")) ; a very dark green
                              (t (:background "#FCFCE1E1FFFF"))) ; a light magenta
  "*Face used to highlight subgroup level 5 of a regexp match."
  :group 'highlight :group 'faces)

(defface hlt-regexp-level-6 '((((background dark)) (:background "#32F200001979")) ; a very dark red
                              (t (:background "#E1E1FFFFF0F0"))) ; a light cyan
  "*Face used to highlight subgroup level 6 of a regexp match."
  :group 'highlight :group 'faces)

(defface hlt-regexp-level-7 '((((background dark)) (:background "#316B22970000")) ; a very dark brown
                              (t (:background "#E1E1EAEAFFFF"))) ; a light blue
  "*Face used to highlight subgroup level 7 of a regexp match."
  :group 'highlight :group 'faces)

(defface hlt-regexp-level-8 '((((background dark)) (:background "#12EC00003F0E")) ; a very dark blue
                              (t (:background "#F6F5FFFFE1E1"))) ; a light yellow
  "*Face used to highlight subgroup level 8 of a regexp match."
  :group 'highlight :group 'faces)

;; This is defined in `faces.el', Emacs 22.  This definition is adapted to Emacs 20.
(unless (facep 'minibuffer-prompt)
  (defface minibuffer-prompt '((((background dark)) (:foreground "cyan"))
                               (t (:foreground "dark blue")))
    "*Face for minibuffer prompts."
    :group 'basic-faces))

;; Same as `icicle-remove-if-not' etc.
(defun hlt-remove-if-not (pred xs)
  "A copy of list XS with only elements that satisfy predicate PRED."
  (let ((result  ()))
    (dolist (x xs) (when (funcall pred x) (push x result)))
    (nreverse result)))

(defun hlt-tty-colors ()
  "Colors available for use with Emacs in a terminal (`emacs -nw')."
  (hlt-remove-if-not #'x-color-defined-p
                     (if (fboundp 'tty-color-alist)
                         (mapcar #'car (tty-color-alist))
                       '("blue" "green" "cyan" "red" "magenta" "brown" "lightgray" "darkgray" "yellow"
                         "white" "lightblue" "lightgreen" "lightcyan" "lightred" "lightmagenta"))))

(when (fboundp 'next-single-char-property-change) ; Don't bother, for Emacs 20.

  (defface hlt-property-highlight '((((background dark)) (:background "Navy"))
                                    (t (:background "Wheat")))
    "*Face used to highlight all links."
    :group 'highlight :group 'faces)
  (defcustom hlt-act-on-any-face-flag nil
    "*Non-nil means highlight actions apply to all text with a face.
nil means they apply only to text that has been highlighted by library
`highlight.el'.

Consult the doc for particular actions, to see if they are affected by
this option."
    :type 'boolean :group 'highlight)

  (defvar hlt-prop-highlighting-state '(nil . nil)
    "Cons representing the state of property highlighting.
The car indicates whether property highlighting is on (nil means off).
The cdr is the position of the last mouse click that changed state, as
a marker."))

(defcustom hlt-auto-face-backgrounds
  (let ((tty-cols   (hlt-tty-colors))
        (tty-faces  (hlt-remove-if-not #'facep '(highlight isearch isearch-fail lazy-highlight
                                                 mode-line mode-line-inactive next-error
                                                 nobreak-space secondary-selection tooltip
                                                 trailing-whitespace))))
    (if (if (fboundp 'display-graphic-p) (display-graphic-p) window-system)
        (append tty-cols  '("DeepPink" "MediumPurple1" "SpringGreen1" "DarkOrange" "HotPink1")
                tty-faces (hlt-remove-if-not #'facep '(header-line mode-line-highlight)))
      (append tty-cols tty-faces)))
  "*Colors or faces rotated among for the next highlighting face.
A face specifies the face to use.

A color name or RGB hex string specifies only the background color to
use.  The foreground color is then determined by option
`hlt-auto-face-foreground'.

This option has no effect if option `hlt-auto-faces-flag' is nil."
  :type '(repeat (choice
                  (color :tag "Background color" :value "yellow")
                  (face  :tag "Face" :value "highlight")))
  :group 'highlight)

;;;###autoload
(defcustom hlt-auto-face-foreground nil
  "*Foreground color for pseudo faces created from a chosen background.
The value is either a color (name or #RGB hex triplet) or nil.  A nil
value means that highlighting does not change the existing foreground
color."
  :type '(choice
          color
          (const :tag "Unspecified: respect current foreground" nil))
  :group 'highlight)

;;;###autoload
(defcustom hlt-auto-faces-flag nil
    "*Non-nil means highlighting can automatically choose faces.
Highlighting action can use the next background color or face in
`hlt-auto-face-backgrounds'.  When a given item in the list is a color
name, not a face, `hlt-auto-face-foreground' is used as the
corresponding foreground.

This option has no effect on unhighlighting."
    :type 'boolean :group 'highlight)

;;;###autoload
(defcustom hlt-default-copy/yank-props '(face)
  "*Properties that `hlt-copy-props' and `hlt-yank-props' use by default.
You can use a prefix argument with those commands to override the
default behavior.
Either a list of properties (symbols) or `t', meaning all properties."
  :type '(choice
          (const :tag "All properties" t)
          (repeat (symbol :tag "Property")))
  :group 'highlight)

;;;###autoload
(defcustom hlt-face-prop (if (< emacs-major-version 22) 'face 'font-lock-face)
  "*Face property used for highlighting: `face' or `font-lock-face'.
If the value is `font-lock-face' then highlighting by library
`highlight.el' disappears when `font-lock-mode' is turned off.

If the value is `face', and if library`font-lock+.el' has been loaded,
then highlighting persists - it is independent of font-locking.

For Emacs 20 or 21, font-locking does not recognize property
`font-lock-face', so just use `face' (the default value for these
versions), not `font-lock-face' - do not change the value."
  :type '(choice
          (const :tag "`face' - highlighting is independent of font-lock"        face)
          (const :tag "`font-lock-face' - highlighting is governed by font-lock" font-lock-face))
  :group 'highlight)

;;;###autoload
(defcustom hlt-line-dups-ignore-regexp "[ \t]*"
  "Regexp to ignore leading and trailing text for duplicate lines.
Or nil if no such text is to be ignored.
Used by `hlt-highlight-line-dups-region' to determine whether two
lines are duplicates."
  :type '(choice
          (string :tag "Regexp")
          (const  :tag "Do not ignore leading or trailing text" nil))
  :group 'highlight)

;;;###autoload
(defcustom hlt-max-region-no-warning 100000
  "*Maximum size (chars) of region to highlight without confirmation.
This is used only for highlighting of a regexp, which can be slow."
  :type 'integer :group 'highlight)

;;;###autoload
(defcustom hlt-overlays-priority 0
  "*Priority of the overlays created by `hlt-*' functions."
  :type 'integer :group 'highlight)

;;;###autoload
(defcustom hlt-use-overlays-flag 'only
  "*Non-nil means use overlays to highlight; nil means use text properties.
This value also affects some actions, such as unhighlighting, for text
that is highlighted.  If the value is `only' (the default value), then
those actions only affect overlay highlighting.  Otherwise, they
affect both kinds of highlighting."
  :type '(choice
          (const :tag "Highlight using text properties, not overlays" nil)
          (const :tag "Highlight using overlays, not text properties" only)
          (sexp  :tag
           "Highlight using overlays, but act also on highlight text properties" t))
  :group 'highlight)

(defvar hlt-face-nb 0 
  "Current index into `hlt-auto-face-backgrounds'.
This variable is always buffer-local.")
(make-variable-buffer-local 'hlt-face-nb)

(defvar hlt-last-face 'highlight
  "The last face used by highlight commands.
The value can also be an alist with two entries:
\(`background-color' . COLOR) and
\(`foreground-color' . hlt-auto-face-foreground).")

(defvar hlt-last-regexp nil "The last regexp highlighted.")

(defvar hlt-previous-use-overlays-flag-value nil "Previous value of `hlt-use-overlays-flag'.")

(defvar hlt-copied-props ()
  "Plist of text properties last copied using `hlt-copy-props'.")
 
;;(@* "Misc Functions - Emacs 20+")

;;; Misc Functions - Emacs 20+ ---------------------------------------

(defun hlt-read-bg/face-name (prompt &optional default)
  "Read a face name or color name using completion.
A color name can also be a hex RGB triplet prefixed by `#'.
To allow this, completion is lax.

If you use Icicles and option `icicle-WYSIWYG-Completions-flag' is
non-nil then in Icicle mode candidate faces and colors are WYSIWYG:
What You See Is What You Get.

Prompt with PROMPT.
Optional arg DEFAULT is a face name used if the user enters nothing.

Return the corresponding face.  But if a color name is chosen then
return a face spec composed of the color name as `background-color'
and the value of `hlt-auto-face-foreground' as `foreground-color'."
  (save-match-data (when (string-match "\\(:\\s *$\\|:?\\s +$\\)" prompt)
                     (setq prompt  (substring prompt 0 (- (length (match-string 0 prompt)))))))
  (unless default (setq default  (if (facep hlt-last-face)
                                     (symbol-name hlt-last-face)
                                   (and (consp hlt-last-face)
                                        (cdr (assq 'background-color hlt-last-face))))))
  (unless (stringp default) (setq default  (format "%s" default)))
  (let* ((icicle-unpropertize-completion-result-flag  t)
         (icicle-multi-completing-p                   t)
         (icicle-list-nth-parts-join-string           ": ")
         (icicle-list-join-string                     ": ")
         (icicle-list-use-nth-parts                   '(1))
         (prompt                                      (if default
                                                          (format "%s (default `%s'): " prompt default)
                                                        (format "%s: " prompt)))
         (faces                                       (if (and (boundp 'icicle-mode)  icicle-mode)
                                                          (mapcar #'icicle-make-face-candidate (face-list))
                                                        ()))
         (completion-annotate-function                (lambda (fc)
                                                        (when (and (boundp 'icicle-mode)  icicle-mode)
                                                          (setq fc  (icicle-transform-multi-completion fc)))
                                                        (if (facep (intern fc)) "  Face" "  Color")))
         (colors                                      (if window-system
                                                          (if (fboundp 'hexrgb-defined-colors)
                                                              (hexrgb-defined-colors)
                                                            (x-defined-colors))
                                                        (hlt-tty-colors)))
         (colors                                      (if (and (boundp 'icicle-mode)  icicle-mode)
                                                          (mapcar #'icicle-make-color-candidate colors)
                                                        (mapcar #'list colors))))
    (if (not (and (boundp 'icicle-mode)  icicle-mode))
        (mapatoms (lambda (sy) (when (facep sy) (push (list (symbol-name sy)) faces))))
      (setq prompt  (copy-sequence prompt)) ; So we can modify it by adding property.
      (put-text-property 0 1 'icicle-fancy-candidates t prompt))
    (let ((bg/face  (completing-read prompt (append faces colors) nil nil ; Lax, to allow #RGB
                                     nil 'face-name-history default)))
      (when (and (boundp 'icicle-mode)  icicle-mode)
        (setq bg/face  (icicle-transform-multi-completion bg/face)))
      (if (facep (intern bg/face))
          (intern bg/face)
        `((background-color . ,bg/face) (foreground-color . ,hlt-auto-face-foreground))))))

;;;###autoload
(defun hlt-choose-default-face (face)
  "Choose a face for highlighting.
Set `hlt-last-face' to the face, and return it.

You can choose a face name or a color name.  If a color is chosen, it
is used for the face background.  The face foreground is determined by
the value of `hlt-auto-face-foreground'."
  (interactive
   (list (hlt-read-bg/face-name "Choose background color or face: "
                                (and (symbolp hlt-last-face)  (symbol-name hlt-last-face)))))
  (setq hlt-last-face face)
  (when (interactive-p) (message "Highlighting will now use face `%S'" face))
  face)

;;;###autoload
(defun hlt-next-face (&optional face-nb msgp)
  "Choose the next face for highlighting and unhighlighting.
Use `hlt-auto-face-backgrounds' and `hlt-auto-face-foreground'.

A non-negative numeric prefix arg N means use the Nth entry of
`hlt-auto-face-backgrounds'.  Counting is 0-based.

A negative numeric prefix arg N means count from the end, not the
beginning, of `hlt-auto-face-backgrounds': -1 means the last entry, -2
means the next-to-last, etc. (counting from the end is 1-based, not
0-based).

From LISP, if the first argument is `previous' then choose the
previous face, not the next one."
  (interactive (list (and current-prefix-arg  (prefix-numeric-value current-prefix-arg))
                     t))
  (let* ((len      (length hlt-auto-face-backgrounds))
         (nb       -1)
         (nb       (and (not (integerp face-nb))
                        (catch 'hlt-next-face
                          (let ((last-bg/f  (if (facep hlt-last-face)
                                                hlt-last-face
                                              (cdr (assq 'background-color hlt-last-face)))))
                            (dolist (bg/f  hlt-auto-face-backgrounds)
                              (setq nb  (1+ nb))
                              (when (equal bg/f last-bg/f) (throw 'hlt-next-face nb))))
                          nil))))
    (setq hlt-face-nb    (mod (if nb
                                  (if (eq face-nb t) (1- nb) (1+ nb))
                                (or (and (integerp face-nb)  face-nb)  1))
                              len)
          hlt-last-face  (let ((bg/f  (nth hlt-face-nb hlt-auto-face-backgrounds)))
                           (if (facep bg/f)
                               bg/f 
                             `((background-color . ,bg/f)
                               (foreground-color . ,hlt-auto-face-foreground)))))
    (when msgp (message "Highlighting will now use face `%s'" hlt-last-face))
    hlt-last-face))

;;;###autoload
(defun hlt-previous-face (&optional face-nb msgp)
  "Like `hlt-next-face', but previous, not next.
Use of a numeric prefix arg is the same as for `hlt-next-face'."
  (interactive (list (and current-prefix-arg  (prefix-numeric-value current-prefix-arg))
                     t))
  (hlt-next-face (if face-nb  (- face-nb) t) msgp))

;;;###autoload
(defun hlt-highlighter (start-event &optional face-nb) ; Suggested binding: `C-x mouse-2'.
  "Highlight the text you drag the mouse over.
The face used is the last face that was used for highlighting.
You can use command `hlt-choose-default-face' to choose the default
face to use.

If `hlt-auto-faces-flag' is non-nil then this command cycles to the
next color/face.  This is the case even if you do not drag the
mouse (empty highlight).  A message tells you what the face is.

A numeric prefix arg N means use the face represented by the Nth entry
of `hlt-auto-face-backgrounds' (uses `hlt-next-face')."
  (interactive "e\np")
  (when (or hlt-auto-faces-flag  current-prefix-arg)
    (hlt-next-face (and current-prefix-arg  face-nb)))
  (save-excursion
    (run-hooks 'mouse-leave-buffer-hook) ; Let temporary modes like isearch turn off.
    (let* ((original-window  (selected-window))
           (echo-keystrokes  0)
           (start-posn       (event-start start-event))
           (start-point      (posn-point start-posn))
           (end-point        start-point)
           (start-window     (posn-window start-posn)))
      (with-current-buffer (window-buffer start-window)
        (let ((read-only                          buffer-read-only)
              (modified-p                         (buffer-modified-p))
              (inhibit-modification-hooks         t)
              (overlay                            (and hlt-use-overlays-flag
                                                       (make-overlay start-point start-point)))
              ;; Otherwise, `put-text-property' calls this, which would remove highlight.
              (font-lock-fontify-region-function  'ignore)
              event)
          (setq buffer-read-only  nil)
          (track-mouse
            (while (progn (setq event  (read-event))
                          (or (mouse-movement-p event)
                              (memq (car-safe event) '(switch-frame select-window))))
              (unless (memq (car-safe event) '(switch-frame select-window))
                (setq end-point  (posn-point (event-end event))))
              (unless (integer-or-marker-p end-point) (hlt-user-error "Mouse dragged out of window"))
              (cond (hlt-use-overlays-flag
                     (setq overlay  (move-overlay overlay start-point end-point))
                     (overlay-put overlay hlt-face-prop  hlt-last-face)
                     (overlay-put overlay 'hlt-highlight hlt-last-face)
                     (overlay-put overlay 'priority      hlt-overlays-priority))
                    (t
                     (put-text-property start-point end-point hlt-face-prop  hlt-last-face)
                     (put-text-property start-point end-point 'hlt-highlight hlt-last-face)
                     (when (eq 'face hlt-face-prop)
                       (put-text-property start-point end-point 'font-lock-ignore t))))))
          (setq buffer-read-only  read-only)
          (set-buffer-modified-p modified-p)))))
  (message "Highlighted with face `%s'"hlt-last-face))

;;;###autoload
(defun hlt-eraser (start-event &optional face-nb) ; Suggested binding: `C-x S-mouse-2'.
  "Erase highlights that you click or drag the mouse over.
If `hlt-use-overlays-flag' is non-nil, then remove overlay
highlighting for the last face that was used for highlighting.  (You
can use command `hlt-choose-default-face' first to choose a different
face.)

If `hlt-use-overlays-flag' is not `only', then remove text-property
highlighting for *ALL* faces (not just highlighting faces).  This
means, in particular, that a value of nil erases both overlays for the
last face and text properties for all faces.

With a numeric prefix arg N, if `hlt-use-overlays-flag' is `only',
erase the face represented by the Nth entry of
`hlt-auto-face-backgrounds' (uses `hlt-next-face')."
  (interactive "e\np")
  (when (and (eq 'only hlt-use-overlays-flag)
             (or hlt-auto-faces-flag  current-prefix-arg)
             face-nb)
    (hlt-next-face (and current-prefix-arg  face-nb)))
  (save-excursion
    (run-hooks 'mouse-leave-buffer-hook) ; Let temporary modes like isearch turn off.
    (let* ((original-window  (selected-window))
           (echo-keystrokes  0)
           (start-posn       (event-start start-event))
           (start            (posn-point start-posn))
           (end              start)
           (start-window     (posn-window start-posn)))
      (with-current-buffer (window-buffer start-window)
        (let ((read-only                          buffer-read-only)
              (modified-p                         (buffer-modified-p))
              (inhibit-modification-hooks         t)
              ;; Otherwise, `put-text-property' calls this, which removes highlight.
              (font-lock-fontify-region-function  'ignore)
              event)
          (setq buffer-read-only  nil)
          (track-mouse
            (while (progn (setq event  (read-event))
                          (or (mouse-movement-p event)
                              (memq (car-safe event) '(switch-frame select-window))))
              (unless (memq (car-safe event) '(switch-frame select-window))
                (let ((posn-point  (posn-point (event-end event))))
                  (unless (integer-or-marker-p posn-point) (hlt-user-error "Mouse dragged out of window"))
                  (setq end    (max end posn-point)
                        start  (min start posn-point))))
              (when hlt-use-overlays-flag ; Erase overlay properties
                (dolist (ov  (overlays-in start end))
                  (hlt-unhighlight-for-overlay ov start end hlt-last-face)))
              (unless (eq 'only hlt-use-overlays-flag) ; Erase text properties
                (remove-text-properties start end '(face nil font-lock-face nil
                                                         hlt-highlight nil font-lock-ignore nil)))))
          (setq buffer-read-only  read-only)
          (set-buffer-modified-p modified-p)))))
  (message "Removed highlighting for face `%s'" hlt-last-face))

;;; (defun hlt-unhighlight-for-overlay (overlay start end &optional face)
;;;   "Remove OVERLAY highlighting from START to END.
;;; Acts only on an OVERLAY that was created by library `highlight'.
;;; If OVERLAY extends beyond the region from START to END, then replace
;;; it with two overlays: one that ends at START and the other that starts
;;; at END.  Otherwise, delete OVERLAY.
;;; Optional arg FACE is a face symbol.  If non-nil, then delete only
;;; overlays with that FACE."
;;;   (let ((oface   (overlay-get overlay 'hlt-highlight))
;;;         (ostart  (overlay-start overlay))
;;;         (oend    (overlay-end   overlay)))
;;;     (when (and oface  (or (not face)  (equal face oface)))
;;;       (delete-overlay overlay)
;;;       (when (< ostart start) (hlt-highlight-region ostart start face))
;;;       (when (> oend end) (hlt-highlight-region end oend face)))))

;; This version has an implementation similar to that of `remove-overlays' in Emacs 22+.
(defun hlt-unhighlight-for-overlay (overlay start end &optional face mousep)
  "Remove highlighting from OVERLAY from START to END.
Acts only on an OVERLAY that was created by library `highlight'.
OVERLAY might be moved or split or both.

Optional arg FACE is a face symbol.  If non-nil, then remove only
`face' property (or `mouse-face' property, if optional arg MOUSEP is
non-nil) highlighting for FACE.

If FACE is nil then remove all `face' property highlighting (or all
`mouse-face' property highlighting, if MOUSEP)."
  ;; (overlay-recenter end)                ; Speed up loops over overlays.
  (when (< end start) (setq start (prog1 end (setq end start))))
  (let ((hlt-face   (overlay-get overlay 'hlt-highlight))
        (ostart     (overlay-start overlay))
        (oend       (overlay-end   overlay))
        (oface      (overlay-get overlay hlt-face-prop))
        (omface     (and mousep  (overlay-get overlay 'mouse-face))))
    (when (and hlt-face  (if face
                             (and (equal face hlt-face)
                                  (if mousep (equal face omface) (equal face oface)))
                           (if mousep omface oface)))
      ;; Either push OVERLAY outside region or split it to exclude region
      ;; or delete it (if it is entirely contained in region).
      (if (< ostart start)
          (if (<= oend end)
              (move-overlay overlay ostart start)
            (move-overlay (copy-overlay overlay) ostart start)
            (move-overlay overlay end oend))
        (if (> oend end)
            (move-overlay overlay end oend)
          (delete-overlay overlay))))))

;;;###autoload
(defun hlt-highlighter-mouse ()
  "Same as `hlt-highlighter', but for binding to a menu item."
  (interactive)
  (message "Drag mouse to highlight text") (sleep-for 1)
  (hlt-highlighter (if (> emacs-major-version 21)
                       (read-event)
                     (read-event)       ; Emacs 20-21: Skip event of choosing menu item.
                     (read-event))))

;;;###autoload
(defun hlt-eraser-mouse ()
  "Same as `hlt-eraser', but for binding to a menu item."
  (interactive)
  (message "Drag mouse over text to remove its highlighting") (sleep-for 1)
  (hlt-eraser (if (> emacs-major-version 21)
                  (read-event)
                (read-event)            ; Emacs 20-21: Skip event of choosing menu item.
                (read-event))))

;;;###autoload
(defun hlt-highlight (&optional prefix) ; Suggested binding: `C-x C-y'.
  "Highlight or unhighlight.
If the region is not active or it is empty, then use the whole buffer.
The face used is the last face that was used for highlighting.
You can use command `hlt-choose-default-face' to choose a different face.

This is several commands rolled into one, depending on the prefix arg:

* No prefix arg: highlight all text in region/buffer
* Plain prefix arg (`C-u') or zero prefix arg (`C-0'): UNhighlight all
* Positive prefix arg (`C-1'): highlight regexp matches
* Negative prefix arg (`C--'): UNhighlight regexp matches

You can also use the individual commands:

* `hlt-highlight-region'          - same as no prefix arg
* `hlt-unhighlight-region'        - same as `C-u' or `C-0'
* `hlt-highlight-regexp-region'   - same as `C-1'
* `hlt-unhighlight-regexp-region' - same as `C--'"
  (interactive "P")
  (setq current-prefix-arg  nil)        ; No MOUSEP for calls to individual cmds.
  (cond ((not prefix) (call-interactively 'hlt-highlight-region))
        ((or (consp prefix)  (zerop (prefix-numeric-value prefix)))
         (save-excursion (call-interactively 'hlt-unhighlight-region)))
        ((> (prefix-numeric-value prefix) 0)
         (call-interactively 'hlt-highlight-regexp-region))
        ((< (prefix-numeric-value prefix) 0)
         (save-excursion (call-interactively 'hlt-unhighlight-regexp-region)))))

;;;###autoload
(defun hlt-highlight-region-in-buffers (buffers &optional msgp)
  "Use `hlt-highlight-region' in each buffer of list BUFFERS.
A prefix arg >= 0 means highlight with `mouse-face', not `face'.
A prefix arg <= 0 means highlight all visible or iconified buffers.
Otherwise, you are prompted for the BUFFERS to highlight, one at a
 time.  Use `C-g' to end prompting.
If you specify no BUFFERS then the current buffer is highlighted.

Non-nil optional arg MSGP means show status messages."
  (interactive (list (if (and current-prefix-arg  (<= (prefix-numeric-value current-prefix-arg) 0))
                         (hlt-remove-if-not (lambda (bf) (get-buffer-window bf 0)) (buffer-list))
                       (hlt-+/--read-bufs))
                     t))
  (hlt-highlight-region
   nil nil nil msgp (and current-prefix-arg  (>= (prefix-numeric-value current-prefix-arg) 0)) buffers))

;;;###autoload
(defun hlt-highlight-region (&optional start end face msgp mousep buffers)
  "Highlight either the region/buffer or new input that you type.
Use the region if active, or the buffer otherwise.

IF *ALL* of the following are true THEN apply the last-used face as a
text property to the next and subsequent characters that you type, and
add that face to a Facemenu menu (`Text Properties' or one of its
submenus):

 * You call this command interactively.
 * You use no prefix arg.
 * Option `hlt-use-overlays-flag' is nil
 * The last property used for highlighting was `face'.

OTHERWISE, the behavior respects `hlt-use-overlays-flag' and depends
on the optional arguments, as follows:

 START and END are the limits of the area to act on.
  Interactively, if the region is active and nonempty then use the
    region limits.  Otherwise, use the buffer limits.
  Noninteractively, ignore START and END unless both are non-nil and
    BUFFERS is either nil (meaning use the current buffer) or a
    singleton list.  When START and END are ignored, the area to act
    on is determined automatically for each buffer in BUFFERS, based
    on whether the region is active there.

 FACE is the face to use.
  Interactively, this is the last face that was used for highlighting.
  (You can use command `hlt-choose-default-face' to choose a different
  face.)

 MSGP non-nil displays a progress message.  Interactively it is t.

 MOUSEP non-nil means use `mouse-face', not `face'.
  Interactively it is provided by the prefix arg.

 BUFFERS is the list of buffers to highlight.
  Interactively it is nil: highlight only the current buffer."
  (interactive `(,@(hlt-region-or-buffer-limits) nil t ,current-prefix-arg))
  (when hlt-auto-faces-flag (hlt-next-face))
  (let ((mbufs  buffers))
    (unless buffers (setq buffers  (list (current-buffer))))
    (dolist (buf  buffers)
      (with-current-buffer buf

        ;; If two or more BUFFERS, or if START and END aren't both defined, then define them for this BUF.
        (when (or (cadr buffers)  (not start)  (not end))
          (let ((start-end  (hlt-region-or-buffer-limits buf)))
            (setq start  (car start-end)
                  end    (cadr start-end))))

        ;; If START or END is a marker then its buffer must be this BUF.  Else skip to next BUF.
        (when (and (or (not (markerp start))  (eq buf (marker-buffer start)))
                   (or (not (markerp end))    (eq buf (marker-buffer end))))
          (if (not face)
              (setq face  hlt-last-face)
            (unless (and (symbolp face)  (hlt-string-match-p "^hlt-regexp-level-" (symbol-name face)))
              (setq hlt-last-face  face)))
          (when msgp (message "Highlighting%s%s..."
                              (if mousep " `mouse-face'" "")
                              (if mbufs (format " in `%s'"  buf) "")))
          (let ((read-only                          buffer-read-only)
                (modified-p                         (buffer-modified-p))
                (inhibit-modification-hooks         t)
                ;; `put-text-property' would otherwise call this function, which would remove highlighting.
                (font-lock-fontify-region-function  'ignore)
                overlay)
            (setq buffer-read-only  nil)
            (cond (hlt-use-overlays-flag
                   (setq overlay  (make-overlay start end))
                   (overlay-put overlay (if mousep 'mouse-face hlt-face-prop) face)
                   (overlay-put overlay 'hlt-highlight                        face)
                   (overlay-put overlay 'priority                             hlt-overlays-priority))
                  (mousep
                   (put-text-property start end 'mouse-face    face)
                   (put-text-property start end 'hlt-highlight face))
                  ((interactive-p)
                   (message "Text you type now will have face `%s'" face)
                   (facemenu-add-new-face face)
                   ;; It is `facemenu-add-face' that either uses region or next insert.
                   (facemenu-add-face face
                                      (and (hlt-nonempty-region-p)  start)
                                      (and (hlt-nonempty-region-p)  end))
                   (when (hlt-nonempty-region-p)
                     (put-text-property start end 'hlt-highlight    face)
                     (when (eq 'face hlt-face-prop)
                       (put-text-property start end 'font-lock-ignore t))))
                  (t (put-text-property start end hlt-face-prop     face)
                     (put-text-property start end 'hlt-highlight    face)
                     (when (eq 'face hlt-face-prop) (put-text-property start end 'font-lock-ignore t))))
            (setq buffer-read-only  read-only)
            (set-buffer-modified-p modified-p))
          (when msgp (let ((remove-msg  "\\[hlt-unhighlight-region]' to remove highlighting"))
                       (when mousep (setq remove-msg  (concat "\\[universal-argument] " remove-msg)))
                       (setq remove-msg  (substitute-command-keys (concat "`" remove-msg)))
                       (message "Highlighting%s%s... done %s"
                                (if mousep " `mouse-face'" "")
                                (if mbufs (format " in `%s'"  (buffer-name buf)) "")
                                remove-msg))))))))
;;;###autoload
(defun hlt-unhighlight-region-in-buffers (buffers &optional msgp)
  "Use `hlt-unhighlight-region' in each buffer of list BUFFERS.
A prefix arg >= 0 means unhighlight `mouse-face', not `face'.
A prefix arg <= 0 means unhighlight all visible or iconified buffers.
Otherwise, you are prompted for the BUFFERS to unhighlight, one at a
 time.  Use `C-g' to end prompting.
If you specify no BUFFERS then the current buffer is unhighlighted.

Non-nil optional arg MSGP means show status messages."
  (interactive (list (if (and current-prefix-arg  (<= (prefix-numeric-value current-prefix-arg) 0))
                         (hlt-remove-if-not (lambda (bf) (get-buffer-window bf 0)) (buffer-list))
                       (hlt-+/--read-bufs 'UN))))
  (hlt-unhighlight-region nil nil nil msgp
                          (and current-prefix-arg  (>= (prefix-numeric-value current-prefix-arg) 0))
                          buffers))

;;;###autoload
(defun hlt-unhighlight-region (&optional start end face msgp mousep buffers)
  "Remove all highlighting in region or buffer.
Use the region if active, or the buffer otherwise.
The arguments are the same as for `hlt-highlight-region'.

If `hlt-use-overlays-flag' is non-nil, then remove overlay highlighting.
If `hlt-use-overlays-flag' is not `only', then remove text-property
 highlighting.  This means, in particular, that a value of nil removes
 both overlays and text properties."
  (interactive `(,@(hlt-region-or-buffer-limits) nil t ,current-prefix-arg))
  (let ((mbufs  buffers))
    (unless buffers (setq buffers  (list (current-buffer))))
    (dolist (buf  buffers)
      (with-current-buffer buf
        ;; Use START and END if provided non-interactively, but not otherwise.
        (unless (and start  end  (or (not (cadr buffers))  (not (interactive-p))))
          (let ((start-end  (hlt-region-or-buffer-limits buf)))
            (setq start  (car start-end)
                  end    (cadr start-end))))
        ;; Do nothing if START or END is a marker for a different buffer.
        (when (and (or (not (markerp start))  (eq buf (marker-buffer start)))
                   (or (not (markerp end))    (eq buf (marker-buffer end))))
          (when msgp (message "Removing%s highlighting%s..."
                              (if mousep " `mouse-face'" "")
                              (if mbufs (format " in `%s'"  buf) "")))
          (let ((read-only-p  buffer-read-only)
                (modified-p   (buffer-modified-p)))
            (setq buffer-read-only  nil)
            (when hlt-use-overlays-flag ; Unhighlight overlay properties.
              (dolist (ov  (overlays-in start end)) (hlt-unhighlight-for-overlay ov start end face mousep)))
            (unless (eq 'only hlt-use-overlays-flag) ; Unhighlight text properties.
              (let ((beg  start)
                    hi-face)
                (while (< beg end)
                  (when (setq hi-face  (get-text-property beg 'hlt-highlight))
                    (when (or (null face)  (equal hi-face face))
                      ;; $$$ Really, we should remove only the part of the `face' and `font-lock-face'
                      ;;     properties that belongs to Highlight, and set the value to be the same as
                      ;;     it is, but without `hlt-last-face'.
                      (remove-text-properties
                       beg (1+ beg) (if mousep
                                        '(mouse-face nil hlt-highlight nil font-lock-ignore nil)
                                      '(face nil font-lock-face nil
                                             hlt-highlight nil font-lock-ignore nil)))))
                  (setq beg  (1+ beg)))))
            (setq buffer-read-only  read-only-p)
            (set-buffer-modified-p modified-p))
          (when msgp (message "Removing%s highlighting%s... done"
                              (if mousep " `mouse-face'" "")
                              (if mbufs (format " in `%s'"  buf) ""))))))))

;;;###autoload
(defun hlt-highlight-regexp-region-in-buffers (regexp buffers &optional face msgp mousep nth)
  "Use `hlt-highlight-regexp-region' in each buffer of list BUFFERS.
A prefix arg >= 0 means highlight with `mouse-face', not `face'.
A prefix arg <= 0 means highlight all visible or iconified buffers.
Otherwise, you are prompted for the BUFFERS to highlight, one at a
 time.  Use `C-g' to end prompting.
If you specify no BUFFERS then the current buffer is highlighted.

See `hlt-highlight-regexp-region' for other arguments."
  (interactive (list (hlt-+/--read-regexp "" 'REGION)
                     (if (and current-prefix-arg  (<= (prefix-numeric-value current-prefix-arg) 0))
                         (hlt-remove-if-not (lambda (bf) (get-buffer-window bf 0)) (buffer-list))
                       (hlt-+/--read-bufs))
                     nil
                     t
                     current-prefix-arg
                     nil))
  (hlt-highlight-regexp-region nil nil regexp nil msgp current-prefix-arg nth buffers))

;;;###autoload
(defun hlt-highlight-regexp-region (&optional start end regexp face msgp mousep nth buffers)
  "Highlight regular expression REGEXP in region/buffer.
Use the region if active, or the buffer otherwise.

Optional args START and END are the limits of the area to act on.
  They default to the region limits.  (But see BUFFERS, below.)
Optional 4th arg FACE is the face to use.
  Interactively, this is the last face that was used for highlighting.
  (You can use command `hlt-choose-default-face' to choose a different face.)
Optional 5th arg MSGP:
  t means to treat this as an interactive call when deciding to
    display all messages.
  non-nil & non-t means to display only error and warning messages.
Optional 6th arg MOUSEP non-nil means to use `mouse-face' property,
  not `face'.  Interactively, this is provided by the prefix arg.
Optional 7th arg NTH determines which regexp subgroup is highlighted.
  If nil or 0, the entire regexp is highlighted.  Otherwise, the NTH
  regexp subgroup (\"\\\\(...\\\\)\" expression) is highlighted.
  (NTH is not available interactively.)
Optional 6th arg BUFFERS is the list of buffers to highlight.
  If non-nil then explicit START and END values are ignored, and the
  actual values are determined automatically for each buffer, based on
  whether the region is active there."
  (interactive (list nil nil (hlt-+/--read-regexp ""   'REGION) nil t current-prefix-arg))
  (hlt-+/--highlight-regexp-region nil start end regexp face msgp mousep nth buffers))

;;;###autoload
(defun hlt-unhighlight-regexp-region-in-buffers (regexp buffers &optional nth msgp)
  "Use `hlt-unhighlight-regexp-region' in each buffer of list BUFFERS.
A prefix arg >= 0 means unhighlight `mouse-face', not `face'.
A prefix arg <= 0 means unhighlight all visible or iconified buffers.
Otherwise, you are prompted for the BUFFERS to unhighlight, one at a
 time.  Use `C-g' to end prompting.
If you specify no BUFFERS then the current buffer is unhighlighted.

See `hlt-highlight-regexp-region' for other arguments."
  (interactive (list (hlt-+/--read-regexp "" 'REGION)
                     (if (and current-prefix-arg  (<= (prefix-numeric-value current-prefix-arg) 0))
                         (hlt-remove-if-not (lambda (bf) (get-buffer-window bf 0)) (buffer-list))
                       (hlt-+/--read-bufs 'UN))
                     nil
                     t))
  (hlt-unhighlight-regexp-region nil nil regexp nil msgp current-prefix-arg nth buffers))
;;;###autoload
(defun hlt-unhighlight-regexp-region (&optional start end regexp face msgp mousep nth buffers)
  "Unhighlight text matching regular expression REGEXP in region/buffer.
This is like `hlt-highlight-regexp-region' (which see), but opposite.
Where `hlt-highlight-regexp-region' highlights REGEXP matches, this
unhighlights the matches."
  (interactive (list nil nil (hlt-+/--read-regexp "UN" 'REGION) nil t current-prefix-arg))
  (hlt-+/--highlight-regexp-region 'UNHIGHLIGHT start end regexp face msgp mousep nth buffers))

;;;###autoload
(defun hlt-highlight-regexp-to-end (regexp &optional face msgp mousep nth)
  "Highlight text after cursor that matches REGEXP.
The behavior respects `hlt-use-overlays-flag' and depends on the
optional arguments, as follows:

 Optional 2nd arg FACE is the face to use.
  Interactively, this is the last face that was used for highlighting.
  (You can use command `hlt-choose-default-face' to choose a different
  face.)

 Optional 3rd arg MSGP non-nil means to display a progress message.
  Interactively, MSGP is t.

 Optional 4th arg MOUSEP non-nil means use property `mouse-face', not
 `face'.  Interactively, MOUSEP is provided by the prefix arg.

 Optional 5th arg NTH determines which regexp subgroup is highlighted.
  If nil or 0, the entire regexp is highlighted.  Otherwise, the NTH
  regexp subgroup (\"\\\\(...\\\\)\" expression) is highlighted.
  (NTH is not available interactively.)"
  (interactive (hlt-+/--highlight-regexp-read-args "" nil))
  (hlt-+/--highlight-regexp-region nil (point) (point-max) regexp face msgp mousep nth))

;;;###autoload
(defun hlt-unhighlight-regexp-to-end (regexp &optional face msgp mousep nth)
  "UNhighlight text after cursor that matches REGEXP.
This is like `hlt-highlight-regexp-to-end' (which see), but opposite.
Where `hlt-highlight-regexp-to-end' highlights REGEXP matches, this
unhighlights the matches."
  (interactive (hlt-+/--highlight-regexp-read-args "UN" nil))
  (hlt-+/--highlight-regexp-region 'UNHIGHLIGHT (point) (point-max) regexp face msgp mousep nth))

;;;###autoload
(defun hlt-highlight-regexp-groups-region (&optional start end regexp msgp mousep)
  "Like `hlt-highlight-regexp-region', but highlight regexp groups.
Highlight regular expression REGEXP in region/buffer.
Use the region if active, or the buffer otherwise.

Up to 8 group levels are highlighted, using faces `hlt-regexp-level-1'
through `hlt-regexp-level-8'.  The current default highlighting face
is used to highlight the non-group matches. You can change the default
face using command `hlt-choose-default-face'.

Optional args START and END are the limits of the area to act on.
  They default to the region limits.
Optional 4th arg MSGP:
  t means to treat this as an interactive call when deciding to
    display all messages.
  non-nil & non-t means to display only error and warning messages.
Optional 5th arg MOUSEP non-nil means to use `mouse-face' property,
  not `face'.  Interactively, this is provided by the prefix arg."
  (interactive (list nil nil (hlt-+/--read-regexp "" 'REGION) t current-prefix-arg))
  (hlt-+/--highlight-regexp-region nil start end regexp :hlt-regexp-groups msgp mousep nil))

;;;###autoload
(defun hlt-unhighlight-regexp-groups-region (&optional start end regexp msgp mousep)
  "Like `hlt-highlight-regexp-groups-region', but opposite.
Where `hlt-highlight-regexp-groups-region' highlights REGEXP matches,
this unhighlights the matches."
  (interactive (list nil nil (hlt-+/--read-regexp "UN" 'REGION) t current-prefix-arg))
  (hlt-+/--highlight-regexp-region 'UNHIGHLIGHT start end regexp :hlt-regexp-groups msgp mousep nil))

(defun hlt-+/--highlight-regexp-region (unhighlightp start end regexp face msgp mousep nth &optional buffers)
  "Helper for `hlt-(un)highlight-regexp-region'.
Non-nil UNHIGHLIGHTP means unhighlight.  Otherwise, highlight.

The other arguments are as for `hlt-highlight-regexp-region', except
that if FACE is `:hlt-regexp-groups' then (un)highlighting is for
regexp groups and NTH and BUFFERS are ignored.

If UNHIGHLIGHTP:
 Do not advance to the next face, even if `hlt-auto-faces-flag'.
 If FACE is nil then unhighlight all faces."
  (unless regexp (setq regexp  hlt-last-regexp))
  (unless (stringp regexp)              ; Else re-search-forward gets an error
    (error "HLT-%sHIGHLIGHT-REGEXP-REGION: REGEXP arg is not a string: `%S'"
           (if unhighlightp "UN" "")
           regexp))
  (let ((mbufs          buffers)
        (regexp-groups  (eq :hlt-regexp-groups face)))
    (unless buffers (setq buffers  (list (current-buffer))))
    ;; Advance the face if highlighting (but not unhighlighting) with auto faces.
    (when (and hlt-auto-faces-flag
               (not unhighlightp)
               (not regexp-groups))
      (hlt-next-face))
    (unless regexp-groups (if face
                              (setq hlt-last-face  face)
                            (unless unhighlightp (setq face  hlt-last-face))))
    (dolist (buf  buffers)
      (with-current-buffer buf
        ;; Use START and END if provided non-interactively, but not otherwise.
        (unless (and start  end  (or (not (cadr buffers))  (not (interactive-p))))
          (let ((start-end  (hlt-region-or-buffer-limits buf)))
            (setq start  (car start-end)
                  end    (cadr start-end))))
        ;; Do nothing if START or END is a marker for a different buffer.
        (when (and (or (not (markerp start))  (eq buf (marker-buffer start)))
                   (or (not (markerp end))    (eq buf (marker-buffer end))))
          (when (and msgp  (not unhighlightp))
            (let ((reg-size  (abs (- end start))))
              (when (and (> reg-size hlt-max-region-no-warning)
                         (not (progn (and (fboundp 'flash-ding) ; In `frame-fns.el'
                                          (flash-ding 'no-terminate-macros (selected-frame)))
                                     (y-or-n-p (substitute-command-keys
                                                (format "Lots of highlighting slows things down.  Do you \
really want to highlight up to %d chars?  "
                                                        reg-size))))))
                (hlt-user-error "OK, highlighting cancelled"))))
          (when (eq t msgp)
            (message "%sighlighting occurrences of `%s'%s..."
                     (if unhighlightp "UNh" "H")
                     regexp
                     (if mbufs (format " in `%s'"  buf) "")))
          (let ((hits-p               nil)
                (hlt-auto-faces-flag  nil)) ; Prevent advancing - we already advanced.
            (save-excursion
              (goto-char start)
              (while (and (< start end)  (not (eobp))  (re-search-forward regexp end t)  (setq hits-p  t))
                (condition-case nil
                    (when (and hits-p  (equal (match-beginning 0) (match-end 0))) (forward-char 1))
                  (end-of-buffer (setq start  end)))
                (if (and regexp-groups  (not unhighlightp))
                    (hlt-highlight-regexp-groups regexp msgp mousep)
                  (funcall (if regexp-groups
                               #'hlt-unhighlight-regexp-region
                             (if unhighlightp #'hlt-unhighlight-region #'hlt-highlight-region))
                           (match-beginning (if (or regexp-groups  (not nth)) 0 nth))
                           (match-end       (if (or regexp-groups  (not nth)) 0 nth))
                           (if regexp-groups regexp face)
                           nil
                           mousep))))
            (when (eq t msgp)
              (if hits-p
                  (message "%sighlighting occurrences of `%s'%s done  %s"
                           (if unhighlightp "UNh" "H")
                           regexp
                           (if mbufs (format " in `%s'"  buf) "")
                           (if unhighlightp
                               ""
                             (let ((remove-msg  "\\[hlt-unhighlight-regexp-region]' to remove highlighting"))
                               (when mousep (setq remove-msg  (concat "\\[universal-argument] " remove-msg)))
                               (setq remove-msg  (substitute-command-keys (concat "`" remove-msg)))
                               remove-msg)))
                (message "No occurrences of `%s' in `%s'" regexp buf))))
          (setq hlt-last-regexp  regexp))))))

(defun hlt-highlight-regexp-groups (regexp msgp mousep)
  "Highlight the regexp groups in the matched text according to REGEXP.
Highlight non-group parts of the match using `hlt-last-face'.
Uses match data for the existing match."
  (let ((level                  1)
        (hlt-overlays-priority  hlt-overlays-priority)
        (max-levels             (min (regexp-opt-depth regexp) 8)))
    (save-excursion
      (condition-case nil
          (let ((hlt-auto-faces-flag  nil))
            (hlt-highlight-region (match-beginning 0) (match-end 0) hlt-last-face msgp mousep)
            (while (<= level max-levels)
              (unless (equal (match-beginning level) (match-end level))
                (setq hlt-overlays-priority  (1+ hlt-overlays-priority))
                (hlt-highlight-region (match-beginning level) (match-end level)
                                      (intern (concat "hlt-regexp-level-" (number-to-string level)))
                                      msgp mousep))
              (setq level  (1+ level))))
        (error nil)))))

;;;###autoload
(defun hlt-unhighlight-region-for-face-in-buffers (face buffers &optional msgp)
  "Use `hlt-unhighlight-region-for-face' in each buffer of list BUFFERS.
A prefix arg >= 0 means unhighlight `mouse-face', not `face'.
A prefix arg <= 0 means unhighlight all visible or iconified buffers.
Otherwise, you are prompted for the BUFFERS to unhighlight, one at a
 time.  Use `C-g' to end prompting.
If you specify no BUFFERS then the current buffer is unhighlighted.

See `hlt-unhighlight-region-for-face' for other arguments."
  (interactive (list (hlt-read-bg/face-name "Remove highlight overlays that use face: ")
                     (if (and current-prefix-arg  (<= (prefix-numeric-value current-prefix-arg) 0))
                         (hlt-remove-if-not (lambda (bf) (get-buffer-window bf 0)) (buffer-list))
                       (hlt-+/--read-bufs 'UN))
                     t))
  (hlt-unhighlight-region-for-face face nil nil msgp current-prefix-arg buffers))

;;;###autoload
(defun hlt-unhighlight-region-for-face (&optional face start end msgp mousep buffers)
  "Remove any highlighting in the region that uses FACE.
Same as `hlt-unhighlight-region', but removes only highlighting
that uses FACE.  Interactively, you are prompted for the face.

This works only for overlay highlighting, not text-property
highlighting.

Note: When text in the region has been highlighted using more than one
face, unhighlighting for one of those faces can mean that adjacent
highlighting outside the region appears to change.  That outside text
still has the same multiple-overlay face highlighting, but the overlay
stacking order is not the same as it was.

Optional arg FACE is the face to use.
  Interactively, this is the last face that was used for highlighting.
  (You can use command `hlt-choose-default-face' to choose a different face.)
Optional args START and END are the limits of the area to act on.
  They default to the region limits.  (But see BUFFERS, below.)
Optional arg MSGP non-nil means show status messages.
Optional arg MOUSEP non-nil means use `mouse-face' property, not
  `face'.  Interactively, MOUSEP is provided by the prefix arg.
Optional arg BUFFERS is the list of buffers to unhighlight for FACE.
  If non-nil then explicit START and END values are ignored, and the
  actual values are determined automatically for each buffer, based on
  whether the region is active there."
  (interactive `(,(hlt-read-bg/face-name "Remove highlight overlays that use face: ")
                 ,@(hlt-region-or-buffer-limits) t ,current-prefix-arg))
  (unless buffers (setq buffers  (list (current-buffer))))
  (if face (setq hlt-last-face  face) (setq face  hlt-last-face))
  (hlt-unhighlight-region start end face msgp mousep buffers))

;; No longer used - use `hlt-unhighlight-for-overlay' instead.
(defun hlt-delete-highlight-overlay (overlay &optional face)
  "Delete OVERLAY if it was created by highlighting (library `highlight').
Optional arg FACE is a face symbol.  If non-nil, then delete only
overlays with that FACE."
  (let ((highlight-face  (overlay-get overlay 'hlt-highlight)))
    (when (and highlight-face  (or (not face)  (equal face highlight-face)))
      (delete-overlay overlay))))

(unless (fboundp 'copy-overlay)         ; Defined in Emacs 22+.
  (defun copy-overlay (o)
    "Return a copy of overlay O."
    (let ((o1 (if (overlay-buffer o)
                  (make-overlay (overlay-start o) (overlay-end o)
                                ;; FIXME: there's no easy way to find the insertion-type of the two markers.
                                (overlay-buffer o))
                (let ((o1  (make-overlay (point-min) (point-min))))
                  (delete-overlay o1)
                  o1)))
          (props (overlay-properties o)))
      (while props (overlay-put o1 (pop props) (pop props)))
      o1)))

;;;###autoload
(defun hlt-replace-highlight-face-in-buffers (old-face new-face buffers &optional msgp)
  "Use `hlt-replace-highlight-face' in each buffer of list BUFFERS.
A prefix arg >= 0 means highlight with `mouse-face', not `face'.
A prefix arg <= 0 means highlight all visible or iconified buffers.
Otherwise, you are prompted for the BUFFERS to highlight, one at a
 time.  Use `C-g' to end prompting.
If you specify no BUFFERS then the current buffer is highlighted.

See `hlt-replace-highlight-face' for other arguments."
  (interactive (list (hlt-read-bg/face-name "Replace face in region highlights. OLD face: ")
                     (hlt-read-bg/face-name "NEW face: ")
                     (if (and current-prefix-arg  (<= (prefix-numeric-value current-prefix-arg) 0))
                         (hlt-remove-if-not (lambda (bf) (get-buffer-window bf 0)) (buffer-list))
                       (hlt-+/--read-bufs))))
  (hlt-replace-highlight-face old-face new-face nil nil msgp current-prefix-arg buffers))

;;;###autoload
(defun hlt-replace-highlight-face (old-face new-face &optional start end msgp mousep buffers)
  "Replace OLD-FACE by NEW-FACE in overlay highlighting in the region.
This command applies only to overlay highlighting created by library
`highlight.el'.

Update the last-used highlighting face.

With a prefix argument, replace OLD-FACE as the `mouse-face' property,
 not the `face' property.

Other arguments:
 Optional args START and END are the limits of the area to act on.
  They default to the region limits.  If the region is not active or
  it is empty, then use the whole buffer.
 Optional arg MSGP non-nil means display a progress message.
 Optional arg MOUSEP non-nil means use `mouse-face' property, not
  `face'.  Interactively, MOUSEP is provided by the prefix arg."
  (interactive `(,(hlt-read-bg/face-name "Replace face in region highlights. Old face: ")
                 ,(hlt-read-bg/face-name "New face: ")
                 ,@(hlt-region-or-buffer-limits) t ,current-prefix-arg))
  (let ((mbufs  buffers))
    (unless buffers (setq buffers  (list (current-buffer))))
    (dolist (buf  buffers)
      (with-current-buffer buf
        ;; Use START and END if provided non-interactively, but not otherwise.
        (unless (and start  end  (or (not (cadr buffers))  (not (interactive-p))))
          (let ((start-end  (hlt-region-or-buffer-limits buf)))
            (setq start  (car start-end)
                  end    (cadr start-end))))
        ;; Do nothing if START or END is a marker for a different buffer.
        (when (and (or (not (markerp start))  (eq buf (marker-buffer start)))
                   (or (not (markerp end))    (eq buf (marker-buffer end))))
          (when msgp
            (message "Replacing overlay highlighting face `%s'%s..."
                     old-face
                     (if mbufs (format " in `%s'"  buf) "")))
          (let ((read-only-p  buffer-read-only)
                (modified-p   (buffer-modified-p)))
            (setq buffer-read-only  nil)
            (dolist (ov  (overlays-in start end))
              (when (equal old-face (overlay-get ov (if mousep 'mouse-face hlt-face-prop)))
                (overlay-put ov (if mousep 'mouse-face hlt-face-prop) new-face)
                (overlay-put ov 'hlt-highlight                        new-face)))
            (setq buffer-read-only  read-only-p)
            (set-buffer-modified-p modified-p))
          (setq hlt-last-face  new-face)
          (when msgp (message "Replacing overlay highlighting face `%s'%s... done"
                              old-face
                              (if mbufs (format " in `%s'"  buf) ""))))))))

;;;###autoload
(defun hlt-highlight-symbol (symbol &optional all-buffers-p face)
  "Highlight occurrences of SYMBOL.
The symbol at point is used by default, or the symbol under the mouse
pointer if the command is invoked using the mouse.

Highlight over the region if active, or the buffer otherwise.
With a prefix arg, use all buffers that are visible or iconified.
\(This first unhighlights occurrences, to prevent stacking up multiple
highlighting on the same occurrences.)

When called from Lisp:
* ALL-BUFFERS-P corresponds to the prefix-argument behavior.
* FACE is the face to use."
  (interactive
   (save-excursion
     (when (listp last-nonmenu-event)
       (mouse-set-point last-nonmenu-event))
     (let ((symb  (symbol-at-point)))
       (unless symb
         (hlt-user-error "No symbol %s" (if (listp last-nonmenu-event) "under mouse pointer" "at point")))
       (list symb current-prefix-arg))))
  (let ((hlt-auto-faces-flag  (not face))
        (regexp               (format (if (> emacs-major-version 21) "\\_<%s\\_>" "%s") symbol))
        (bufs                 (if all-buffers-p
                                  (hlt-remove-if-not (lambda (bf) (get-buffer-window bf 0)) (buffer-list))
                                (list (current-buffer))))
        (first-buf-p          t)
        limits)
    (dolist (buf  bufs)
      (with-current-buffer buf
        (unless first-buf-p (setq hlt-auto-faces-flag  face)) ; FACE is nil interactively.
        (setq limits  (hlt-region-or-buffer-limits))
        (hlt-unhighlight-regexp-region (car limits) (cadr limits) regexp)
        (hlt-highlight-regexp-region   (car limits) (cadr limits) regexp face))
      (setq first-buf-p  nil))))

;;;###autoload
(defun hlt-unhighlight-symbol (symbol &optional all-buffers-p face)
  "Unhighlight occurrences of SYMBOL.
The symbol at point is used by default, or the symbol under the mouse
pointer if the command is invoked using the mouse.

Unhighlight over the region if active, or the buffer otherwise.
With a prefix arg, use all buffers that are visible or iconified.

When called from Lisp:
* ALL-BUFFERS-P corresponds to the prefix-argument behavior.
* FACE is the face to unhighlight."
  (interactive
   (save-excursion
     (when (listp last-nonmenu-event)
       (mouse-set-point last-nonmenu-event))
     (let ((symb  (symbol-at-point)))
       (unless symb
         (hlt-user-error "No symbol %s" (if (listp last-nonmenu-event) "under mouse pointer" "at point")))
       (list symb current-prefix-arg))))
  (let ((hlt-auto-faces-flag  (not face))
        (regexp               (format (if (> emacs-major-version 21) "\\_<%s\\_>" "%s") symbol))
        (bufs                 (if all-buffers-p
                                  (hlt-remove-if-not (lambda (bf) (get-buffer-window bf 0)) (buffer-list))
                                (list (current-buffer))))
        limits)
    (dolist (buf  bufs)
      (with-current-buffer buf
        (setq limits  (hlt-region-or-buffer-limits))
        (hlt-unhighlight-regexp-region (car limits) (cadr limits) regexp face)))))

;;;###autoload
(defun hlt-highlight-enclosing-list (arg &optional face mousep)
  "Highlight the ARGth level sexp enclosing point.
ARG is the numeric prefix value.

A negative prefix arg prompts you for the face to use.  This face is
used by default from then on.  You can also choose the default face
using command `hlt-choose-default-face'.  The same face is used as the
default for all `hlt-*' functions.

When used in Lisp code:
 MOUSEP non-nil means use property `mouse-face', not `face'."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     (if (< (prefix-numeric-value current-prefix-arg) 0)
                         (call-interactively #'hlt-choose-default-face)
                       hlt-last-face)))
  (unless arg (setq arg  1))
  (set-mark (save-excursion (up-list (- arg)) (point)))
  (up-list arg)
  (unless face (setq face  hlt-last-face))
  (hlt-highlight-region nil nil face 'MSGP mousep)
  (setq mark-active  nil))

;;;###autoload
(defun hlt-highlight-single-quotations (&optional face)
  "Highlight single-quoted text in the region.
This means, for example, commands and keys between `'s: `foobar'.
If the region is not active or it is empty, then use the whole buffer.
With a prefix argument, prompt for the highlighting face to use.
Otherwise, use the last face used for highlighting.
 You can also use command `hlt-choose-default-face' to choose a
 different face."
  (interactive "P")
  (if face
      (setq face           (hlt-read-bg/face-name "Use highlighting face: ")
            hlt-last-face  face)
    (setq face  hlt-last-face))
  (apply #'hlt-highlight-regexp-region
         (append (hlt-region-or-buffer-limits) (list "`\\([^']+\\)'" face (and (interactive-p)  t) nil 1))))

;;;###autoload
(defun hlt-highlight-line-dups-region (&optional start end msgp flip)
  "Highlight sets of duplicate lines in the region.
Each set is given a different background, according to user option
`hlt-auto-face-backgrounds'.

Whether leading and trailing whitespace is ignored is controlled by
option `hlt-line-dups-ignore-regexp'.  But a prefix argument reverses
this: if the option value is \"\" then whitespace defined by
\"[ \t]*\" is ignored, and otherwise whitespace is not ignored."
  (interactive `(,@(hlt-region-or-buffer-limits) t ,current-prefix-arg))
  (let ((hlt-auto-faces-flag  t)
        count line ignore line-re)
    (save-excursion
      (goto-char start)
      (while (< (point) end)
        (setq count    0
              line     (buffer-substring-no-properties (line-beginning-position)
                                                       (line-end-position))
              ignore   (and (not (string= "" line))
                            (if hlt-line-dups-ignore-regexp
                                (if flip "" hlt-line-dups-ignore-regexp)
                              (if flip "[ \t]*" "")))
              line-re  (concat "^" ignore (regexp-quote line) ignore "$"))
        (save-excursion
          (goto-char start)
          (while (< (point) end)
            (if (not (re-search-forward line-re end t))
                (goto-char end)
              (setq count  (1+ count))
              (unless (< count 2)
                (hlt-highlight-region (line-beginning-position) (line-end-position))
                (forward-line 1)))))
        (forward-line 1)))))

;;;###autoload
(defun hlt-highlight-lines (start end face msgp)
  "Highlight full lines in region, window-wide."
  (interactive (list (save-excursion (goto-char (region-beginning)) (line-beginning-position))
                     (save-excursion (goto-char (region-end))       (line-beginning-position 2))
                     nil
                     t))
  (hlt-highlight-region start end face msgp))

;;;###autoload
(defun hlt-mouse-face-each-line (&optional start end face msgp)
  "Put `mouse-face' on each line of buffer in region.
If the region is active and not empty, then limit mouse-face
highlighting to the region.  Otherwise, use the whole buffer.
With a prefix argument, prompt for the highlighting face to use.
Otherwise, use the last face used for highlighting.
 You can also use command `hlt-choose-default-face' to choose a
 different face.
Optional args START and END are the limits of the area to act on.
  They default to the region limits.
Optional arg MSGP non-nil means display a progress message."
  (interactive `(,@(hlt-region-or-buffer-limits) ,current-prefix-arg t))
  (unless (and start  end) (let ((start-end  (hlt-region-or-buffer-limits)))
                             (setq start  (car start-end)
                                   end    (cadr start-end))))
  (if face
      (setq face           (hlt-read-bg/face-name "Use highlighting face: ")
            hlt-last-face  face)
    (setq face  hlt-last-face))
  (when msgp (message "Putting mouse face `%s' on each line..." face))
  (let ((buffer-read-only           nil)
        (inhibit-field-text-motion  t)  ; Just to be sure, for `end-of-line'.
        overlay)
    (save-excursion
      (save-restriction
        (narrow-to-region start end)
        (goto-char (point-min))
        (while (not (eobp))
          (cond (hlt-use-overlays-flag
                 (setq overlay  (make-overlay (point) (setq start  (progn (end-of-line) (point)))))
                 (overlay-put overlay 'mouse-face    face)
                 (overlay-put overlay 'hlt-highlight face)
                 (overlay-put overlay 'priority      hlt-overlays-priority))
                (t
                 (put-text-property (point) (progn (end-of-line) (point)) 'mouse-face face)
                 (put-text-property start end 'hlt-highlight face)))
          (forward-line 1)))))
  (when msgp (message "Putting mouse face `%s' on each line... done" face)))

;;;###autoload
(defun hlt-toggle-use-overlays-flag ()
  "Toggle `hlt-use-overlays-flag'.
If the current value is non-nil, it is set to nil.
If the current value is nil, it is set to the last non-nil value."
  (interactive)
  (let ((before-toggle  hlt-use-overlays-flag))
    (setq hlt-use-overlays-flag                 (and (not hlt-use-overlays-flag)
                                                     hlt-previous-use-overlays-flag-value)
          hlt-previous-use-overlays-flag-value  before-toggle))
  (message (cond ((eq hlt-use-overlays-flag 'only)
                  "Highlight actions now use only overlay properties, not text properties")
                 (hlt-use-overlays-flag
                  "Highlighting with overlays now, but actions affect also text properties")
                 (t "Highlight actions now use only text properties, not overlay properties"))))
 
;;(@* "Functions for Highlighting Zones - Emacs 22+")

;;; Functions for Highlighting Zones - Emacs 22+ ----------

(when (> emacs-major-version 21)

  ;; Avoid using `define-obsolete-function-alias', for compatibility with Emacs 20 byte-compiler.
  ;;
  (defalias      'zz-izone-limits         #'zz-basic-zones)
  (make-obsolete 'zz-izone-limits         #'zz-basic-zones)
  (defalias      'zz-izone-limits-in-bufs #'zz-basic-zones-in-bufs)
  (make-obsolete 'zz-izone-limits-in-bufs #'zz-basic-zones-in-bufs)

  (defalias      'hlt-highlight-regions        #'hlt-highlight-zones)
  (make-obsolete 'hlt-highlight-regions        #'hlt-highlight-zones)
  (defalias      'hlt-highlight-regions-in-buf #'hlt-highlight-zones-in-bufs)
  (make-obsolete 'hlt-highlight-regions-in-buf #'hlt-highlight-zones-in-bufs)

  (defalias      'hlt-unhighlight-regions         #'hlt-unhighlight-zones)
  (make-obsolete 'hlt-unhighlight-regions         #'hlt-unhighlight-zones)
  (defalias      'hlt-unhighlight-regions-in-bufs #'hlt-unhighlight-zones-in-bufs)
  (make-obsolete 'hlt-unhighlight-regions-in-bufs #'hlt-unhighlight-zones-in-bufs)

  ;; No need to use (zz-basic-zones zz-izones nil 'ONLY-THIS-BUFFer), since `hlt-highlight-region' DTRT.
;;;###autoload
  (defun hlt-highlight-zones (&optional zones face msgp mousep buffers)
    "Apply `hlt-highlight-region' to each zone in `zz-izones'.
No prefix arg means highlight the zones.
A non-positive prefix arg means UNhighlight the zones instead.
A non-negative prefix arg means highlight with property `mouse-face',
not property `face'.

Interactively, the face used for highlighting is the last face that
was used for highlighting.  (You can use `\\[hlt-choose-default-face]' to choose a
different face.)

You need library `zones.el' to use this command interactively.

When called from Lisp, ZONES is a list of (START END) zone limits, and
the other args are passed to `hlt-highlight-region'."
    (interactive (list (if (require 'zones nil t)
                           (zz-basic-zones zz-izones)
			 (hlt-user-error "You need library `zones.el' to use this command interactively"))
                       nil
                       t
                       (and current-prefix-arg  (natnump (prefix-numeric-value current-prefix-arg)))))
    (let ((fn  (if (and (interactive-p)
			current-prefix-arg
			(not (> (prefix-numeric-value current-prefix-arg) 0)))
                   #'hlt-unhighlight-region
		 #'hlt-highlight-region)))
      (dolist (start+end  zones)
	(funcall fn (nth 0 start+end) (nth 1 start+end) face msgp mousep buffers))))

  ;; No need to use (zz-basic-zones zz-izones nil 'ONLY-THIS-BUFFER), since `hlt-unhighlight-region' DTRT.
;;;###autoload
  (defun hlt-unhighlight-zones (&optional zones face msgp mousep buffers)
    "Apply `hlt-unhighlight-region' to each zone in `zz-izones'.
With a prefix arg, remove property `mouse-face' from the zones.
Interactively, the face unhighlighted is the last face that was used
for highlighting.  (You can use `\\[hlt-choose-default-face]' to choose a different face.)

You need library `zones.el' to use this command interactively.

When called from Lisp, ZONES is a list of (START END) zone limits, and
the other args are passed to `hlt-unhighlight-region'."
    (interactive (list (if (require 'zones nil t)
                           (zz-basic-zones zz-izones)
			 (hlt-user-error "You need library `zones.el' to use this command interactively"))
                       nil
                       t
                       current-prefix-arg))
    (dolist (start+end  zones)
      (hlt-unhighlight-region (car start+end) (cadr start+end) face msgp mousep buffers)))

  (defun hlt-highlight-zones-in-buffers (buffers &optional msgp)
    "Use `hlt-highlight-zones' in each buffer of list BUFFERS.
A prefix arg >= 0 means highlight with `mouse-face', not `face'.
A prefix arg <= 0 means highlight all visible or iconified buffers.
Otherwise, you are prompted for the BUFFERS to highlight, one at a
 time.  Use `C-g' to end prompting.
If you specify no BUFFERS then the current buffer is highlighted.

You need library `zones.el' for this command.

Non-nil optional arg MSGP means show status messages."
    (interactive (list (if (and current-prefix-arg  (<= (prefix-numeric-value current-prefix-arg) 0))
                           (hlt-remove-if-not (lambda (bf) (get-buffer-window bf 0)) (buffer-list))
			 (hlt-+/--read-bufs))
                       nil
                       'MSGP))
    (hlt-highlight-zones (zz-basic-zones-in-bufs buffers) nil msgp
			 (and current-prefix-arg  (>= (prefix-numeric-value current-prefix-arg) 0))
			 buffers))

  (defun hlt-unhighlight-zones-in-buffers (buffers &optional msgp)
    "Use `hlt-unhighlight-zones' in each buffer of list BUFFERS.
A prefix arg >= 0 means unhighlight `mouse-face', not `face'.
A prefix arg <= 0 means unhighlight all visible or iconified buffers.
Otherwise, you are prompted for the BUFFERS to unhighlight, one at a
 time.  Use `C-g' to end prompting.
If you specify no BUFFERS then the current buffer is unhighlighted.

You need library `zones.el' for this command.

Non-nil optional arg MSGP means show status messages."
    (interactive (list (if (and current-prefix-arg  (<= (prefix-numeric-value current-prefix-arg) 0))
                           (hlt-remove-if-not (lambda (bf) (get-buffer-window bf 0)) (buffer-list))
			 (hlt-+/--read-bufs))))
    (hlt-unhighlight-zones (zz-basic-zones-in-bufs buffers) nil msgp
                           (and current-prefix-arg  (>= (prefix-numeric-value current-prefix-arg) 0))
                           buffers))

  )
 
;;(@* "Copying and Yanking Text Properties")

;;; Copying and Yanking Text Properties -------------------

;;;###autoload
(defalias 'hlt-paste-props 'hlt-yank-props)
;;;###autoload
(defun hlt-yank-props (start end &optional arg msgp)
  "Yank (paste) copied text properties over the active region.
Interactively, do nothing if there is no nonempty active region.
By default, yank only the copied properties defined by
 `hlt-default-copy/yank-props'.
With a plain or non-negative prefix arg, yank all copied properties.
With a negative prefix arg, you are prompted for the copied properties
 to yank.  To finish entering properties, hit `RET RET' (i.e., twice).

NOTE: If the list of copied text properties is empty, then yanking
      *removes all properties* from the text in the region.  This
      provides an easy way to UNpropertize text."
  (interactive "r\nP\np")
  ;; Do nothing if no active region.
  (unless (or (hlt-nonempty-region-p)  (not msgp)) (hlt-user-error "No region to paste properties to"))
  (let ((read-only                           buffer-read-only)
        (modified-p                          (buffer-modified-p))
        (inhibit-modification-hooks          t)
        ;; Otherwise, `put-text-property' calls this, which removes highlight.
        (font-lock-fontify-region-function   'ignore)
        (props-to-yank                       (hlt-props-to-copy/yank hlt-copied-props arg)))
    (undo-boundary)
    (setq buffer-read-only  nil)
    (set-text-properties start end props-to-yank)
    ;; Set/reset props `hlt-highlight' and `font-lock-ignore', if `face' is one of the props.
    ;; (The Emacs 20 code here is fudged: it just uses `member' instead of `plist-member'.)
    (cond ((fboundp 'plist-member)
           (put-text-property start end 'hlt-highlight (and (plist-member props-to-yank hlt-face-prop)  t))
           (when (eq 'face hlt-face-prop)
             (put-text-property start end 'font-lock-ignore (and (plist-member props-to-yank hlt-face-prop)
                                                                 t))))
          ;; Emacs 20 - no `plist-member'.  (Though `font-lock-ignore' has no effect for Emacs 20.)
          (t
           (put-text-property start end 'hlt-highlight    (and (member hlt-face-prop props-to-yank)  t))
           (when (eq 'face hlt-face-prop)
             (put-text-property start end 'font-lock-ignore (and (member hlt-face-prop props-to-yank)  t)))))
    (setq buffer-read-only  read-only)
    (set-buffer-modified-p modified-p)
    (when msgp (if props-to-yank
                   (message "Yanked propert%s `%s'" (if (car (cddr props-to-yank)) "ies" "y")
                            (let ((pprops  ()))
                              (while props-to-yank
                                (push (pop props-to-yank) pprops)
                                (pop props-to-yank))
                              (mapconcat #'symbol-name (nreverse pprops) "', `")))
                 (message "ALL PROPERTIES REMOVED (yanked empty list of properties)")))))

;;;###autoload
(defun hlt-mouse-copy-props (&optional event arg msgp)
  "Same as `hlt-copy-props', but copy at mouse pointer, not at point."
  (interactive "e\nP\np")
  (with-current-buffer (window-buffer (posn-window (event-end event)))
    (save-excursion (goto-char (posn-point (event-end event))) (hlt-copy-props (point) arg msgp))))

;; For testing
;; (global-set-key [C-S-down-mouse-2] 'hlt-mouse-copy-props)
;; (global-set-key [C-S-mouse-2]      'ignore)

;;;###autoload
(defun hlt-copy-props (&optional position arg msgp)
  "Copy text properties at point for use by `hlt-yank-props'.
Properties are copied to `hlt-copied-props'.
By default, copy the properties defined by
 `hlt-default-copy/yank-props'.
With a plain or non-negative prefix arg, copy all properties.
With a negative prefix arg, you are prompted for the properties to
 copy.  To finish entering properties, hit `RET RET' (i.e., twice)."
  (interactive "d\nP\np")
  (unless position  (setq position  (point)))
  (let ((props-to-copy  (hlt-props-to-copy/yank (text-properties-at position) arg)))
    (setq hlt-copied-props  props-to-copy)
    (when msgp (if props-to-copy
                   (message "Copied propert%s `%s'" (if (car (cddr props-to-copy)) "ies" "y")
                            (let ((pprops  ()))
                              (while props-to-copy
                                (push (pop props-to-copy) pprops)
                                (pop props-to-copy))
                              (mapconcat #'symbol-name (nreverse pprops) "', `")))
                 (message "Emptied copied properties list - yanking will REMOVE ALL")))))

(defun hlt-props-to-copy/yank (avail-props arg)
  "Return a plist of properties to copy or yank.
AVAIL-PROPS is a plist of available properties.
ARG is from a raw prefix argument.
 If nil, then use the properties from AVAIL-PROPS that are also
  among those specified by `hlt-default-copy/yank-props'.
 If a plain or non-negative prefix arg, then use all properties in
  AVAIL-PROPS.
 If a negative prefix arg, then prompt for the properties
  to use, using completion against the candidates in AVAIL-PROPS."
  (cond ((and arg  (natnump (prefix-numeric-value arg)))
         (copy-sequence avail-props))   ; Copy/yank all props available.
        (arg                            ; Prompt for props, from among those available.
         (let ((props-avail  avail-props)
               (props-alist  ()))
           (while props-avail (push (cons (symbol-name (pop props-avail)) (pop props-avail)) props-alist))
           (if (not (cdr props-alist))
               avail-props
             (hlt-subplist (hlt-read-props-completing props-alist) avail-props))))
        (t                              ; Copy/yank the available default props.
         (if (eq t hlt-default-copy/yank-props)
             avail-props
           (hlt-subplist hlt-default-copy/yank-props avail-props)))))

(defun hlt-subplist (properties available)
  "Return a plist of the entries for PROPERTIES in plist AVAILABLE.
PROPERTIES is a list of properties, without their values."
  (let ((plist     ())
        (prop+val  nil))
    (dolist (prop  properties)
      (when (setq prop+val  (if (fboundp 'plist-member)
                                (plist-member available prop)
                              (member prop available))) ; Emacs 20 fudge.
        (push prop plist)
        (push (cadr prop+val) plist)))
    (nreverse plist)))

(defun hlt-read-props-completing (properties)
  "Read and return a list of text properties from those in PROPERTIES.
PROPERTIES is an alist whose cars are text property names (strings).
Read one property name at a time, with completion.  Use `C-g' if done."
  (let ((prompt1        "Property (RET for each, empty input to finish): ")
        (prompt2        "Property: ")
        (props-to-copy  ())
        prop)
    (setq prop        (completing-read prompt1 properties nil t)
          properties  (delete (assoc prop properties) properties))
    (unless (string= "" prop)
      (push (intern prop) props-to-copy)
      (while (and properties  (not (string= "" prop)))
        (setq prop        (completing-read prompt2 properties nil t)
              properties  (delete (assoc prop properties) properties))
        (unless (string= "" prop) (push (intern prop) props-to-copy)))
      (nreverse props-to-copy))))
 
;;(@* "Misc Functions - Emacs 21+")

;;; Misc Functions - Emacs 21+ ---------------------------------------

(when (fboundp 'next-single-char-property-change) ; Don't bother, for Emacs 20.
  (defun hlt-show-default-face (face)
    "Show FACE, by default, the default highlighting face.
With a prefix argument, prompt for the highlighting face to show.
Otherwise, show the last face used for highlighting.
 You can also use command `hlt-choose-default-face' to choose a different face."
    (interactive (list (if current-prefix-arg
                           (hlt-read-bg/face-name "Show highlighting face: ")
                         hlt-last-face)))
    (hlt-listify-invisibility-spec)
    (remove-from-invisibility-spec face))

  (defun hlt-listify-invisibility-spec ()
    "Convert `buffer-invisibility-spec' to list form.
If it is already a list, do nothing.
If it is t, set it to a list of all `invisible' spec values in the buffer.
That is, for each character in the buffer that has property `invisible',
the invisibility criteria specified by that value are accumulated."
    (unless (listp buffer-invisibility-spec)
      (setq buffer-invisibility-spec  nil)
      (let ((start  (point-min))
            (end    (point-max))
            spec)
        (dolist (ov  (overlays-in start end))
          (when (setq spec  (overlay-get ov 'invisible))
            (unless (listp spec) (setq spec  (list spec)))
            (setq buffer-invisibility-spec  (hlt-set-union spec buffer-invisibility-spec))))
        (while (< start end)
          (when (setq spec  (get-text-property start 'invisible))
            (unless (listp spec) (setq spec  (list spec)))
            (setq buffer-invisibility-spec  (hlt-set-union spec buffer-invisibility-spec)))
          (setq start  (1+ start)))))
    buffer-invisibility-spec)

  ;; From `cl-seq.el', function `union', without keyword treatment.
  (defun hlt-set-union (list1 list2)
    "Combine LIST1 and LIST2 using a set-union operation.
The result list contains all items that appear in either LIST1 or
LIST2.  This is a non-destructive function; it copies the data if
necessary."
    (cond ((null list1) list2)
          ((null list2) list1)
          ((equal list1 list2) list1)
          (t
           (unless (>= (length list1) (length list2))
             (setq list1  (prog1 list2 (setq list2  list1)))) ; Swap them.
           (while list2
             (unless (member (car list2) list1) (setq list1  (cons (car list2) list1)))
             (setq list2  (cdr list2)))
           list1)))

  ;; From `cl-seq.el', function `intersection', without keyword treatment.
  (defun hlt-set-intersection (list1 list2)
    "Set intersection of lists LIST1 and LIST2.
This is a non-destructive operation: it copies the data if necessary."
    (and list1  list2
         (if (equal list1 list2)
             list1
           (let ((result  ()))
             (unless (>= (length list1) (length list2))
               (setq list1  (prog1 list2 (setq list2  list1)))) ; Swap them.
             (while list2
               (when (member (car list2) list1) (setq result  (cons (car list2) result)))
               (setq list2  (cdr list2)))
             result))))

  (defun hlt-hide-default-face (&optional start end face)
    "Hide the last face used for highlighting.
With a prefix argument, prompt for the highlighting face to hide,
 instead.  You can also use command `hlt-choose-default-face' to
 choose a different face.

If `hlt-act-on-any-face-flag' is non-nil, then the face to be hidden
can be any face you choose.  Otherwise, it must be a face that has
been used for highlighting by library `highlight.el'.

Hiding a face at some location means two things:
1) setting its `invisible' property there, making it susceptible to
   being hidden by `buffer-invisibility-spec', and
2) adding it to `buffer-invisibility-spec', so that it is hidden.

This command hides all text with the specified face that has the
`invisible' property, throughout the entire buffer.  However, it only
adds the `invisible' property to text with an overlay or text
property, depending on `hlt-use-overlays-flag', and it only does so
within the region, if the region is active.

Non-interactively:
FACE is the face to hide. It defaults to the last highlighting face.
START and END are the limits of the area to act on. They default to
  the region limits."
    (interactive `(,@(hlt-region-or-buffer-limits)
                     ,(if current-prefix-arg
                          (hlt-read-bg/face-name "Hide highlighting face: ")
                        hlt-last-face)))
    (unless (and start  end) (let ((start-end  (hlt-region-or-buffer-limits)))
                               (setq start  (car start-end)
                                     end    (cadr start-end))))
    (unless face (setq face  hlt-last-face))
    (hlt-listify-invisibility-spec)
    ;; Add FACE to `invisible' property throughout START...END,
    ;; whenever it is used as a highlighting face.
    (save-excursion
      (save-window-excursion
        (goto-char start)
        (let ((zone-beg  start)
              zone-end zone)
          (while (and zone-beg  (< zone-beg end))
            (setq zone      (hlt-next-highlight zone-beg end face nil nil 'no-error-msg)
                  zone-beg  (car zone)
                  zone-end  (cdr zone))
            ;; Add FACE to `invisible' property from `zone-beg' to `zone-end'.
            (when hlt-use-overlays-flag
              (let ((overlays  (overlays-at zone-beg)))
                (while overlays
                  (when (and (or hlt-act-on-any-face-flag
                                 (equal face (overlay-get (car overlays) 'hlt-highlight)))
                             (equal face (overlay-get (car overlays) hlt-face-prop)))
                    (overlay-put (car overlays) 'invisible
                                 (hlt-add-listifying (overlay-get (car overlays) 'invisible) face)))
                  (when overlays (setq overlays  (cdr overlays))))))
            (when (and (not (eq hlt-use-overlays-flag 'only))
                       (or hlt-act-on-any-face-flag  (equal face (get-text-property (point) 'hlt-highlight)))
                       ;; $$$$$$ (equal face (get-text-property (point) hlt-face-prop)))
                       (let ((pt-faces  (get-text-property (point) hlt-face-prop)))
                         (if (consp pt-faces) (memq face pt-faces) (equal face pt-faces))))
              (put-text-property zone-beg zone-end 'invisible
                                 (hlt-add-listifying (get-text-property zone-beg 'invisible) face)))
            (hlt-add-to-invisibility-spec face))))))

  ;; Same as `add-to-invisibility-spec', except it doesn't allow duplicates.
  (defun hlt-add-to-invisibility-spec (element)
    "Add ELEMENT to `buffer-invisibility-spec'.
See documentation for `buffer-invisibility-spec' for the kind of elements
that can be added."
    (when (eq buffer-invisibility-spec t) (setq buffer-invisibility-spec  (list t)))
    (add-to-list 'buffer-invisibility-spec element))

  (defun hlt-add-listifying (orig-val val-to-add)
    "Add VAL-TO-ADD to list ORIG-VAL, listifying ORIG-VAL first if needed."
    (unless (listp orig-val) (setq orig-val  (list orig-val)))
    (add-to-list 'orig-val val-to-add)
    orig-val)

  ;; Suggested binding: `C-S-n'.
  (defun hlt-next-highlight (&optional start end face mousep backward-p no-error-p wrap-p)
    "Go to the next highlight in FACE.
Interactively:

 * If `hlt-auto-faces-flag' is non-nil then FACE is:
      the `hlt-auto-face-backgrounds' face at point, if any,
   or the last  `hlt-auto-face-backgrounds' face used, if any,
   or the first `hlt-auto-face-backgrounds' face, if not.

 * If `hlt-auto-faces-flag' is nil then FACE is the last face used for
    highlighting.  Remember that you can use command
   `hlt-choose-default-face' to choose a different highlighting face.

If `hlt-act-on-any-face-flag' is non-nil, then the target face can be
any face you choose.  Otherwise, it must be a face that has been used
for highlighting by library `highlight.el'.

With a prefix argument, go to the next `mouse-face' property with
FACE, not the next `face' property.

If `hlt-use-overlays-flag' is non-nil, then overlay highlighting is
targeted.  If `hlt-use-overlays-flag' is not `only', then
text-property highlighting is targeted.  This means, in particular,
that a value of nil targets both overlays and text properties.

If the region is active and not empty, then limit movement to the
region.  Otherwise, use the whole buffer.

When called non-interactively:

 - START and END are the buffer limits: region or whole buffer.

 - non-nil MOUSEP means use `mouse-face' property, not `face'.

 - non-nil NO-ERROR-P means do not raise an error if no highlight with
   FACE is found, and leave point at END.

 - non-nil WRAP-P is generally only for interactive use.  It means if
   the END (START, if BACKWARD-P is non-nil) limit is reached then
   wrap around again, from START (END, if BACKWARD-P is non-nil).

 - Return a cons of the limits of the text starting at point that has
   property `hlt-highlight' of value FACE: (BEGIN-FACE . END-FACE), where
   BEGIN-FACE is point and END-FACE is the first position just after
   value FACE ends."
    (interactive
     `(,@(hlt-region-or-buffer-limits)
         ,(if (not hlt-auto-faces-flag)
              nil                       ; Use `hlt-last-face'.
            (save-excursion
              (when (listp last-nonmenu-event) (mouse-set-point last-nonmenu-event))
              (let* ((face  (get-char-property (point) hlt-face-prop))
                     (face  (if (and (consp face)  (facep (car face)))
                                (car face)
                              face))    ; Use only 1st face at pt.
                     (bg    (if (facep face) (face-background face) (cdr (assq 'background-color face))))
                     (hlt   (equal face (get-char-property (point) 'hlt-highlight)))
                     (bg/f  (or (and hlt (car (member face hlt-auto-face-backgrounds)))
                                (and hlt (car (member bg   hlt-auto-face-backgrounds)))
                                (car hlt-auto-face-backgrounds))))
                (if (facep bg/f)
                    bg/f
                  `((background-color . ,bg/f)
                    (foreground-color . ,hlt-auto-face-foreground))))))
         ,current-prefix-arg nil nil t))
    (unless (and start  end) (let ((start-end  (hlt-region-or-buffer-limits)))
                               (setq start  (car start-end)
                                     end    (cadr start-end))))
    (if face (setq hlt-last-face  face) (setq face  hlt-last-face))
    (when backward-p (setq end  (prog1 start (setq start  end))))
    (let ((face-found  nil)
          (orig-point  (point))
          (pos         start)
          loop-beg)

      (while (and (or wrap-p  (not (if backward-p (bobp) (eobp))))
                  (not (equal face face-found))
                  (not (= pos end)))
        (setq loop-beg  pos)
        (save-restriction
          (narrow-to-region loop-beg end)
          (setq pos  (if backward-p
                         (goto-char (previous-single-char-property-change
                                     (point) (if mousep 'mouse-face hlt-face-prop) nil (point-min)))
                       (goto-char (next-single-char-property-change
                                   (point) (if mousep 'mouse-face hlt-face-prop) nil (point-max))))))
        (when hlt-use-overlays-flag
          (let ((overlays  (overlays-at (point))))
            (while overlays
              (when (and (or hlt-act-on-any-face-flag
                             (equal face (overlay-get (car overlays) 'hlt-highlight)))
                         (equal face (overlay-get (car overlays) hlt-face-prop)))
                (setq face-found  face
                      overlays    ()))
              (when overlays (setq overlays  (cdr overlays))))))
        (when (and (not face-found)
                   (not (eq hlt-use-overlays-flag 'only))
                   (or hlt-act-on-any-face-flag  (equal face (get-char-property (point) 'hlt-highlight)))
                   ;; $$$$$$ (equal face (get-char-property (point) hlt-face-prop)))
                   (let ((pt-faces  (get-char-property (point) hlt-face-prop)))
                     (if (consp pt-faces) (memq face pt-faces) (equal face pt-faces))))
          (setq face-found  face))
        (when (and wrap-p               ; Wrap around.
                   (= pos end)
                   (if backward-p (<= loop-beg start) (> loop-beg start))
                   (not (equal face face-found)))
          (setq pos  start) (goto-char start)))

      (unless (or (and (equal face face-found)  (not (eq (point) orig-point)))  no-error-p)
        (goto-char orig-point)
        (hlt-user-error "No %s highlight with face `%s'" (if backward-p "previous" "next") face)))
    (unless (interactive-p)
      (cons (point)
            (next-single-char-property-change (point) (if mousep 'mouse-face hlt-face-prop)
                                              nil (if backward-p start end)))))


  ;; Suggested binding: `C-S-p'.
  (defun hlt-previous-highlight (&optional start end face mousep no-error-p wrap-p)
    "Go to the previous highlight in the last face used for highlighting.
This is the same as `hlt-previous-highlight', except movement is backward."
    (interactive `(,@(hlt-region-or-buffer-limits) nil ,current-prefix-arg nil t))
    (unless (and start  end) (let ((start-end  (hlt-region-or-buffer-limits)))
                               (setq start  (car start-end)
                                     end    (cadr start-end))))
    (hlt-next-highlight start end face mousep t no-error-p wrap-p))

  (defun hlt-highlight-faces-in-buffer (start end)
    "List of highlighting faces in current buffer between START and END.
This includes faces used in overlays and as text properties.
Only highlighting faces are included, that is, faces associated with a
`hlt-highlight' property."
    (save-excursion
      (save-window-excursion
        (let ((faces  ())
              (beg  start)
              face)
          (setq end  (min end (point-max)))
          (goto-char beg)
          (while (< beg end)
            (save-restriction
              (narrow-to-region beg end)
              (setq beg  (goto-char
                          (next-single-char-property-change (point) hlt-face-prop nil (point-max)))))
            (when (setq face  (get-text-property (point) 'hlt-highlight)) (add-to-list 'faces face))
            (let ((overlays  (overlays-at (point))))
              (while overlays
                (when (and (overlay-get (car overlays) 'hlt-highlight)
                           (setq face  (overlay-get (car overlays) hlt-face-prop)))
                  (add-to-list 'faces face)
                  (setq overlays  ()))
                (when overlays (setq overlays  (cdr overlays))))))
          faces))))

  (defun hlt-toggle-act-on-any-face-flag ()
    "Toggle `hlt-act-on-any-face-flag'."
    (interactive)
    (setq hlt-act-on-any-face-flag  (not hlt-act-on-any-face-flag))
    (message (if hlt-act-on-any-face-flag
                 "Highlight actions now apply to any face, not just a highlighting face"
               "Highlight actions now apply only to a highlighting face")))
  )
 
;;(@* "Functions for Highlighting Propertized Text - Emacs 21+")

;;; Functions for Highlighting Propertized Text - Emacs 21+ ----------

(when (fboundp 'next-single-char-property-change) ; Don't bother, for Emacs 20.
  (defun hlt-highlight-property-with-value (property &optional values start end face type msgp mousep)
    "Highlight region text that has PROPERTY with a value in VALUES.
If VALUES is nil then highlight for *any* non-nil value of PROPERTY.
Otherwise, highlight only where PROPERTY has a value in VALUES.

Interactively, you are prompted for PROPERTY and VALUES.  For VALUES
  you can enter either a list or a single, non-list value.  A list is
  always interpreted as a list of values, not as a single list value.
  Use `RET' with no input to highlight for *any* non-nil value.

With a prefix argument, use the `mouse-face' property with FACE for
highlighting, not the `face' property.

Optional args START and END are the limits of the area to act on.
  They default to the region limits (buffer, if no active region).
Optional 5th arg FACE is the face to use for highlighting.
  Interactively, this is the last face that was used for highlighting.
  (Use command `hlt-choose-default-face' to choose a different face.)
Optional 6th arg TYPE is `overlay', `text', or nil, and specifies the
  type of property - nil means to look for both overlay and text
  properties.  Interactively, TYPE is derived from
  `hlt-use-overlays-flag'.
Optional 7th arg MSGP non-nil means to display a progress message.
Optional 8th arg MOUSEP non-nil means use the `mouse-face' property,
  not the `face' property, for highlighting.  Interactively, MOUSEP
  is provided by the prefix arg."
    (interactive
     `(,(intern (read-string "Property to highlight: " nil 'highlight-property-history))
       ,(let* ((strg  (read-string "Property value: "))
               (vals  (if (string= "" strg) () (car (read-from-string strg)))))
          (unless (listp vals) (setq vals  (list vals)))
          vals)
       ,@(hlt-region-or-buffer-limits)
       nil
       ,(if hlt-use-overlays-flag (if (eq hlt-use-overlays-flag 'only) 'overlay nil) 'text)
       t
       ,current-prefix-arg))
    (unless (and start  end) (let ((start-end  (hlt-region-or-buffer-limits)))
                               (setq start  (car start-end)
                                     end    (cadr start-end))))
    (if face (setq hlt-last-face  face) (setq face  hlt-last-face))
    (when msgp (message "Highlighting%s..." (if mousep " `mouse-face'" "")))
    (let ((zone-end  nil))
      (condition-case highlight-property-with-value-err
          (save-excursion
            (while (and (< start end)
                        (let* ((charval  (and (or (not type)  (eq type 'overlay))
                                              (get-char-property start property)))
                               (textval  (and (or (not type)  (eq type 'text))
                                              (get-text-property start property)))
                               (currval  (hlt-flat-list charval textval)))
                          (if values
                              (not (hlt-set-intersection values currval))
                            (not currval))))
              (setq start  (next-single-char-property-change start property nil end)))
            (while (and start  (< start end))
              (setq zone-end  (or (next-single-char-property-change start property nil end)
                                  end))
              (hlt-highlight-region start zone-end face nil mousep)
              (setq start  zone-end)
              (while (and (< start end)
                          (let* ((charval  (and (or (not type)  (eq type 'overlay))
                                                (get-char-property start property)))
                                 (textval  (and (or (not type)  (eq type 'text))
                                                (get-text-property start property)))
                                 (currval  (hlt-flat-list charval textval)))
                            (if values
                                (not (hlt-set-intersection values currval))
                              (not currval))))
                (setq start  (next-single-char-property-change start property nil end)))))
        (quit (hlt-unhighlight-region start end face))
        (error (hlt-unhighlight-region start end face)
               (error (error-message-string highlight-property-with-value-err)))))
    (let ((remove-msg  (substitute-command-keys
                        "`\\[universal-argument] \\[hlt-highlight]' to remove highlighting")))
      (when msgp (message "Highlighting... done %s" remove-msg))))

  (defun hlt-flat-list (val1 val2)
    "Return a flat list with all values in VAL1 and VAL2."
    (let ((result  ()))
      (unless (listp val1) (setq val1  (list val1)))
      (unless (listp val2) (setq val2  (list val2)))
      (while val1 (add-to-list 'result (pop val1)))
      (while val2 (add-to-list 'result (pop val2)))
      result))

  ;; Use it like this:
  ;; (add-hook 'post-command-hook 'hlt-mouse-toggle-link-highlighting)
  ;;
  (defun hlt-mouse-toggle-link-highlighting ()
    "Toggle highlighting links with a mouse click.
Face `hlt-property-highlight' is used for the highlighting.
Do nothing if the click is at a different location from the last one.
This calls `hlt-toggle-link-highlighting' to do the toggling.
Links in the entire buffer are affected, even if the region is active.
This is intended to be used on `post-command-hook'."
    (when (and (string-match "mouse" (format "%S" (event-basic-type last-command-event)))
               (memq 'click (event-modifiers last-command-event)))
      (let* ((estart  (event-start last-command-event))
             (pos     (copy-marker (posn-point estart))))
        (when (integer-or-marker-p pos)
          (save-excursion
            (with-current-buffer (window-buffer (posn-window estart))
              (when (condition-case nil
                        (get-char-property (min pos (point-max)) 'mouse-face)
                      (error nil))
                (hlt-toggle-link-highlighting nil nil pos))))))))

  (defun hlt-toggle-link-highlighting (&optional start end pos)
    "Toggle highlighting links with face `hlt-property-highlight'.
A link is considered to be any text with property `mouse-face'.
Calls `hlt-toggle-property-highlighting', passing the args."
    (interactive `(,@(hlt-region-or-buffer-limits)))
    (hlt-toggle-property-highlighting 'mouse-face start end 'hlt-property-highlight (interactive-p) nil pos))

  ;; Use it like this:
  ;; (add-hook 'post-command-hook (lambda () (hlt-mouse-toggle-property-highlighting myprop myface)
  ;;
  (defun hlt-mouse-toggle-property-highlighting (property &optional face msgp mousep)
    "Toggle highlighting text that has PROPERTY, with a mouse click.
Face `hlt-property-highlight' is used for the highlighting.
Do nothing if the click is at a different location from the last one.
Propertized text in the entire buffer is (un)highlighted, even if the
region is active.

This calls `hlt-toggle-link-highlighting', passing the args.
This is intended to be used on `post-command-hook'."
    (when (and (string-match "mouse" (format "%S" (event-basic-type last-command-event)))
               (memq 'click (event-modifiers last-command-event)))
      (let* ((estart  (event-start last-command-event))
             (pos     (copy-marker (posn-point estart))))
        (when (integer-or-marker-p pos)
          (save-excursion
            (with-current-buffer (window-buffer (posn-window estart))
              (when (condition-case nil
                        (get-char-property (min pos (point-max)) property)
                      (error nil))
                (hlt-toggle-property-highlighting property nil nil face (interactive-p) mousep pos))))))))

  (defun hlt-toggle-property-highlighting (property &optional start end face msgp mousep pos)
    "Toggle highlighting of all text that has PROPERTY.
Highlighting is done using overlays.
With a prefix arg, highlight using property `mouse-face' not `face'.

Optional arg POS is a buffer position.  If it is the same as the
  position recorded in `hlt-prop-highlighting-state', then do not
  toggle.  In any case, update `hlt-prop-highlighting-state' with POS.
Other args are the same as for `hlt-highlight-property-with-value'."
    (interactive `(,(intern (read-string "Property to highlight: " nil 'highlight-property-history))
                   ,@(hlt-region-or-buffer-limits)
                   nil  t  ,current-prefix-arg))
    (when (or (not pos)  (equal pos (cdr hlt-prop-highlighting-state)))
      (cond ((car hlt-prop-highlighting-state)
             (hlt-unhighlight-all-prop property start end face (interactive-p) mousep)
             (setcar hlt-prop-highlighting-state  nil))
            (t
             (hlt-highlight-all-prop property start end face (interactive-p) mousep)
             (setcar hlt-prop-highlighting-state t))))
    (when pos (setcdr hlt-prop-highlighting-state  pos)))

  (defun hlt-highlight-all-prop (property &optional start end face msgp mousep)
    "Highlight all text that has PROPERTY (any value), using FACE.
Highlight using only overlays.
Args are the same as for `hlt-highlight-property-with-value'."
    (interactive `(,(intern (read-string "Property to highlight: " nil 'highlight-property-history))
                   ,@(hlt-region-or-buffer-limits)))
    (hlt-highlight-property-with-value property () start end face 'overlay (interactive-p) mousep))

  (defun hlt-unhighlight-all-prop (prop &optional start end face msgp mousep)
    "Unhighlight all text highlighted with face `hlt-property-highlight'.
Args are the same as for `hlt-highlight-property-with-value'."
    (interactive `(,@(hlt-region-or-buffer-limits)))
    (let ((hlt-use-overlays-flag  'only)) (hlt-unhighlight-region-for-face face start end mousep)))

  )
 
;;(@* "Functions for Highlighting Isearch Matches")

;;; Functions for Highlighting Isearch Matches -----------------------

;;;###autoload
(defun hlt-highlight-isearch-matches (&optional face msgp mousep buffers string)
  "Highlight matches of the (nonempty) Isearch search pattern using FACE.
If the region is active then it limits highlighting.  If inactive then
highlight matches throughout the buffer, or the list of BUFFERS.  If
this is accessed from a `multi-search' command then the BUFFERS are
the buffers being searched.

With no prefix arg:
 * If `hlt-auto-faces-flag' is nil then use the last highlighting face
   used or chosen with command `hlt-choose-default-face'.
 * If non-nil then use the next highlighting face.

With a non-negative prefix arg, prompt for the face to use.
With a non-positive prefix arg, use `mouse-face' instead of `face'.

To use a prefix argument you must set either `isearch-allow-scroll' or
`isearch-allow-prefix' (if available) to non-nil.  Otherwise, a prefix
arg during Isearch exits Isearch.

If invoked outside of Isearch, use the last Isearch search pattern or,
if none, prompt for the pattern to match

Non-interactively:
 FACE is the face to use for highlighting.
 Non-nil MOUSEP means use `mouse-face' property, not `face'.
 BUFFERS are the buffers to act on.
 STRING is the search string to use."
  (interactive
   (list (if (and current-prefix-arg  (>= (prefix-numeric-value current-prefix-arg) 0))
             (let (fac)
               ;; This is better than the vanilla Emacs approach used for `isearch-highlight-regexp'
               ;; Because this lets you continue searching after highlighting.
               (with-isearch-suspended (setq fac  (call-interactively #'hlt-choose-default-face)))
               fac)
           (if hlt-auto-faces-flag (hlt-next-face) hlt-last-face))
         t
         (and current-prefix-arg  (<= (prefix-numeric-value current-prefix-arg) 0))
         ()
         (or (and (not (equal "" isearch-string))  isearch-string)
             (read-string "Highlight string: "))))
  (let ((bufs                   (or buffers  (and (boundp 'multi-isearch-buffer-list)
                                                  multi-isearch-buffer-list)))
        (regexp                 (cond ((functionp isearch-word) (funcall isearch-word string))
                                      (isearch-word             (word-search-regexp string))
                                      (isearch-regexp string)
                                      ((if (and (eq isearch-case-fold-search t)  search-upper-case)
                                           (isearch-no-upper-case-p string isearch-regexp)
                                         isearch-case-fold-search)
                                       ;; Turn STRING into a case-insensitive regexp.
                                       (mapconcat (lambda (c)
                                                    (let ((s  (string c)))
                                                      (if (string-match "[[:alpha:]]" s)
                                                          (format "[%s%s]" (upcase s) (downcase s))
                                                        (regexp-quote s))))
                                                  string ""))
                                      (t (regexp-quote string))))
        (hlt-overlays-priority  1002)) ; Higher than Isearch's 1000 priority.
    (hlt-+/--highlight-regexp-region nil nil nil regexp face msgp mousep nil bufs)))

;;;###autoload
(defun hlt-unhighlight-isearch-matches (&optional face msgp mousep buffers string)
  "Unhighlight matches of the current (nonempty) Isearch search pattern.
With no prefix arg, unhighlight all faces highlighted by `hlt-*'.
With a non-negative prefix arg, prompt for the face to unhighlight.
With a non-positive prefix arg, use `mouse-face' instead of `face'.
With any other prefix arg, unhighlight the last highlighting face used
 or chosen with command `hlt-choose-default-face'.
 (`hlt-auto-faces-flag' has no effect.)

If invoked outside of Isearch, use the last Isearch search pattern or,
if none, prompt for the pattern to match.

To use a prefix argument you must set either `isearch-allow-scroll' or
`isearch-allow-prefix' (if available) to non-nil.  Otherwise, a prefix
arg during Isearch exits Isearch.

Non-interactively, args are as for `hlt-highlight-isearch-matches',
except that nil FACE means unhighlight all faces."
  (interactive
   (let ((bufs  (and (boundp 'multi-isearch-buffer-list)  multi-isearch-buffer-list)))
     (list (and current-prefix-arg
                (if (>= (prefix-numeric-value current-prefix-arg) 0)
                    (let (fac)
                      (with-isearch-suspended (setq fac  (call-interactively #'hlt-choose-default-face)))
                      fac)
                  hlt-last-face))
           t
           (and current-prefix-arg  (<= (prefix-numeric-value current-prefix-arg) 0))
           bufs
           (or (and (not (equal "" isearch-string))  isearch-string)
               (read-string "Highlight string: ")))))
  (hlt-+/--highlight-regexp-region t nil nil string face msgp mousep nil buffers))

(define-key isearch-mode-map (kbd "M-s h h") 'hlt-highlight-isearch-matches)
(define-key isearch-mode-map (kbd "M-s h u") 'hlt-unhighlight-isearch-matches)

(when (boundp 'search-map)              ; Emacs 23+
  (define-key search-map (kbd "M-s h h") 'hlt-highlight-isearch-matches)
  (define-key search-map (kbd "M-s h u") 'hlt-unhighlight-isearch-matches))
 
;;(@* "General and Utility Functions")

;;; General and Utility Functions

;; Same as `tap-string-match-p' in `thingatpt+.el' and  `bmkp-string-match-p' in `bookmark+-bmu.el'.
(if (fboundp 'string-match-p)
    (defalias 'hlt-string-match-p 'string-match-p) ; Emacs 23+
  (defun hlt-string-match-p (regexp string &optional start)
    "Like `string-match', but this saves and restores the match data."
    (save-match-data (string-match regexp string start))))

;; Similar to `region-or-buffer-limits' in `misc-fns.el', but takes arg BUFFERS.
(defun hlt-region-or-buffer-limits (&optional buffer)
  "Return the start and end of the region as a list, smallest first.
If the region is empty or not active, then bob and eob are used.
Optional arg BUFFER is the buffer to use.  Defaults to current buffer."
  (with-current-buffer (or buffer  (current-buffer))
    (if (not (hlt-nonempty-region-p))
        (list (point-min) (point-max))
      (if (< (point) (mark)) (list (point) (mark)) (list (mark) (point))))))

(defun hlt-+/--highlight-regexp-read-args (un regionp)
  "Read arguments for `hlt-(un)highlight-regexp-(region|to-end)'.
See `hlt-+/--read-regexp' for arguments."
  `(,@(and regionp  (hlt-region-or-buffer-limits))
    ,(hlt-+/--read-regexp un regionp)
    nil
    t
    ,current-prefix-arg))

(defun hlt-+/--read-regexp (un regionp)
  "Read regexp for (un)highlight regexp commands.
Arguments affect the prompt as follows:
 Unhighlight if UN is \"UNh\", highlight if \"\".
 Highlight in region if REGIONP is non-nil, after cursor if nil."
  (let ((prompt  (format "Regexp to %shighlight%s: " un (if regionp "" " after cursor"))))
    (if (fboundp 'icicle-read-string-completing)
        (icicle-read-string-completing prompt hlt-last-regexp
                                       (lambda (c) (string-match "regexp" (symbol-name c)))
                                       (if (and (boundp 'hi-lock-mode)  hi-lock-mode)
                                           'hi-lock-regexp-history
                                         'regexp-history))
      (read-string prompt nil (if (and (boundp 'hi-lock-mode)  hi-lock-mode)
                                  'hi-lock-regexp-history
                                'regexp-history)
                   hlt-last-regexp))))

(defun hlt-+/--read-bufs (&optional un)
  "Read names of buffers to highlight, one at a time.
`C-g' ends reading.
Non-nil optional arg UN means prompt to unhighlight; else highlight."
  (let ((bufs  ())
        buf)
    (while (condition-case nil
               (setq buf  (read-buffer (format "%sighlight in buffer (C-g to end): " (if un "UNh" "H"))
                                       (and (not (member (buffer-name (current-buffer)) bufs))
                                            (current-buffer))
                                       t))
             (quit nil))
      (push buf bufs))
    (delq nil (mapcar #'get-buffer (nreverse bufs)))))

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'highlight)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; highlight.el ends here

