;;; bookmark+-doc.el - Documentation and change history for library `bookmark+.el'.
;;
;; Filename: bookmark+-doc.el
;; Description: Documentation and change history for library `bookmark+.el'.
;; Author: Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 2000-2010, Drew Adams, all rights reserved.
;; Created: Fri Sep 15 07:58:41 2000
;; Last-Updated: Sat May 22 23:55:16 2010 (-0700)
;;           By: dradams
;;     Update #: 12218
;; URL: http://www.emacswiki.org/cgi-bin/wiki/bookmark+-doc.el
;; Keywords: bookmarks, placeholders, annotations, search, info, w3m, gnus
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Documentation and change history for Bookmark+ (library
;;    `bookmark+.el'), which provides extensions to standard library
;;    `bookmark.el'.
;;
;;    This documentation is also available in these ways:
;;
;;    1. From the bookmark list (`C-x r l'):
;;       Use `?' to show the current bookmark-list status and general
;;       help, then click link `Doc in Commentary' or link `Doc on the
;;       Web'.
;;
;;    2. From the Emacs-Wiki Web site:
;;       http://www.emacswiki.org/cgi-bin/wiki/BookmarkPlus.
;;    
;;    3. From the Bookmark+ group customization buffer:
;;       `M-x customize-group bookmark-plus', then click link
;;       `Commentary'.
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
;;  http://dto.freeshell.org/notebook/Linkd.html.
;;
;;  (@> "Documentation")
;;    (@> "Installing Bookmark+")
;;    (@> "Bookmark+ Features")
;;    (@> "Different Types of Jump Commands")
;;    (@> "Bookmark Tags")
;;    (@> "Bookmark Tags Can Have Values")
;;    (@> "Function, Sequence, and Variable-List Bookmarks")
;;    (@> "Bookmark-List Views - Saving and Restoring State")
;;      (@> "Quitting Saves the Bookmark-List State")
;;      (@> "State-Restoring Commands and Bookmarks")
;;    (@> "Bookmark List (Display)")
;;      (@> "Tag Commands and Keys")
;;      (@> "Sets of Bookmarks")
;;      (@> "Open Dired for the Marked Files")
;;      (@> "Marking and Unmarking Bookmarks")
;;      (@> "Filtering Bookmarks (Hiding and Showing)")
;;      (@> "Only Visible Bookmarks Are Affected")
;;      (@> "Omitting Bookmarks from Display")
;;      (@> "Sorting Bookmarks")
;;    (@> "Bookmarks for Specific Files or Buffers")
;;    (@> "Use Bookmark+ with Icicles")
;;    (@> "Open Bookmarks Using Windows File Associations")
;;    (@> "Bookmark Compatibility with Vanilla Emacs (`bookmark.el')")
;;    (@> "New Bookmark Structure")
;;  (@> "Change log")
 
;;(@* "Documentation")
;;
;;  Documentation
;;  -------------
;;
;;(@* "Installing Bookmark+")
;;  ** Installing Bookmark+ **
;;
;;  Put library `bookmark+.el' in your `load-path'.
;;  Add this to your init file (~/.emacs) : (require 'bookmark+)
;;
;;
;;(@* "Bookmark+ Features")
;;  ** Bookmark+ Features **
;;
;;  Here is an overview of the features that Bookmark+ provides.  Some
;;  of these are detailed in other sections, below.
;;
;;  * Richer bookmarks.  They record more.  They are more accurate.
;;
;;     - You can tag bookmarks, a la del.icio.us.  This is perhaps the
;;       most important Bookmark+ feature.  In effect, tags define
;;       bookmark sets.  A bookmark can have any number of tags, and
;;       multiple bookmarks can have the same tag.  You can mark or
;;       show just the bookmarks with a given tag or a set of tags.
;;
;;     - Bookmark tags can in fact be more than just names.  They can
;;       be full-fledged user-defined attributes, with Emacs-Lisp
;;       objects as their values.
;;
;;     - Bookmarks record the number of visits and the time of the
;;       last visit.  You can sort, show/hide, or mark bookmarks based
;;       on this info.
;;
;;     - You can combine bookmarks, to make composite, or sequence,
;;       bookmarks.  Invoking a sequence bookmark invokes each of its
;;       component bookmarks in turn.  A component bookmark can itself
;;       be a sequence bookmark.
;;
;;     - You can bookmark a region of text, not just a position.  When
;;       you jump to a bookmark that records a region, the region is
;;       activated (see option `bookmarkp-use-region-flag').  (Region
;;       activation is not supported for Gnus bookmarks.)
;;
;;     - Bookmarks are relocated better than for vanilla Emacs when
;;       the contextual text changes.
;;
;;  * Additional types of bookmarks.
;;
;;     - You can bookmark a Dired buffer, recording and restoring its
;;       `ls' switches, which files are marked, which subdirectories
;;       are inserted, and which (sub)directories are hidden.
;;
;;     - You can bookmark a buffer that is not associated with a file.
;;
;;     - You can bookmark the current desktop, as defined by library
;;       `desktop.el' - use command `bookmarkp-set-desktop-bookmark'
;;       (`C-x p K').  You can "jump" to (that is, restore) a saved
;;       desktop.  A desktop includes:
;;
;;         - Some global variables.  To exclude variables normally
;;           saved, see option `bookmarkp-desktop-no-save-vars'.
;; 	   - The current set of buffers and their associated files.
;;           For each: its mode, point, mark, & some local variables.
;;
;;     - You can bookmark a Gnus article, a URL (if you use W3M), a
;;       PDF file (DocView), an image, or a UNIX manual page (from the
;;       output of Emacs command `man' or `woman').
;;
;;     - A bookmark can represent a function, which is invoked when
;;       you "jump" to the bookmark.
;;
;;     - As mentioned above, a bookmark can represent a sequence of
;;       other bookmarks.
;;
;;     - A bookmark can represent a set of variables and their values.
;;
;;     - You can bookmark buffer `*Bookmark List*' itself.  Jumping to
;;       such a bookmark restores the recorded sort order, filter,
;;       title, and omit list
;;       (see (@> "Omitting Bookmarks from Display")).
;;
;;  * Type-specific jump commands.
;;
;;     - When you want to jump to a bookmark of a specific type
;;       (e.g. Dired), you can use a command that offers only such
;;       bookmarks as completion candidates.
;;
;;  * Improvements for the bookmark list (buffer `*Bookmark List*',
;;    aka "menu list") that is displayed using `C-x r l'.
;;
;;     - The last display state is saved (by default), and is restored
;;       the next time you show the list.  (Tip: Use the bookmark list
;;       as your "Home" page at Emacs startup.)
;;
;;     - Marking/unmarking is enhanced.  It is similar to Dired's.
;;
;;     - You can easily mark or show different classes of bookmarks.
;;
;;     - Faces distinguish bookmarks by type.
;;
;;     - You can sort bookmarks in many ways.  You can easily define
;;       your own sort orders, even complex ones.
;;
;;     - You can regexp-search (`M-a') or query-replace (`M-q') the
;;       targets (destination file or buffers) of the marked
;;       bookmarks, in the current bookmark-list sort order.  For
;;       Emacs 23 and later, you can even search incrementally (`M-s a
;;       C-s', or `M-s a C-M-s' for regexp).
;;
;;     - You can save the current bookmark-list state and return to it
;;       later.  There are a few ways to do this, including
;;       bookmarking the list itself.
;;       See (@> "Bookmark-List Views - Saving and Restoring State").
;;
;;     - You can use `M-d >' to open Dired for just the local file and
;;       directory bookmarks that are marked (`>').
;;
;;     - If you use Emacs on Microsoft Windows, you can open bookmarks
;;       according to Windows file associations.  (You will also need
;;       library `w32-browser.el'.)
;;
;;     - You can use (lax) completion when you set a bookmark using
;;       `bookmark-set' (`C-x r m'), choosing from existing bookmarks
;;       for the same buffer.  This makes it easy to update a nearby
;;       bookmark (e.g. relocate it).  With a numeric prefix argument
;;       (or if there are no bookmarks for the buffer), you can choose
;;       from all bookmarks.
;;
;;     - You can edit a bookmark (its name and file name/location).
;;
;;     - A popup menu is available on `mouse-3', with actions for the
;;       individual bookmark you point to when you click the mouse.
;;
;;     - A complete menu, `Bookmark+', is provided on the menu-bar.
;;       Use it, in particular, when you don't remember a key binding.
;;       The same menu is available on `C-mouse-3'.
;;
;;     - The vanilla `Bookmarks' menu, which is typically a submenu of
;;       the `Edit' menu-bar menu, is modified by adding several items
;;       from the `Bookmark+' menu, including the `Jump To' submenu
;;       (called `Jump To Bookmark' there).
;;
;;  * Multiple bookmark files.
;;
;;     - Although vanilla Emacs lets you load different bookmark
;;       files, this feature is not well supported, if not
;;       contradictory.  With Bookmark+ you can easily switch among
;;       alternative bookmark files or load multiple files into the
;;       same session, accumulating their bookmark definitions.
;;
;;  * Dedicated prefix keys.
;;
;;     - Prefix `C-x p' is used for bookmark keys, in general.  The
;;       vanilla keys on prefix `C-x r' are still available also, but
;;       that prefix is shared with register commands, making it less
;;       convenient for bookmarks.  Using `C-x p' lets you focus on
;;       bookmarks.
;;
;;     - Prefixes `C-x j' and `C-x 4 j' (for other-window) are used
;;       for bookmark jump commands.  Again, a dedicated prefix key
;;       helps you focus on one kind of action (jumping).
;;
;;  * Helpful help.
;;
;;     - Information about individual bookmarks.
;;
;;       . Anywhere in Emacs, `C-x p ?'  (command
;;         `bookmarkp-describe-bookmark') describes any bookmark.
;;         With a prefix argument, it shows you the full information
;;         that defines it (internal form).
;;
;;       . In the bookmark list, `C-h RET' (or `C-h C-RET') describes
;;         the bookmark under the cursor.  Again, a prefix arg means
;;         show the full (internal) information.
;;
;;     - General Bookmark+ documentation.
;;
;;       . Anywhere in Emacs, `M-x bookmarkp-bmenu-mode-status-help'
;;         shows detailed information about the current state of the
;;         bookmark list.  Click button `Doc in Commentary' or button
;;         `Doc on the Web' to access the complete documentation.
;;
;;         (Use button `Customize' to customize all '''Bookmark+'''
;;         faces and options.)
;;
;;       . In the bookmark list, `?' and `C-h m' are the same as `M-x
;;         bookmarkp-bmenu-mode-status-help'.  (`C-h m' in the
;;         bookmark list does not show you info about minor modes.  If
;;         you want that, use `M-x describe-mode'.)
;;
;;       . In the `bookmark-plus' group customization buffer (`M-x
;;         customize-group bookmark-plus'), click button `Commentary'.
;;
;;       . From the Emacs-Wiki Web site,
;;         http://www.emacswiki.org/cgi-bin/wiki/BookmarkPlus.
;;    
;;  * Synergy with Icicles.
;;
;;     - Icicles works with Bookmark+ to provide enhanced bookmark
;;       jumping (visiting), setting, and help.  It gives you a
;;       bookmark browser.  See (@> "Use Bookmark+ with Icicles") and
;;       http://www.emacswiki.org/cgi-bin/wiki/Icicles.
;;
;;
;;(@* "Different Types of Jump Commands")
;;  ** Different Types of Jump Commands **
;;
;;  When you jump to a bookmark, you can use completion to specify the
;;  bookmark name.  With Bookmark+ you can easily have a large number
;;  of bookmarks.  If you want to jump to a bookmark of a specific
;;  type, such as Info, you can use a Bookmark+ command that is
;;  specific to bookmarks of that type: only those bookmarks are
;;  completion candidates.
;;
;;  Commands `bookmarkp-jump-to-type' and
;;  `bookmarkp-jump-to-type-other-window' prompt you first for the
;;  type of bookmark you want to jump to, then for a bookmark of that
;;  type (only).
;;
;;  In addition to these general commands, there are type-specific
;;  commands: `bookmarkp-dired-jump', `bookmarkp-info-jump', and so
;;  on.  And there are commands to jump to bookmarks for the current
;;  buffer or for particular buffers or files
;;  (see (@> "Bookmarks for Specific Files or Buffers")).  All jump
;;  commands are bound to keys that have the prefix `C-x j'.
;;  `bookmarkp-dired-jump' is bound to `C-x j d',
;;  `bookmarkp-info-jump' to `C-x j i', and so on.
;;
;;  There are several commands for jumping to a bookmark with tags.
;;  The completion candidates can be those bookmarks that have all
;;  tags in a given set, some tags in a given set, all tags matching a
;;  regexp, or some tags matching a regexp.  You are prompted for the
;;  set of tags or the regexp to match.  These commands all have the
;;  prefix key `C-x j t', with the regexp-matching ones having the
;;  prefix key `C-x j t %'.  The key suffix is `*' for "all" and `+'
;;  for "some".  For example, `C-x j t % +' jumps to a bookmark you
;;  choose that has one or more tags that match the regexp you input.
;;
;;  There is an other-window version of most jump commands, and it is
;;  bound to the same key as the same-window command, except the
;;  prefix is `C-x 4 j', not `C-x j'.  For instance,
;;  `bookmarkp-dired-jump-other-window' is bound to `C-x 4 j d'.
;;
;;  These round out the jump-command prefix keys:
;;
;;    C-x j j    - bookmark-jump
;;    C-x j :    - bookmarkp-jump-to-type
;;
;;    C-x 4 j j  - bookmark-jump-other-window
;;    C-x 4 j :  - bookmarkp-jump-to-type-other-window
;;
;;  The `C-x j' and `C-x 4 j' bindings are global.  In addition, in
;;  some modes `j' is bound to the corresponding type-specific jump
;;  command.  For example, in Info mode, `j' is bound to
;;  `bookmarkp-info-jump'.  (Dired is an exception here: `J' is used
;;  instead of `j', since `j' is already taken for `dired-goto-file'.)
;;  These commands are also added to the mode's menu-bar menu.
;;
;;  In Dired mode, `C-j' is bound to a special Dired-specific jump
;;  command, `bookmarkp-dired-jump-current', whose destinations all
;;  use the current Dired directory.  The aim of `C-j' is not to
;;  change directories but to change to a different set of markings,
;;  switches, inserted subdirectories, or hidden subdirectories for
;;  the same Dired directory.
;;
;;  Finally, in addition to the predefined bookmark types, which you
;;  can use as described above, you can define a "type"-specific jump
;;  command for any set of bookmarks.  That is, you can use any
;;  specific set of bookmarks as the completion candidates for a new
;;  jump command.  Such a set is really only a pseudo-type: the actual
;;  bookmarks can each be of any type.
;;
;;  You could use this feature, for example, to define a jump command
;;  for the bookmarks that belong to a given project.
;;
;;  To define such a command, you first mark the bookmarks that you
;;  want to be the completion candidates, then you use `M-c' (command
;;  `bookmarkp-bmenu-define-jump-marked-command') in the bookmark
;;  list.
;;
;;
;;(@* "Bookmark Tags")
;;  ** Bookmark Tags **
;;
;;  With Bookmark+ you can bookmark several kinds of Emacs object.
;;  Bookmarks record locations - that is their primary purpose.  They
;;  can also record annotations: general free-text descriptions of
;;  your choosing.
;;
;;  Bookmark+ bookmarks can also be tagged, in del.icio.us style, as a
;;  way to organize them, which also means as a way to organize the
;;  objects that are bookmarked.  A bookmark tag is a string that
;;  contains no newline characters.
;;
;;  You can add as many tags as you like to any bookmark, and multiple
;;  bookmarks can have the same tag(s).  In fact, that's the whole
;;  idea behind using them for organizing.
;;
;;  This feature is unrelated to the fact that bookmarks record
;;  locations and are useful for navigating.  You can, if you want,
;;  use bookmarks to tag files in various ways purely for purposes of
;;  organizing them (e.g. into projects), whether or not you ever use
;;  the bookmarks as a way to visit them.
;;
;;  For example, if you use Dired+ (library `dired+.el'), then you can
;;  use `M-b' (`diredp-do-bookmark') in Dired to create a bookmark for
;;  each of the marked files in the Dired buffer.  Even if you never
;;  use those bookmarks for navigating to the files, you can use them
;;  with tags to organize the files.  See also
;;  (@> "Open Bookmarks Using Windows File Associations").
;;
;;  To make tags more useful, Bookmark+ provides lots of commands:
;;  commands for adding or removing tags, and for marking or unmarking
;;  bookmarks that are tagged in various ways.  When combined with
;;  other Bookmark+ commands (e.g. search, query-replace) that apply
;;  to the marked bookmarks in the `*Bookmark List*' window, you can
;;  really do quite a lot using bookmark tags.  Use your imagination!
;;  See (@> "Tag Commands and Keys") for more about this.
;;
;;
;;(@* "Bookmark Tags Can Have Values")
;;  ** Bookmark Tags Can Have Values **
;;
;;  Bookmark tags are simply names (strings) when you create them.
;;  Nearly all of the predefined operations that use tags use these
;;  names: sorting, marking, and so on.  But you can in fact add an
;;  associated value to each tag.  This means that a tag can act just
;;  like an object attribute or property: it can be a name/value pair.
;;
;;  To add a value to a tag that has none, or to change the current
;;  value of a tag, you use command `bookmarkp-set-tag-value', bound
;;  to `T v' in the bookmark list.  You are prompted for the bookmark,
;;  the tag, and the new value.
;;
;;  A tag value can be a number, symbol, string, list, vector, and so
;;  on.  It can be as complex as you like.  It can be any Emacs-Lisp
;;  object that has read syntax, that is, that is readable by the Lisp
;;  reader.  (Everything that is saved as part of a bookmark must be
;;  readable; otherwise, your bookmark file could not be read
;;  (loaded).)
;;
;;  Because tag values can be pretty much anything, you are pretty
;;  much on your own when it comes to making use of them.  Bookmark+
;;  does not provide predefined functions for using tag values.  In
;;  general, tag values are something you will use with home-grown
;;  Lisp code for your own purposes.
;;
;;  However, you can easily make some interactive use of tag values
;;  with little effort.  You can, for example, define a predicate that
;;  tests whether a bookmark has a tag value that satisfies some
;;  property (e.g. is a number greater than 3.14159265358979), and
;;  then you can use command
;;  `bookmarkp-bmenu-mark-bookmarks-satisfying' to mark those
;;  bookmarks.
;;
;;  Tags that have the prefix "bookmarkp-" are reserved - do not name
;;  your own tags using this prefix.
;;
;;  Currently, "bookmarkp-jump" is the only predefined bookmark tag.
;;  You can give this tag a value that is a function - it is called
;;  whenever the tagged bookmark is visited.  Any Lisp-readable
;;  function value is allowed: a symbol or a lambda expression.
;;
;;  For example, to display `Hello!' when a bookmark is visited you
;;  can use this:
;;
;;    T v bookmark-jump RET (lambda () (message "Hello!"))
;;
;;  The function that is the value of a "bookmarkp-jump" tag is called
;;  just after the the standard hook `bookmark-after-jump-hook' is
;;  invoked.  You can use this tag to invoke functions that are
;;  specific to individual bookmarks; bookmarks can thus have their
;;  own, extra jump functions.
;;
;;
;;(@* "Function, Sequence, and Variable-List Bookmarks")
;;  ** Function, Sequence, and Variable-List Bookmarks **
;;
;;  Bookmarks are typically thought of only as recorded locations.
;;  Invoking a bookmark, called "jumping" to it, traditionally means
;;  just visiting its location.  Bookmark+ looks at bookmarks in a
;;  more general way than that.  A bookmark is a shortcut of some
;;  kind - nothing more.
;;
;;  A given type of bookmark is defined by its handler function, which
;;  really could do anything you like.  We've already seen the
;;  examples of region bookmarks, which restore the active region, and
;;  Dired bookmarks, which restore a set of Dired markings, switches,
;;  inserted subdirectories, and hidden (sub)directories.
;;
;;  A "function bookmark" simply invokes some function - any function.
;;  You can, for instance, define a window or frame configuration and
;;  record that as a bookmark.  Then jump to the bookmark to switch
;;  contexts.  (You can also bookmark a desktop and jump to that.)
;;
;;  Function bookmarks might not seem too interesting, since we have
;;  other ways of invoking functions in Emacs.  But the other features
;;  of Bookmark+ combine with this feature.  You can, for instance,
;;  tag such bookmarks.
;;
;;  And you can combine them, invoking the functions sequentially.
;;  This is just a particular case of using a "sequence bookmark",
;;  which simply records a sequence of bookmarks.  The bookmarks in a
;;  sequence can be of any kind, including other sequence bookmarks.
;;
;;  Command `bookmarkp-make-function-bookmark' creates a function
;;  bookmark - you give it a function symbol or a lambda expression.
;;  Command `bookmarkp-bmenu-make-sequence-from-marked' creates a
;;  sequence from the marked bookmarks in the bookmark list, in their
;;  current order.
;;
;;  A variable-list bookmark saves and restores the values of a set of
;;  variables.  Command `bookmarkp-set-varlist-bookmark' prompts you
;;  for the variables to include in the list and then sets the
;;  bookmark.  Command `bookmarkp-jump-varlist' (`C-x j v') restores
;;  the recorded variable values for the bookmark's buffer.  You can
;;  also create varlist bookmarks non-interactively, using function
;;  `bookmarkp-create-varlist-bookmark'.
;;
;;  If you use library `wide-n.el', then you can move among multiple
;;  restrictions (narrowings) in a buffer.  The restrictions are
;;  stored in buffer-local variable `wide-n-restrictions'.  Command
;;  `bookmarkp-set-restrictions-bookmark' bookmarks this value for the
;;  current buffer.  Jumping to such a bookmark restores the saved
;;  ring/stack of restrictions.
;;
;;
;;(@* "Bookmark-List Views - Saving and Restoring State")
;;  ** Bookmark-List Views - Saving and Restoring State **
;;
;;  The bookmark list (buffer `*Bookmark List*') provides a view into
;;  the set of bookmarks.  You can mark, sort, and hide (filter, omit)
;;  bookmarks - see (@> "Bookmark List (Display)").  The state of the
;;  displayed bookmark list can thus change.
;;
;;  At different times, and in different contexts, different views can
;;  be useful.  Bookmark+ lets you save the current state of the
;;  displayed list and later restore it.  There are a couple of
;;  different ways to do this.
;;
;;
;;(@* "Quitting Saves the Bookmark-List State")
;;  *** Quitting Saves the Bookmark-List State ***
;;
;;  If option `bookmarkp-bmenu-state-file' is non-nil, which it is by
;;  default, then Bookmark+ remembers the last state of the bookmark
;;  list when you quit it or you quit Emacs, and it restores that
;;  state when you show the list again (which could be in the next
;;  Emacs session).  You can think of this feature as your "Home" page
;;  for bookmarks, giving you a stepping stone to the files and
;;  directories you use most.
;;
;;  If, for example, when you quit the bookmark list you are showing
;;  only bookmarks to Info nodes and UNIX manual pages, sorted in a
;;  particular way, and with some of them marked for particular
;;  processing, then the next time you open the list the same state is
;;  restored: the same set of bookmarks is shown, in the same order,
;;  with the same markings.
;;
;;  You can turn off this automatic bookmark-list display state
;;  saving, if you want, by customizing option
;;  `bookmarkp-bmenu-state-file' to nil.  And you can toggle this
;;  option at any time, using `M-l' in the bookmark list (command
;;  `bookmarkp-toggle-saving-menu-list-state').  In particular, if you
;;  want your next visit to the bookmark list to start out with a
;;  previously recorded state instead of the current state, just hit
;;  `M-l' before quitting the bookmark list.
;;
;;
;;(@* "State-Restoring Commands and Bookmarks")
;;  *** State-Restoring Commands and Bookmarks ***
;;
;;  In addition to automatically saving/restoring the final
;;  bookmark-list display state, you can save this state at any time,
;;  any number of times, for later restoration.  This gives you the
;;  ability to create multiple persistent views of your bookmarks.
;;
;;  There are two ways to do this:
;;
;;  * Create a bookmark for the `*Bookmark List*' buffer itself.
;;  * Define a command that restores the bookmark-list state.
;;
;;  When you use `C-x r m' (`bookmark-set') in buffer `*Bookmark
;;  List*' to create a bookmark, the current sort order, filter,
;;  title, and omit list are saved as part of the bookmark.  (These
;;  concepts are described below -
;;  see (@> "Bookmark List (Display)").)  Jumping to such a bookmark
;;  restores all of these.
;;
;;  Alternatively, you can define a command that does the same thing,
;;  but without creating another bookmark - use `c'
;;  (`bookmarkp-bmenu-define-command') in the bookmark list to do
;;  this.  You are prompted for the name of the new command.  Use the
;;  command anytime (including in another Emacs session) to restore
;;  the bookmark list.
;;
;;  Define any number of such commands for the views you use.  The
;;  file for saving the definitions (see option
;;  `bookmarkp-bmenu-commands-file') is never overwritten, so you can
;;  also add other code to it manually, if you want.  The file is read
;;  the first time the bookmark list is displayed in a given Emacs
;;  session.
;;
;;  The state that is saved and restored using a bookmark-list
;;  bookmark or a command defined using `c' is only a partial state.
;;  The current set of markings and some other information are not
;;  saved, in order to save disk space and save/restore time.
;;
;;  Sometimes, however, you really want to save the entire
;;  bookmark-list state, creating a full snapshot.  You can use `C'
;;  (`bookmarkp-bmenu-define-full-snapshot-command') to do that.  This
;;  defines a command that restores the bookmark list completely.
;;  That is the same thing that happens automatically (by default)
;;  whenever you quit the bookmark list (or Emacs), but defining
;;  snapshot commands lets you have multiple saved states and switch
;;  to them at will.
;;
;;  Be aware, however, that full-snapshot command definitions can be
;;  quite large, since they each contain a copy of the current
;;  bookmark list and any accessory lists (hidden and marked bookmarks
;;  etc.).
;;
;;  Whether you use `c' or `C' to define a state-restoring command or
;;  you create a bookmark-list bookmark, you can create a sequence
;;  bookmark that combines such bookmark-list restoration with
;;  activation of other bookmarks.  (To include a state-restoring
;;  command in a sequence, you need to first create a function
;;  bookmark that uses the command, and then include that bookmark in
;;  the sequence.)
;;
;;
;;(@* "Using Multiple Bookmark Files")
;;  ** Using Multiple Bookmark Files **
;;
;;  Bookmark-list views (see
;;  (@> "Bookmark-List Views - Saving and Restoring State")) provide
;;  one way to switch among various sets of bookmarks that you use.
;;  But that feature affects only the bookmarks that you see displayed
;;  in buffer `*Bookmark List*', not the actual set of available
;;  bookmarks.
;;
;;  The bookmarks available to you are defined in a bookmark file.  By
;;  default, they are stored in the file named by option
;;  `bookmark-default-file' (`~/.emacs.bmk', by default).  You do not
;;  need to load or save this file manually; Emacs does that for you
;;  automatically.
;;
;;  But you can also have extra, alternative bookmark files if you
;;  want, and at any time you can change the bookmark file that is
;;  current.  To do that, use `C-x p L' (uppercase `L'), which is
;;  bound to command `bookmarkp-switch-bookmark-file'.  You can see
;;  which file is current by using `?' or `C-h m' in the buffer
;;  `*Bookmark List*' (or anywhere else using `M-x
;;  bookmarkp-bmenu-mode-status-help').
;;
;;  Having multiple bookmark files gives you an added degree of
;;  flexibility, but you must keep track of these extra bookmark files
;;  yourself - they are not managed for you automatically.
;;
;;  When bookmarks are saved automatically, or when you save them
;;  using `bookmark-save' (`S' in the bookmark list or `C-x p s'
;;  globally) and you don't use a prefix argument, they are saved in
;;  the current bookmark file.
;;
;;  You can turn off the automatic saving of the current bookmark
;;  file, by customizing option `bookmark-save-flag' to nil.  And you
;;  can toggle this option at any time, using `M-~' in the bookmark
;;  list (command `bookmarkp-toggle-saving-bookmark-file').
;;
;;  Besides using multiple bookmark files as alternatives, you can
;;  combine them, using them as component bookmark subsets (like
;;  modules).  To do that, use command `C-x p l' (lowercase `l'),
;;  which is bound to `bookmark-load', and do not use a prefix
;;  argument.  (Using a prefix argument with `C-x p l' is the same as
;;  using `C-x p L': it switches bookmark files.)
;;
;;  To create additional bookmark files, to use as either alternatives
;;  or component files, you can either copy an existing bookmark file
;;  or use `bookmarkp-empty-file' (`C-x p 0') to create a new, empty
;;  bookmark file.  If you use `C-x p 0' with an existing bookmark
;;  file, then its bookmarks are all deleted - it is emptied.
;;
;;  Instead of simply copying a bookmark file, you can use
;;  `bookmark-save' with a prefix argument, or use `bookmark-write'
;;  (bound to `C-x p w'), to save the currently defined bookmarks to a
;;  different bookmark file.
;;
;;  However a bookmark file was created, you can switch to it and then
;;  add or delete bookmarks selectively, to change its content.
;;  Remember that you can delete bookmarks from the current set using
;;  command `bookmark-delete' (`C-x p d') or, in the bookmark list,
;;  using `d' plus `x' or marking then `D'.
;;
;;
;;(@* "Bookmark List (Display)")
;;  ** Bookmark List (Display) **
;;
;;  Bookmark+ enhances the bookmark list (aka the bookmark "menu
;;  list", a misnomer) that is displayed in buffer `*Bookmark List*'
;;  when you use `C-x r l' (command `bookmark-bmenu-list').
;;
;;  Bookmarks are highlighted to indicate their type. You can mark and
;;  unmark bookmarks, show or hide bookmarks of particular types, and
;;  more.  Bookmarks that have tags are marked with a `t'.  Bookmarks
;;  that have an annotation are marked with an `a' (not with a `*' as
;;  in vanilla `bookmark.el').
;;
;;  Use `?' or `C-h m' in buffer `*Bookmark List*' for more
;;  information about the bookmark list, including the following:
;;
;;  * The current status of sorting, filtering, and marking.
;;
;;  * A legend for the faces used for different bookmark types.
;;
;;
;;(@* "Tag Commands and Keys")
;;  *** Tag Commands and Keys***
;;
;;  There are lots of tag-related bookmark commands, and they are all
;;  bound to keys in buffer `*Bookmark List*'.  How can you keep them
;;  straight or remember the keys?
;;
;;  `C-h m' (or `?') is your friend, of course.  Beyond that, the
;;  tag-related keys are organized as follows:
;;
;;    They all have the prefix key `T'.
;;
;;    `m' means mark
;;    `u' means unmark
;;    `>' stands for the marked bookmarks
;;    `*' means AND (set intersection; all)
;;    `+' means OR  (set union; some/any)
;;    `~' means NOT (set complement)
;;
;;  The key `T m *', for instance, marks (`m') the bookmarks that are
;;  tagged with all (`*' = AND) of a given set of tags.  It prompts you
;;  for one or more tags that the bookmarks must have, and it marks
;;  all bookmarks that have all of the tags you enter.
;;
;;  The key `T u ~ +' unmarks (`u') the bookmarks that do not (`~')
;;  have any (`+' = OR) of the tags you specify.  And so on.
;;
;;  The marking and unmarking commands for tags compare the tags a
;;  bookmark has with tags that you enter.  Any bookmarks that have no
;;  tags are ignored - they are neither marked nor unmarked by these
;;  commands.
;;
;;  `+' and `-' can also mean add and remove tags, respectively, and
;;  `>' stands for the marked bookmarks.  So `T > +' adds (`+') one or
;;  more tags to each of the marked (`>') bookmarks.
;;
;;  In general, the tag-related commands let you enter a set of tags,
;;  one at a time.  Thus, instead of having a command that adds a
;;  single tag to the current bookmark, you have a command that adds
;;  any number of tags to it.  To add just a single tag, hit `RET'
;;  twice: once to enter the tag, and once again to indicate that it
;;  is the last (i.e., the only) one.
;;
;;  If you just hit `RET' immediately, specifying an empty set of
;;  tags, then each of the commands does something different, but
;;  reasonable.  For `T m *', for instance, an empty list of tags
;;  means to mark (only) the bookmarks that have any tags at all.
;;
;;  Finally, for the marking/unmarking tags commands, a prefix
;;  argument flips the sense of the command, in this way:
;;
;;  "some are" -> "some are NOT", i.e., "not all are" (and vice versa)
;;  "all are"  -> "all are NOT",  i.e., "none are"    (and vice versa)
;;
;;  In other words:
;;
;;    C-u T m *    =  T m ~ +  (all are NOT      = not some are)
;;    C-u T m ~ +  =  T m *    (not some are NOT = all are)
;;    C-u T m +    =  T m ~ *  (some are NOT     = not all are)
;;    C-u T m ~ *  =  T m +    (not all are NOT  = some are)
;;
;;  You'll figure it out ;-).
;;
;;  Remember that `C-h RET' shows you the tags that belong to the
;;  current bookmark (under the cursor).  And `C-u C-h RET' shows you
;;  the full internal form of the tags, that is, the name+value pairs.
;;
;;  You can also sort bookmarks according to how they are tagged, even
;;  in complex ways.  See (@> "Sorting Bookmarks").
;;
;;
;;(@* "Sets of Bookmarks")
;;  *** Sets of Bookmarks ***
;;
;;  The best way to think about tags is as names of sets.  All
;;  bookmarks tagged `blue' constitute the bookmark set named `blue'.
;;
;;  The bookmarks visible in the bookmark list at any time also
;;  constitute an unnamed set.  Likewise, the marked bookmarks and the
;;  unmarked bookmarks are unnamed sets.  Bookmark+ is all about
;;  helping you act on sets of Emacs objects.  Bookmarks are named,
;;  persistent pointers to objects such as files and file sets.
;;  Bookmark tags are named, persistent sets of bookmarks (and hence
;;  of their target objects).
;;
;;  The marking commands make it easy to combine sets as unions or
;;  intersections.  And you can give the result a name for quick
;;  access later, just by adding a new tag.  in other words, do the
;;  set-definition work only once, and name the result.
;;
;;  How would you tag as `Java IDE Projects' the bookmarks that are
;;  already tagged both `Java' and `ide'?
;;
;;  1. `T m * Java RET ide RET RET', to mark them.
;;  2. `T > + Java IDE Projects RET RET, to tag them.
;;
;;  How would you sort your bookmarks, to show all those tagged both
;;  `blue' and `moon' first?
;;
;;  1. `T m * blue RET moon RET RET', to mark them.
;;  2. `s >' to sort the marked bookmarks first
;;     (see (@> "Sorting Bookmarks"), below).
;;
;;  If you wanted to show only the marked bookmarks, instead of
;;  sorting to put them first in the list, you would use `>'
;;  instead of `s >'.
;;
;;  How would you query-replace the set of files that are tagged with
;;  any of the tags `alpha', `beta', and `gamma', but are not tagged
;;  `blue' or `moon'?
;;
;;    1. `F S', to show only the file bookmarks.
;;    2. `T m + alpha RET beta RET gamma RET RET', to mark the
;;       bookmarks that have at least one of those tags.
;;    3. `T u + blue RET moon RET RET', to unmark those that are
;;       tagged `blue' or `moon'.
;;    4. `M-q' to query-replace the marked files.
;;
;;  If that were a set of files that you used often, then you would
;;  name the set by giving the files a new tag.
;;
;;  The point is that bookmarks, and bookmark tags in particular, let
;;  you define and manipulate sets of Emacs objects.  It doesn't
;;  matter how you define such a set: regexp matching (marking,
;;  filtering), by object type, by tag combinations...  Sets need not
;;  be named to act on them, but you can provide them with persistent
;;  names (tags) to avoid redefining them over and over.  Manipulation
;;  of bookmarked objects includes visiting, searching, and
;;  query-replacing.  And you can define your own bookmark types
;;  (using bookmark handlers) and associated manipulations.
;;
;;
;;(@* "Open Dired for the Marked Files")
;;  *** Open Dired for the Marked Files ***
;;
;;  You've seen that the bookmark list has many features that are
;;  similar to Dired features.  But Dired is specialized for files and
;;  directories, and it has many more features for manipulating them.
;;  The bookmark list is not intended to replace Dired.
;;
;;  You can, however, use the bookmark list to take advantage of
;;  arbitrary Dired features for file and directory bookmarks.
;;  Command `bookmarkp-bmenu-dired-marked' (`M-d >') weds Bookmark+'s
;;  set-defining and set-manipulating features (tagging, marking,
;;  filtering etc.) to Dired's file-manipulating features.
;;
;;  `M-d >' opens a Dired buffer that is specialized for just the
;;  files and directories whose bookmarks are marked in the bookmark
;;  list.  (Other marked bookmarks are ignored by the command.)  The
;;  files and directories can be located anywhere; they need not be in
;;  the same directory.  They are listed in Dired using absolute file
;;  names.
;;
;;  (In Emacs versions prior to release 23.2, only local files and
;;  directories can be handled, due to Emacs bug #5478.  In such
;;  versions, remote-file bookmarks are ignored by `M-d >'.)
;;
;;  This Bookmark+ feature makes sets of files and directories
;;  immediately amenable to all of the operations provided by Dired.
;;
;;  It is particularly useful in conjunction with tags.  Use bookmark
;;  tags and marks to define a possibly complex set of file and
;;  directory bookmarks.  Then hit `M-d >' to list them in a Dired
;;  buffer.  Then use any Dired commands you want to act on any of
;;  them.
;;
;;  For example, to compress bookmarked files that are tagged with
;;  both `blue' and `moon':
;;
;;  1. Mark them using `T m * blue RET moon RET RET'.
;;  2. Open Dired for them using `M-d >'.
;;  3. Mark them in Dired, then compress them using `Z'.
;;
;;  Since tags are persistent, Bookmark+ gives you a good way to
;;  define an arbitrary set of files as a project and then open them
;;  in Dired at any time to operate on them.
;;
;;  If you use Dired+ (library `dired+.el'), then a similar feature is
;;  available for the marked files and directories: You can use
;;  `C-M-*' in Dired to open a separate Dired buffer for them only.
;;  You can of course then bookmark that resulting Dired buffer, if
;;  you like.
;;
;;  If you use Icicles, then whenever you use a command that reads a
;;  file (or directory) name, you can use `M-|' during file-name
;;  completion to open Dired on the currently matching set of file
;;  names.  That is, this is the same kind of special Dired buffer
;;  that is provided for file and directory bookmarks by `M-d >' in
;;  the bookmark list.
;;
;;
;;(@* "Marking and Unmarking Bookmarks")
;;  *** Marking and Unmarking Bookmarks ***
;;
;;  Bookmark+ enhances the marking and unmarking of bookmarks in the
;;  bookmark list in several ways.  These enhancements are similar to
;;  features offered by Dired and Dired-X.  You can use:
;;
;;  * `% m' to mark the bookmarks that match a regexp.  The entire
;;    line in the bookmark list is checked for a match, that is, both
;;    the bookmark name and the file name, if shown.
;;
;;  * `M-DEL' (or `U') to unmark all bookmarks, or all that are marked
;;    `>', or all that are flagged `D' for deletion.
;;
;;  * `t' to toggle (swap) the marked and unmarked bookmarks: those
;;    that are marked become unmarked, and vice versa.
;;
;;  * `>' to show only the marked bookmarks or `<' to show only the
;;    unmarked bookmarks.  Repeat to show them all again.
;;
;;  * `F M', `I M' etc. to mark only the file bookmarks, Info
;;    bookmarks etc.  (The first key here is the same as the
;;    corresponding filter key, e.g. `F' for files - see next topic.)
;;
;;
;;(@* "Filtering Bookmarks (Hiding and Showing)")
;;  *** Filtering Bookmarks (Hiding and Showing) ***
;;
;;  You can hide and show different sets of bookmarks in the bookmark
;;  list.  There are commands to show only bookmarks of a particular
;;  type - e.g. `I S' to show only Info bookmarks.  These are, in
;;  effect, shortcuts for first marking those bookmarks and then
;;  showing only the marked bookmarks (and then unmarking).  For
;;  example, `F S' is a shortcut for `F M >' (and then `U RET').
;;
;;  You can also filter to show only the bookmarks that match a given
;;  regexp.  There are two ways to do this:
;;
;;  * Use `P B' (for "pattern", "bookmark") and type a regexp.  The
;;    bookmarks are filtered incrementally, as you type.  Only the
;;    bookmark name is matched (not the file name).  Hit any
;;    non-inserting key, such as `RET', to finish defining the
;;    pattern.
;;
;;    Similarly, hit `P F' for bookmarks whose file names match a
;;    regexp, and `P T' for bookmarks with one or more tags that match
;;    a regexp.  See (@> "Bookmark Tags"), above, for information
;;    about tags.
;;
;;  * Just as in Dired, use `% m' to mark the bookmarks that match a
;;    regexp.  Then use `>' to show only the marked bookmarks.  See
;;    (@> "Marking and Unmarking Bookmarks"), above.
;;
;;    This method has the advantage that you can show the complement,
;;    the bookmarks that do *not* match the regexp, by using `<'
;;    instead of `>'.  It also has the advantage that matching checks
;;    the combination of bookmark name and file name (use `M-t' to
;;    toggle showing file names).
;;
;;
;;(@* "Only Visible Bookmarks Are Affected")
;;  *** Only Visible Bookmarks Are Affected ***
;;
;;  Commands that operate on the current bookmark or on the marked or
;;  the unmarked bookmarks act only on bookmarks that are displayed
;;  (not hidden).  This includes the commands that mark or unmark
;;  bookmarks.  This means that you can easily define any given set of
;;  bookmarks.
;;
;;  For example:
;;
;;    Use `F S' to show only bookmarks associated with files.
;;    Use `% m' to mark those that match a particular regexp.
;;    Use `R S' to show only bookmarks that have regions.
;;    Use `m' to mark some of those region bookmarks individually.
;;    Use `.' to show all bookmarks.
;;    Use `t' to swap marked and unmarked (so unmarked are now marked)
;;    Use `D' to delete all of the marked bookmarks (after confirming)
;;
;;  Together, steps 1-7 delete all file bookmarks that match the
;;  regexp and all region bookmarks that you selectively marked.
;;
;;
;;(@* "Omitting Bookmarks from Display")
;;  *** Omitting Bookmarks from Display ***
;;
;;  In sections (@> "Marking and Unmarking Bookmarks") and
;;  (@> "Filtering Bookmarks (Hiding and Showing)") you learned how
;;  to hide and show bookmarks in the bookmark list.  This section is
;;  about a different kind of hiding, called "omitting".
;;
;;  Omitted bookmarks are not shown in the bookmark list, no matter
;;  what filtering is used.  The only way to show omitted bookmarks is
;;  to show all of them and only them, using `O S', which is bound to
;;  command `bookmarkp-bmenu-show-only-omitted'.
;;
;;  Omitted bookmarks are still available even if they are not shown,
;;  and you can still jump to them (e.g. using `C-x r b').  You just
;;  don't see them in the bookmark list.  And that's the reason for
;;  this feature: to hide those bookmarks that you don't care to see.
;;
;;  The most common use for this feature is to hide the component
;;  bookmarks that make up a sequence bookmark (see
;;  (@> "Function, Sequence, and Variable-List Bookmarks")).  The
;;  default behavior when you create a sequence bookmark is in fact to
;;  omit its component bookmarks from the displayed list.
;;
;;  You can omit any bookmarks by marking them and then using `O >'
;;  (`bookmarkp-bmenu-omit/unomit-marked').  If you are looking at the
;;  omitted bookmarks (after using `O S'), then `O >' un-omits the
;;  bookmarks marked there.  Think of two complementary spaces: the
;;  normal bookmark list and the omitted bookmark list.  When you use
;;  `O >', the marked bookmarks that are currently shown are moved to
;;  the opposite space.
;;
;;  You can un-omit all of the omitted bookmarks at once, using `O U'
;;  (`bookmarkp-unomit-all').  You can also call this command from
;;  outside the bookmark-list display.
;;
;;
;;(@* "Sorting Bookmarks")
;;  *** Sorting Bookmarks ***
;;
;;  Filtering hides certain kinds of bookmarks.  Sometimes, you want
;;  to see bookmarks of various kinds, but you want them to be grouped
;;  or sorted in different ways, for easy recognition, comparison, and
;;  access.
;;
;;  Bookmarks shown in the bookmark list are sorted using the current
;;  value of option `bookmarkp-sort-comparer'.  (If that is nil, they
;;  are unsorted, which means they appear in reverse chronological
;;  order of their creation.)
;;
;;  You can use `s s'... (repeat hitting the `s' key) to cycle among
;;  the various sort orders possible, updating the display
;;  accordingly.  By default, you cycle among all available sort
;;  orders, but you can shorten the cycling list by customizing option
;;  `bookmarkp-sort-orders-for-cycling-alist'.
;;
;;  You can also change directly to one of the main sort orders
;;  (without cycling) using `s n', `s f n', etc. - use `C-h m' or `?'
;;  for more info.
;;
;;  You can reverse the current sort direction (ascending/descending)
;;  using `s r'.  Also, repeating any of the main sort-order commands
;;  (e.g. `s n') cycles among that order, the reverse, and unsorted.
;;
;;  For a complex sort, which involves composing several sorting
;;  conditions, you can also use `s C-r' to reverse the order of
;;  bookmark sorting groups or the order within each group (depending
;;  on whether `s r' is also used).  Be aware that this can be a bit
;;  unintuitive.  If it does not do what you expect or want, or if it
;;  confuses you, then don't use it ;-).  (`s C-r' has no noticeable
;;  effect on simple sorting.)
;;
;;  Remember that you can combine sorting with filtering different
;;  sets of bookmarks - bookmarks of different kinds (e.g. Info) or
;;  bookmarks that are marked or unmarked.
;;
;;  Finally, you can easily define your own sorting commands and sort
;;  orders.  See macro `bookmarkp-define-sort-command' and the
;;  documentation for option `bookmarkp-sort-comparer'.  (Bookmark+
;;  uses option `bookmarkp-sort-comparer'; it *ignores* vanilla Emacs
;;  option `bookmark-sort-flag'.)
;;
;;  Of particular note is that you can interactively define commands
;;  that sort by a given list of tags - you use keys `T s' (command
;;  `bookmarkp-define-tags-sort-command') to do that.  You are
;;  prompted for the tags to sort by.  Bookmarks are sorted first
;;  according to whether they are tagged with the first tag, then the
;;  second tag, and so on.  Otherwise, sorting is by bookmark name.
;;
;;  The tags you specify are used, in order, in the name of the new
;;  command.  For example, if you enter tags `alpha', `beta', and
;;  `gamma', in that order, then the sorting command created is
;;  `bookmarkp-bmenu-sort-alpha-beta-gamma'.  The new command is saved
;;  in your bookmark commands file (`bookmarkp-bmenu-commands-file').
;;
;;  Note that because you can add a new tag to all bookmarks that have
;;  some given set of tags, you can use that single (new) tag to
;;  represent the entire tag set.  Sorting by that tag is then the
;;  same as sorting by the tag set.  You can of course use overlapping
;;  sets in the composite sort command.  You can, for example, sort
;;  first according to tag `tag1', which represents the set of tags
;;  `alpha', `beta', `gamma', `delta', and then sort according to tag
;;  `tag2', which represents the set of tags `beta', `delta'.
;;
;;  See also (@> "Use Bookmark+ with Icicles") - the same technique is
;;  used in Icicles for sorting bookmarks as completion candidates.
;;
;;
;;(@* "Bookmarks for Specific Files or Buffers")
;;  ** Bookmarks for Specific Files or Buffers **
;;
;;  A bookmark typically records a position or a region in a file or
;;  buffer.  Sometimes you are interested in accessing or examining
;;  only the bookmarks for particular files or buffers.  For example,
;;  you might want to navigate among the bookmarks for the current
;;  buffer.  Or you might want to search the regions recorded in the
;;  bookmarks for a particular file.
;;
;;  For a bookmark, the recorded file and buffer name differ in that
;;  the file name is absolute.  Bookmarks for buffer `foo.el' include
;;  all files named `foo.el', whereas bookmarks for file
;;  `/project1/lisp/foo.el' include only the files in that one
;;  directory.
;;
;;  Bookmark+ provides some commands to handle these use cases.  The
;;  keys bound to these commands use `f' for file and `b' for buffer.
;;  In the bookmark-list display, the following keys affect the
;;  bookmarks for a particular file or buffer whose name you provide
;;  (with completion).
;;
;;  * `= f M' and `= b M' - mark 
;;  * `= f S' and `= b S' - show (only)
;;
;;  For navigation, the following keys jump to bookmarks for
;;  particular files or buffers.  (Use `C-x 4 j' for other-window.)
;;
;;  * `C-x j .'                   - current buffer
;;  * `C-x j = f' and `C-x j = b' - specified file(s) or buffer(s)
;;
;;  For the `=' keys you are prompted for one or more file names or
;;  buffer names.
;;
;;  Finally, because the bookmarks in the current buffer can be of
;;  particular interest, `C-x p .' opens the bookmark-list display for
;;  only those bookmarks.
;;
;;
;;(@* "Use Bookmark+ with Icicles")
;;  ** Use Bookmark+ with Icicles **
;;
;;  Icicles (http://www.emacswiki.org/cgi-bin/wiki/Icicles) enhances
;;  your use of Bookmark+ in several ways.
;;
;;  When jumping to a bookmark, you can narrow the completion
;;  candidates to bookmarks of a particular type (e.g. Info, using
;;  `C-M-i'; remote, using `C-M-@'; region, using `C-M-r').  You can
;;  narrow again (and again), to another bookmark type, to get the
;;  intersection (e.g. remote Info bookmarks that define a region).
;;
;;  You can also narrow against different bookmark-name patterns
;;  (e.g. regexps) - so-called "progressive completion".  And take the
;;  complement (e.g., bookmarks whose names do not match
;;  `foo.*2010.*bar').  (This is not special to bookmarks; it is
;;  standard Icicles practice.)
;;
;;  In Icicle mode, several of the Bookmark+ keys are remapped to
;;  corresponding Icicles multi-commands.  A bookmark jump key thus
;;  becomes a bookmarks browser.  For example, `C-x j d' browses among
;;  any number of Dired bookmarks.
;;
;;  When you browse among bookmarks, visiting them, the current
;;  destination (position) is highlighted temporarily using
;;  crosshairs, to make it stand out.
;;
;;  A single key can set a bookmark or visit bookmarks.  This key is
;;  whatever command `bookmark-set' would normally be bound to -
;;  e.g. `C-x r m'.  A prefix arg controls what it does.  If negative
;;  (`M--'), jump to (browse) bookmarks.  Otherwise, set a bookmark,
;;  as follows:
;;
;;  * Numeric prefix arg (non-negative): No prompt.  Use the buffer
;;    name plus the text of the region (if active) or the current line
;;    as the bookmark name.  Quickest way to set a bookmark.
;;
;;  * No prefix arg (as usual): Prompt for bookmark name.  But if the
;;    region is active, use the buffer name plus the region text as
;;    the default name.
;;
;;  * Plain `C-u' (as usual): Prompt for name; no bookmark overwrite.
;;
;;  During completion of a bookmark name, many features of the
;;  bookmark-list display (see (@> "Bookmark List (Display)")) are
;;  available on the fly.  Buffer `*Completions*' acts like a dynamic
;;  version of `*Bookmark List*':
;;
;;  * Candidates are highlighted in the `*Completions*' window
;;    according to their bookmark type.
;;
;;  * Candidates are Icicles multi-completions with up to three parts:
;;
;;     a. the bookmark name
;;     b. the bookmark file or buffer name
;;     c. any tags
;;
;;    You can match any or all of the parts.  For example, you can
;;    match bookmarks that have tags by typing this regexp:
;;
;;    C-M-j . * C-M-j S-TAB
;;
;;    Each `C-M-j' inserts `^G\n', which is `icicle-list-join-string',
;;    the string used to join the parts.  This regexp says, "match the
;;    completion candidates that have all three parts (two join
;;    strings)", hence some tags.
;;
;;    You can turn off the use of multi-completion candidates for
;;    subsequent commands, so only bookmark names are used, by hitting
;;    `M-m' in the minibuffer.  You can think of this as similar to
;;    using `M-t' in `*Bookmark List*' to toggle showing file names.
;;    You can make not showing files and tags the default behavior by
;;    customizing `icicle-show-multi-completion-flag'.
;;
;;  * You can sort completion candidates using the Bookmark+ sort
;;    orders.  Use `C-,' to cycle among sort orders.
;;
;;  * You can use Icicles candidate-help keys (`C-M-RET', `C-M-down',
;;    etc.) to get detailed information about the current bookmark
;;    candidate.  `C-u C-M-RET' shows the complete, internal info
;;    defining the bookmark.  And without doing anything, summary info
;;    about the current candidate is available in the mode line of
;;    buffer `*Completions*'.
;;
;;  * You can use Icicles candidate-action keys (`C-RET', `C-mouse-2',
;;    `C-down', etc.) to visit any number of bookmarks.  For example,
;;    holding down `C-down' cycles among the current bookmark
;;    candidates, opening each in turn.
;;
;;  * You can use `S-delete' to delete the bookmark named by the
;;    current candidate.  You can delete any number of bookmarks this
;;    way, during a single invocation of a bookmark command.
;;
;;  * You can define Icicles sets of bookmarks, persistent or not, and
;;    act on their members in various ways.
;;
;;
;;(@* "Open Bookmarks Using Windows File Associations")
;;  ** Open Bookmarks Using Windows File Associations **
;;
;;  If you use Emacs on Microsoft Windows, then you can take advantage
;;  of Windows file associations to open bookmarks.  To do this, you
;;  will also need library `w32-browser.el'.
;;
;;  In the bookmark list, the following keys are bound to commands
;;  that open bookmarks using the associated Windows `Open'
;;  applications:
;;
;;    `M-RET'     - `bookmarkp-bmenu-w32-open'
;;    `M-mouse-2' - `bookmarkp-bmenu-w32-open-with-mouse'
;;    `M-v'       - `bookmarkp-bmenu-w32-open-select' (like `v')
;;
;;  If you use Dired+ (library `dired+.el'), then you can use `M-b' to
;;  bookmark all of the marked files in a Dired buffer, even if you
;;  normally do not or cannot visit those files in Emacs.  For
;;  instance, you can bookmark music files or image files, without
;;  ever opening them as files in Emacs.
;;
;;  Together with the use of bookmark tags, this gives you a handy way
;;  to organize and access objects of any kind whose files are
;;  recognized by Windows as being associated with a given
;;  application.  See (@> "Bookmark Tags").
;;
;;
;;(@* "Bookmark Compatibility with Vanilla Emacs (`bookmark.el')")
;;  ** Bookmark Compatibility with Vanilla Emacs (`bookmark.el') **
;;
;;  Library `bookmark+.el' is generally compatible with GNU Emacs
;;  versions 20 through 23.
;;
;;  1. All bookmarks created using any version of vanilla Emacs
;;     (library `bookmark.el') continue to work with `bookmark+.el'.
;;
;;  2. All bookmarks created using library `bookmark+.el' will work
;;     with all Emacs versions (20-23), provided you use library
;;     `bookmark+.el' to access them.
;;
;;  3. Most bookmarks created using `bookmark+.el' will not interfere
;;     with the behavior of vanilla Emacs, versions 21-23.  The new
;;     bookmark types are simply ignored by vanilla Emacs.  For
;;     example:
;;
;;     - A bookmark with a region is treated like a simple position
;;       bookmark: the destination is the region start position.
;;
;;     - A Gnus bookmark does not work; it is simply ignored.
;;
;;     However, there are two cases in which `bookmark+.el' bookmarks
;;     will raise an error in vanilla Emacs:
;;
;;     * You cannot use non-file (e.g. buffer-only) bookmarks with any
;;       version of vanilla Emacs.
;;
;;     * You cannot use any bookmarks created using `bookmark+.el'
;;       with vanilla Emacs 20.
;;
;;     The Emacs bookmark data structure has changed from version to
;;     version.  Library `bookmark+.el' always creates bookmarks that
;;     have the most recent structure (Emacs 23).  As is the case for
;;     any bookmarks that have the Emacs 23 structure, these bookmarks
;;     will not work in vanilla Emacs 20 (that is, without
;;     `bookmark+.el').
;;
;;  Bottom line: Use `bookmark+.el' to access bookmarks created using
;;  `bookmark+.el'.
;;
;;
;;(@* "New Bookmark Structure")
;;  ** New Bookmark Structure **
;;
;;  The bookmark data structure, variable `bookmark-alist', has been
;;  enhanced to support new bookmark types.  For a description of this
;;  enhanced structure, use `C-h v bookmark-alist'.
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;;(@* "Change log")
;;
;; 2010/05/22 dadams
;;     *-this-buffer-p: Return nil for bookmarks not really associated with a buffer.
;;     *-default-handler, *-goto-position: Forgot comma to eval file-name when no-such-file error.
;;     *-show-annotation: Bind buffer-read-only to nil for updating.
;; 2010/05/19 dadams
;;     Added: bookmarkp-this-buffer-bmenu-list.  Bound to `C-x p .'.
;;     menu-bar-bookmark-map:
;;       Added bookmarkp-this-buffer-bmenu-list.  Added separators.
;;       Added vanilla items edit, write, load, to impose order.  Renamed item edit.
;; 2010/05/16 dadams
;;     bookmark-set: Quoted history arg.  Thx to S. Nemec.
;;     bookmarkp-bmenu-define-full-snapshot-command: Use quote comma, not quote, for *-specific-*.
;; 2010/05/15 dadams
;;     Replace *same-(buffer|file)-jump* by *specific-(buffers|files)-jump*: read multiple buff/files.
;;     Renamed: *same-(buffer|file)* to *-last-specific-(buffer|file)* for pred, alist, and var.
;;     Renamed: *same-(buffer|file)* to *specific-(buffer|file)* for hist, *types*, mark/show cmds.
;;     Renamed: *-selected-buffers-alist* to *-specific-buffers-alist*.
;;     Added: *-specific-files-alist*, *-(all|some)-tags(-regexp)-alist-only.
;;     *-completing-read-(buffer|file)-name: Use (buffer|file)-name-history, not *-same-*-history.
;;     *-read-tags-completing: Rewrote to correctly handle cons and string tags, error handling, etc.
;;     *-bmenu-(add|remove)-tags-*-marked: Error handling.
;;     *(all|some)-tags(-regexp)-jump*: Use *-(all|some)-tags(-regexp)-alist-only.
;; 2010/05/11 dadams
;;     Added: bookmarkp-bmenu-mark-same-(buffer|file)-bookmarks, bookmarkp-this-(buffer|file)-p,
;;            bookmarkp-this-(buffer|file)-alist-only, bookmarkp-bmenu-show-only-same-(buffer|file),
;;            bookmarkp-completing-read-(buffer|file)-name, bookmarkp-same-(buffer|file)-history,
;;            bookmarkp-(same|this)-(buffer|file)-alist-only, bookmarkp-last-same-(buffer|file),
;;            bookmarkp-(same|this)-(buffer|file)-jump(-other-window), bookmarkp-(buffer|file)-names,
;;            bookmarkp-same-(buffer|file)-as-last-p, bookmarkp-other-window-pop-to-flag,
;;            bookmarkp-select-buffer-other-window.
;;     Use bookmarkp-select-buffer-other-window instead of switch-to-buffer-other-window everywhere.
;;     Bound = (b|f) (M|S), C-x j (=|.) (b|f) to (same|current)-(buffer|file) commands.
;;     *-types-alist: Handle same-(buffer|file) too.
;;     *-bmenu-list, *-bmenu-define-full-snapshot-command, *-save-menu-list-state:
;;       Handle bookmarkp-last-same-(buffer|file) as part of state.
;; 2010/05/05 dadams
;;     bookmarkp-create-varlist-bookmark, bookmarkp-make-varlist-record:
;;       Added optional arg BUFFER-NAME.
;;     bookmark-alist: Corrected doc string wrt BUFFER-NAME and region context strings.
;; 2010/05/04 dadams
;;     Added: bookmarkp-create-varlist-bookmark.
;;     bookmarkp-jump-varlist: If bookmark's buffer doesn't exist, use current buffer and show msg.
;; 2010/04/24 adams
;;     Added: bookmarkp-bmenu-show-only-varlists, bookmarkp-set-restrictions-bookmark,
;;            bookmarkp-set-varlist-bookmark, bookmarkp-varlist-jump, bookmarkp-varlist,
;;            bookmarkp-jump-varlist, bookmarkp-make-varlist-record, bookmarkp-printable-p,
;;            bookmarkp-printable-vars+vals, bookmarkp-read-variables-completing,
;;            bookmarkp-read-variable, bookmarkp-varlist-alist-only, bookmarkp-varlist-bookmark-p,
;;            bookmarkp-varlist-history.
;;     Bound bookmarkp-bmenu-show-only-varlists to V S, bookmarkp-varlist-jump to C-x j v (and menu).
;;     *-types-alist: Added bookmarkp-varlist-history.
;;     *-bmenu-mode-status-help, *-bmenu-propertize-item, *-describe-bookmark: Handle varlist bmks.
;;     *-bmenu-w32-open-select: Changed binding to M-v from V.
;;     *-bmenu-mode: Updated doc string.
;; 2010/04/17 dadams
;;     bookmark-set: Numeric prefix arg means use all bookmarks as completion candidates.
;;                   Simplified the prompt.
;;     bookmarkp-completing-read-1:
;;       Use icicle-transform-multi-completion in icicle-delete-candidate-object
;;     Ensure loaded before byte-compile (put a require after provide).
;;     Move bookmarkp-replace-regexp-in-string before macro bookmarkp-define-sort-command (uses it).
;;     bookmarkp-bmenu-w32-open-with-mouse, bookmarkp-bmenu-mouse-3-menu:
;;       Use with-current-buffer, not save-excursion of set-buffer.
;;     bookmarkp-make-dired-record, bookmarkp-jump-dired: Use dolist, not mapcar (just side effect).
;;     bookmarkp-(some|all)-tags-jump(-other-window): Removed extraneous arg in error call.
;; 2010/04/16 dadams
;;     Added: bookmarkp-completing-read-1, bookmarkp-completing-read-lax,
;;            bookmarkp-selected-buffers-alist-only.
;;     bookmark-set: Use bookmark-completing-read-lax w/ buffer's bookmarks, not read-from-minibuffer.
;;     bookmark-completing-read: Define using bookmarkp-completing-read-1.
;; 2010/04/09 dadams
;;     bookmarkp-edit-bookmark: Update dired-directory property along with filename property. 
;; 2010/03/28 dadams
;;     bookmarkp-goto-position: Don't funcall bookmarkp-jump-display-function if it is nil.
;; 2010/03/28 dadams
;;     bookmark-default-handler: Don't funcall bookmarkp-jump-display-function if it is nil.
;; 2010/03/27 dadams
;;     Changed the customization group from bookmarkp to bookmark-plus.
;;     Moved doc and change history from bookmark+.el to this new file, bookmark+-doc.el.
;;     bookmarkp-commentary-button: Use bookmark+-doc.el, not bookmark+.el.
;; 2010/03/17 dadams
;;     Added: bookmarkp-toggle-bookmark-set-refreshes, bookmarkp-refresh-latest-bookmark-list,
;;            bookmarkp-after-set-hook.
;; 2010/03/16 dadams
;;     Fixed parens placement (typo) for last change to define *-jump-woman for Emacs 20.
;; 2010/03/11 dadams
;;     Define bookmarkp-jump-woman also for Emacs 20 (just raise an error).
;;     *-show-annotation: Typo: bookmark -> bmk-name.
;; 2010/03/10 dadams
;;     Added: bookmarkp-bookmark-creation-cp, bookmarkp-bmenu-sort-by-creation-time (bind: s0, menu).
;;     *-make-record-default: Add entry: created.
;;     *-describe-bookmark: Add creation time to description.
;; 2010/03/03 dadams
;;     *-sort-and-remove-dups: Do not set bookmarkp-sorted-alist to the result.
;;     *-bmenu-list-1: Set bookmarkp-sorted-alist to the result of calling *-sort-and-remove-dups.
;; 2010/03/02 dadams
;;     Added: bookmarkp-sorted-alist.
;;     *-bmenu-list-1: Use bookmarkp-sorted-alist.
;;     *-sort-and-remove-dups: Set bookmarkp-sorted-alist to the result.
;;     All *-cp (and hence *-define-file-sort-predicate):
;;       Accept bookmark names as args, in addition to bookmarks.
;;     bookmark-alpha-p: Don't use bookmarkp-make-plain-predicate, to avoid infinite recursion.
;; 2010/02/28 dadams
;;     Added: bookmarkp-send-bug-report.
;;     bookmarkp-bmenu-mode-status-help: Rewrote to provide only Bookmark+ help.  Added help buttons.
;;     Fixed Commentary typos.
;; 2010/02/26 dadams
;;     Added: bookmarkp-desktop-change-dir, bookmarkp-desktop-kill, bookmarkp-desktop-delete.
;;     *-jump-desktop: Call *-desktop-change-dir.
;;     *-read-bookmark-for-type: Added optional arg ACTION.
;; 2010/02/24 dadams
;;     *-bmenu-list: Handle case null last-bookmark-file (due to old file format).  Thx to Seb Luque.
;;     *-make-record-default: protect dired-buffers with boundp.  Thx to Janek Schwarz.
;; 2010/02/16 dadams
;;     bookmarkp-define-sort-command: Add msg suffix about repeating.
;;     bookmarkp-msg-about-sort-order: Added optional arg SUFFIX-MSG.
;; 2010/02/15 dadams
;;     Added: bookmark-bmenu-switch-other-window (redefinition for Emacs 20-22).
;;     *-bmenu-mode: Added redefinition, instead of advising.
;;     *-send-edited-annotation, *-relocate, *-rename, *-bmenu-refresh-menu-list,
;;       *-remove(-all)-tags, *-add-tags:
;;         Refresh the menu list, to pick up changes.
;;     *-refresh-menu-list: Added optional arg BOOKMARK: go to it.
;;     Do not bind bookmark-bmenu-relocate unless it's defined.
;;     *-handler-cp: Respect case-fold-search.
;; 2010/02/14 dadams
;;     Renamed bookmark-bmenu-list-1 to bookmarkp-bmenu-list-1.
;;     Added faces: bookmarkp-(a|t|>|D)-mark, bookmarkp-heading (don't use bookmark-menu-heading).
;;     Added redefinitions: bookmark-send-edited-annotation, bookmark(-bmenu)-show-annotation,
;;                          bookmark-show-all-annotations.
;;     *-bmenu-mark, *-bmenu-delete, *-bmenu-list-1: Add faces to marks.
;;     *-bmenu-list-1 and everywhere: Get rid of "% " before menu-list title.
;;     *-bmenu-list-1: Use "a", not "*", as annotation mark.
;;                     Add "t" mark for tags.  Add an extra space before bookmark name.
;;     *-bmenu-marks-width: change value from 2 to 4, for the tags column and the extra space.
;; 2010/02/13 dadams
;;     Added: bookmarkp-desktop-history,
;;            bookmarkp-desktop-jump (bound to C-x j K; added to menu),
;;            bookmarkp-bookmark-list-jump (bound to C-x j B; added to menu),
;;            bookmarkp-bookmark-list-alist-only, bookmarkp-bookmark-list-history.
;;     *-types-alist: Added entries for desktops and bookmark-lists.
;;     *-describe-bookmark: Added optional arg, to show full (internal) info.
;;                          Bind it to ? in bookmark-map.
;;     *-jump-bookmark-list: Pop to the bookmark-list (to show it).
;;     *-bmenu-mark-w3m-bookmarks: Typo: wrong predicate.
;;     *(-bmenu)-remove-all-tags: Raise error if no tags to remove.
;;     *-bmenu-remove-all-tags: Require confirmation if interactive.
;;     *-bmenu-remove-tags: Added optional arg MSGP.
;;     Menus: Added "..." as needed.
;;     *-bmenu-mouse-3-menu: Guard bookmark-bmenu-relocate with fboundp.
;; 2010/02/12 dadams
;;     Added: bookmarkp-bmenu-define-jump-marked-command.  Bound to M-c and added to menu.
;;     Changed bookmarkp-toggle-saving-bookmark-file binding to M-~ (M-s conflicts w isearch-multi).
;;     Updated bookmark-bmenu-mode doc string.
;; 2010/02/11 dadams
;;     Added: bookmarkp-types-alist,
;;            bookmarkp-(dired|gnus|info|man|region|w3m|(non-|local-|remote-)file)-history.
;;     bookmark-completing-read: Added optional HIST arg.
;;     *-(relocate|rename|insert(-location)): Added bookmark default for interactive use.
;;     *-jump-dired: Handle bookmarkp-jump-display-function.
;;     *-read-bookmark-for-type: Added optional HIST arg.
;;     *-jump-to-type(-other-window),
;;       *-(dired|file|gnus|info|man|region|w3m|(local-|non-|remote-)file)-jump*(-other-window):
;;         Use type-specific history var.
;; 2010/02/09 dadams
;;     Added: bookmarkp-get-tag-value.
;;     bookmark--jump-via: Handle special bookmark tag bookmarkp-jump.
;; 2010/02/08 dadams
;;     Renamed: bookmarkp-bmenu-dired-marked-local to bookmarkp-bmenu-dired-marked.
;;     bookmarkp-bmenu-dired-marked: Handle remote bookmarks if Emacs > 23.1.
;;     Support tags with values.
;;       Added: bookmarkp-tag-name, bookmarkp-full-tag, bookmarkp(-bmenu)-set-tag-value.
;;       Renamed variable (not fn) bookmarkp-tags-list to bookmarkp-tags-alist.
;;       Use bookmarkp-full-tag everywhere for tag completion.
;;       *-has-tag-p: Use assoc-default, not member.
;;       *-read-tag(s)-completing: CANDIDATE-TAGS is now an alist.
;;       *-list-all-tags: Added optional arg FULLP (prefix arg).
;;       *-tags-list: Added optional arg NAMES-ONLY-P.
;;       *-(add|remove|rename)-tags: Use copy-alist, not copy-sequence.  Alist, not list, membership.
;;       *-rename-tag: Raise error if no tag with old name.
;;       *-bmenu-mark-bookmarks-tagged-regexp, *-regexp-filtered-tags-alist-only, *-describe-bookmark,
;;         *-(all|some)-tags-regexp-jump(-other-window):
;;           Use bookmarkp-tag-name.
;;       *-bmenu-mark/unmark-bookmarks-tagged-(all|some)/(none|not-all), *-define-tags-sort-command:
;;         Use assoc-default, not member.
;;     Added: bookmarkp-bmenu-add-tags, bookmarkp-bmenu-remove(-all)-tags.
;;     *-bmenu-mouse-3-menu: Use bookmarkp-bmenu-add-tags, bookmarkp-bmenu-remove(-all)-tags.
;;                           Added bookmarkp-bmenu-set-tag-value.
;;     *-bmenu-mark-bookmarks-satisfying: Made it a command (interactive).
;; 2010/02/07 dadams
;;     *-write-file: Corrected handling of ALT-MSG.
;;     Cleanup.
;;       *-remove-tags: Don't call *-get-tags twice.
;;       *-bmenu-(un)mark-bookmarks-tagged(-not)-(all|none|some):
;;         Don't duplicate what bookmarkp-read-tags-completing does.
;;       *-add-tags, *-remove-tags(-from-all): TAGS arg must be a list from the beginning.
;;       *-remove-tags-from-all, *-rename-tag: Use bookmark-all-names - don't mapcar car over alist.
;;       *-all-tags-regexp-jump: Corrected to use same pred as *-all-tags-regexp-jump-other-window.
;;       *-(some|all)-tags-jump(-other-window): Use bookmarkp-has-tag-p - don't repeat the definition.
;;       *-read-tag(s)-completing: Removed unnecessary or.
;; 2010/02/06 dadams
;;     *-write-file, *-empty-file: Corrected handling of ALT-MSG.
;; 2010/02/05 dadams
;;     Added: bookmarkp-same-file-p, bookmarkp-empty-file.
;;     Bound bookmarkp-empty-file to C-x p 0, and added it to menus.
;;     *-bmenu-list, *-switch-bookmark-file: Use bookmarkp-same-file-p.
;;     bookmark-write-file: Added optional ALT-MSG arg.
;; 2010/02/04 dadams
;;     Added: bookmarkp-bmenu-omit, bookmarkp-list-all-tags.  Added to mouse-3 menu, Tags menus.
;; 2010/02/01 dadams
;;     Added: bookmarkp-current-bookmark-file, bookmarkp-switch-bookmark-file,
;;            (redefinition of) bookmark-load, (redefinition of) bookmark-save,
;;            bookmarkp-toggle-saving-bookmark-file, bookmarkp-last-save-flag-value.
;;     *-bmenu-list: Restore bookmarkp-current-bookmark-file if appropriate.
;;     *-bmenu-mode-status-help: Show bookmarkp-current-bookmark-file.
;;     *-bmenu-define-full-snapshot-command, *-save-menu-list-state:
;;       Save bookmarkp-current-bookmark-file.
;;     Bound bookmarkp-switch-bookmark-file to L and C-x r L.  Added both load commands to both menus.
;;     *-toggle-saving-menu-list-state: Changed binding to M-l.  Error if init value is nil.
;;     Bound *-toggle-saving-bookmark-file to M-s and added to menu.
;;     Added bookmark-write to bookmarkp-bmenu-menubar-menu (Save As).
;;     bookmarkp-bmenu-menubar-menu: Added :help strings everywhere.
;;     bookmarkp-bmenu-mode-status-help: Added face legend.
;; 2010/01/31 dadams
;;     Added: bookmarkp-tags-list, bookmarkp-read-tag-completing, bookmarkp-use-w32-browser-p,
;;            bookmarkp-bmenu-w32-open(-select|-with-mouse).  Bind *-w32* to M-RET, V, M-mouse-2.
;;     *-default-handler: Call w32-browser if bookmarkp-use-w32-browser-p.
;;     *-bmenu-unomit-marked: Don't try to return to original position (duh).
;;     *-bmenu-goto-bookmark-named: Use eobp as loop condition.  Call bookmark-bmenu-ensure-position.
;;     *-read-tags-completing:
;;       Added arg UPDATE-TAGS-LIST-P.  Call bookmark-maybe-load-default-file.
;;       Use bookmarkp-tags-list if CANDIDATE-TAGS is nil.  Update that var if UPDATE-TAGS-LIST-P.
;;     *-(add|remove)-tags: Added arg NO-CACHE-UPDATE-P.  If nil, call bookmarkp-tags-list.
;;     *-remove-tags-from-all, *-rename-tag, *-bmenu-(add|remove)-tags-(to|from)-marked:
;;       Call bookmarkp-tags-list.
;;     *-remove-tags-from-all: Pass nil as tags arg to bookmarkp-read-tags-completing.
;;     *-rename-tag: Use bookmarkp-read-tag-completing, not read-string.
;; 2010/01/29 dadams
;;     bookmarkp-describe-bookmark: Handle desktop bookmarks specifically.
;;     Added: bookmarkp-menu-popup-max-length.
;;     bookmark-completing-read: Use bookmarkp-menu-popup-max-length.
;;     bookmarkp-bmenu-state-file: Added missing default value for const.
;;     Don't add jump-other entry to menu-bar-bookmark-map (just use Jump To submenu).
;; 2010/01/28 dadams
;;     bookmarkp-(all|some)-tags(-regexp)-jump(-other-window): Error if no bookmarks with the tags.
;;     bookmarkp-(all|some)-tags-jump(-other-window): Handle case where user enters no tags.
;;     Use :advertised-binding property for bookmark-jump(-other-window).
;;     Added: bookmarkp-bmenu-jump-menu.
;;     Added bookmarkp-bmenu-jump-menu to menu-bar-bookmark-map and bookmarkp-bmenu-menubar-menu.
;; 2010/01/27 dadams
;;     Added: bookmarkp-every, bookmarkp-(all|some)-tags(-regexp)-jump(-other-window).
;; 2010/01/26 dadams
;;     Added: bookmarkp-bmenu-dired-marked-local.  Bound to M-d >.
;; 2010/01/23 dadams
;;     Added: bookmarkp-handler-cp, bookmarkp-desktop-no-save-vars, bookmarkp-set-desktop-bookmark,
;;            bookmarkp-make-desktop-record, bookmarkp-jump-desktop, bookmarkp-desktop-read,
;;            bookmarkp-desktop-alist-only, bookmarkp-desktop-bookmark-p,
;;            bookmarkp-bmenu-mark-desktop-bookmarks, bookmarkp-bmenu-show-only-desktops,
;;            face bookmarkp-desktop.
;;     bookmarkp-bmenu-sort-by-bookmark-type: Add bookmarkp-handler-cp to the list (last).
;;     bookmarkp-bmenu-propertize-item: Add face bookmarkp-desktop for desktop bookmarks.
;;     Bound: bookmarkp-set-desktop-bookmark to C-x p K, C-x r K,
;;            bookmarkp-bmenu-mark-desktop-bookmarks to K M (and Mark menu),
;;            bookmarkp-bmenu-show-only-desktops to K S (and Show menu).
;;     bookmark-bmenu-mode doc string: Updated for new commands.
;;     Added autoload cookies for all defcustoms.
;; 2010/01/20 dadams
;;     Added: bookmarkp-bmenu-mode-status-help.  Bound to C-h m, ?.
;; 2010/01/19 dadams
;;     bookmarkp-remote-file-bookmark-p: Include remote Dired bookmarks.  Thx to Simon Harrison.
;;     Added: bookmarkp-describe-bookmark-internals, bookmarkp-bmenu-describe-this+move-(down|up),
;;            defalias for list-bookmarks.
;;     bookmarkp-describe-bookmark: Reformatted help output.  Added more info about Dired bookmarks.
;;     bookmarkp-bmenu-describe-this-bookmark:
;;       C-u calls bookmarkp-describe-bookmark-internals.  Bound also to C-h C-RET.
;; 2010/01/18 dadams
;;     Added: bookmarkp-dired-subdirs.
;;     bookmarkp-make-dired-record, bookmarkp-jump-dired: Handle inserted and hidden dirs.
;;     bookmarkp-jump-dired: Use expand-file-name, not concat.
;; 2010/01/17 dadams
;;     Added:
;;       bookmarkp-jump(-other-window)-map, bookmarkp-jump-1, bookmark-all-names (redefined),
;;       bookmarkp-read-bookmark-for-type, bookmarkp-dired-jump-current(-other-window),
;;       bookmarkp-(dired|(local-|remote-|non-)file|gnus|info|man|region|w3m)-jump(-other-window),
;;       bookmarkp-jump-to-type(-other-window).
;;     bookmark-jump(-other-window): Use bookmarkp-jump-1.
;;     bookmark-completing-read: Added optional args ALIST and PRED.
;;     bookmarkp-default-bookmark-name: Added optional arg ALIST.
;; 2010/01/14 dadams
;;     bookmark-bmenu-surreptitiously-rebuild-list: Put back save-excursion & save-window-excursion.
;; 2010/01/02 dadams
;;     Renamed *-bmenu-check-position to *-bmenu-ensure-position, per Emacs 23.2.  Added defalias.
;; 2010/01/01 dadams
;;     *-bmenu-(un)mark, *-bmenu-other-window, *-bmenu-rename: Call bookmark-bmenu-check-position.
;;     *-bmenu-delete: Don't call bookmark-bmenu-check-position again at end.
;;     *-bmenu-edit-bookmark: Call bookmark-bmenu-check-position at beginning, not end.
;; 2009/12/30 dadams
;;     Added: bookmarkp-bmenu-header-lines, bookmarkp-bmenu-marks-width.  Use everywhere.
;; 2009/12/29 dadams
;;     Added: bookmarkp-make-bookmark-list-record, bookmarkp-jump-bookmark-list, face
;;            bookmarkp-bookmark-list.
;;     *-bmenu-propertize-item: Highlight bookmark-list bookmarks.
;;     *-bmenu-refresh-menu-list: Set bookmarkp-latest-bookmark-alist to refreshed list.
;;     Face *-local-directory: Made dark background version the inverse of light.
;;     *-bmenu-list-1: Use eq, not equal, test for bookmarkp-omitted-alist-only as filter fn.
;;     *-bmenu-define(-full-snapshot)-command: Include bookmarkp-bmenu-omitted-list in saved state.
;; 2009/12/26 dadams
;;     Added: bookmarkp-bmenu-omitted-list, bookmarkp-bmenu-show-only-omitted, bookmarkp-unomit-all,
;;            bookmarkp-bmenu-omit/unomit-marked, bookmarkp-bmenu-(un-)omit-marked,
;;            bookmarkp-omitted-alist-only.
;;            Bind *-bmenu-omit/unomit-marked, *-bmenu-show-only-omitted, *-unomit-all to O>,OS,OU.
;;     Added omit/un-omit stuff to Bookmark+ menu.
;;     bookmarkp-remove-assoc-dups, bookmarkp-sort-and-remove-dups: Added optional arg OMIT.
;;     bookmark-delete: Update bookmarkp-bmenu-omitted-list.
;;     bookmarkp-save-menu-list-state, bookmark-bmenu-list:
;;       Save/restore bookmarkp-bmenu-omitted-list as part of state.
;;     bookmark-bmenu-list-1: Treat omitted list when bookmarkp-omitted-alist-only.
;;     bookmarkp-marked-bookmark-p: Arg can now be a bookmark (or a bookmark name).
;;     bookmarkp-bmenu-unmark-all: Start by going forward 2 lines, not 1, if user hit RET.
;;     bookmarkp-bmenu-make-sequence-from-marked:
;;       Added optional arg DONT-OMIT-P.  If nil, omit marked bookmarks.
;;       If the seq bookmark already exists, prompt to add to it or replace it.
;;       Go to the new bookmark in the menu list.
;; 2009/12/15 dadams
;;     Added: bookmarkp-sequence-jump-display-function, bookmarkp-sequence, bookmarkp-function,
;;            bookmarkp-bmenu-make-sequence-from-marked, bookmarkp-jump-sequence,
;;            bookmarkp-sequence-bookmark-p, bookmarkp-make-function-bookmark,
;;            bookmarkp-jump-function, bookmarkp-function-bookmark-p.
;;     bookmarkp-describe-bookmark: Update for sequence and function bookmarks.
;;     bookmark-bmenu-list: Use condition-case when read from bookmarkp-bmenu-state-file.
;;                          Bind emacs-lisp-mode-hook to nil.
;;     bookmark-bmenu-surreptitiously-rebuild-list: Use save-current-buffer.
;;     bookmarkp-bmenu-propertize-item: Add faces to function and sequence bookmarks.
;;     bookmarkp-bmenu-menubar-menu: Add *-make-sequence-*-from-marked, *-make-function-bookmark.
;;     bookmark--jump-via: Call *-record-visit with BATCH arg, to preserver point in menu list.
;;     bookmark-bmenu-list-1: fit-frame only if buffer is *Bookmark List*.
;; 2009/12/13 dadams
;;     *-alist-only: Call bookmark-maybe-load-default-file.
;; 2009/12/11 dadams
;;     Added: bookmarkp-list-defuns-in-commands-file, bookmarkp-remove-dups.
;; 2009/12/06 dadams
;;     Added: bookmarkp-bmenu-mouse-3-menu (bound to mouse-3),
;;            bookmarkp-bmenu-(menubar|define-command|sort|show|tags|mark)-menu. 
;;     bookmark-bmenu-delete: Remove newly flagged bookmark from bookmarkp-bookmark-marked-list.
;;     bookmarkp-define-tags-sort-command: Save macroexpanded definition in
;;                                         bookmarkp-bmenu-commands-file.
;; 2009/12/04 dadams
;;     Added: bookmarkp-bmenu-define-full-snapshot-command (bound to C),
;;            bookmarkp-define-tags-sort-command.
;;     bookmarkp-bmenu-mark-bookmarks-tagged-regexp: Removed extra forward-line if we mark line.
;; 2009/12/03 dadams
;;     Added: bookmarkp-bmenu-define-command (bound to c), bookmarkp-bmenu-commands-file.
;;     bookmark-bmenu-list: Read bookmarkp-bmenu-commands-file.
;;     bookmarkp-sort-and-remove-dups: Bug fix - return the list even when null sort function.
;; 2009/11/01 dadams
;;     Added: *-bmenu-check-position (redefinition), bmkext-jump-* defaliases.
;;     *-(w3m|man|gnus)-bookmark-p: Recognize the aliases.
;;     *-jump-man: Bind Man-notify-method.
;;     *-bmenu-goto-bookmark-named: Check the text property, instead of searching.
;;     *-bmenu-bookmark: Wrap in condition-case.
;; 2009/10/31 dadams
;;     Added: bookmark-bmenu-list-1. bookmarkp-toggle-saving-menu-list-state (C-t),
;;            bookmarkp-bmenu-state-file, bookmarkp-bmenu-first-time-p,
;;            bookmarkp-last-bmenu-(bookmark|state-file), bookmark-exit-hook-internal
;;            (redefinition), bookmarkp-save-menu-list-state.
;;     bookmark-bmenu-list: Restore menu-list state if appropriate.  Call bookmark-bmenu-list-1.
;;     bookmarkp-bmenu-quit: If *-bmenu-state-file is non-nil, save the state.
;;     bookmark-write-file: Use case, not cond.
;;     bookmark-set: Use command name as default for man-page bookmark name.
;;     bookmark-delete: Update bookmarkp-latest-bookmark-alist.
;; 2009/10/28 dadams
;;     Renamed: bookmarkp-bookmark-marked-p to bookmarkp-marked-bookmark-p
;;              bookmarkp-bmenu-sort-by-gnus-thread to bookmarkp-bmenu-sort-by-Gnus-thread.
;;     Added: bookmarkp-man, bookmarkp-make-(wo)man-record, bookmarkp-jump-(wo)man,
;;            bookmarkp-man-bookmark-p, bookmarkp-bmenu-mark-man-bookmarks,
;;            bookmarkp-bmenu-show-only-man-pages, bookmarkp-man-alist-only.
;;     *-bmenu-propertize-item: Handle (wo)man bookmarks.  Use bookmarkp-info-bookmark-p.
;;     *-regexp-filtered-*: Use bookmarkp-remove-if-not.
;;     *-write-file: Remove text properties from file name also.
;;     *-regexp-filtered-(tags|(bookmark|file)-name)-alist-only: Use *-remove-if-not.
;; 2009/10/26 dadams
;;     Added: bookmarkp-bmenu-mark-*-bookmarks, bmenu-mark-bookmarks-satisfying.
;;     Bound those and *-show-only-* accordingly.
;;     bookmarkp-file-alist-only: Redefined to just use *-file-bookmark-p.
;; 2009/10/25 dadams
;;     bookmarkp-bmenu-propertize-item: Put bookmark name on line as text property.
;;     bookmark-bmenu-bookmark: Get bookmark name from text property bookmarkp-bookmark-name.
;;     Removed: bookmarkp-latest-sorted-alist.
;;     bookmark-bmenu-list: Use bookmarkp-bmenu-title only if defined.
;; 2009/10/21 dadams
;;     Added: bookmarkp-barf-if-not-in-menu-list.  Use in place of its body.
;;     Added: bookmarkp-bmenu-mark-bookmarks-tagged-regexp.  Bound to T m %.
;;     Added: bookmarkp-record-visit.  Use in *--jump-via.  Replaces next two removals.
;;     Removed: bookmarkp-add-or-update-time, bookmarkp-increment-visits.
;;     Renamed: *-record-(end|front|rear)-context(-region)-string'.
;;              New names: bookmarkp-(end-)position-(pre|post)-context(-region).
;;     *-bmenu-describe-this-bookmark: Added *-barf-if-not-in-menu-list.
;;     *-bmenu-(un)mark-all, *-bmenu-regexp-mark, *-bmenu-toggle-marks:
;;       Removed with-current-buffer.
;; 2009/10/20 dadams
;;     Added: bookmarkp-bmenu-filter-function, bookmarkp-bmenu-title.
;;     Removed: bookmarkp-bmenu-called-from-inside-p.
;;     *-bmenu-list:
;;       Removed TITLE arg.  Get title from bookmarkp-bmenu-title or default.
;;       Use interactive-p and absence of menu list, not *-bmenu-called-from-inside-p, as the
;;         criterion for removing marks.  Fixes bugs such as bookmark creation removing marks.
;;     *-define-sort-command, *-bmenu-execute-deletions, *-increment-visits,
;;       *-add-or-update-time, *-bmenu-show-only-*, *-bmenu-show-all,
;;       *-bmenu-refresh-menu-list, *-bmenu-toggle-show-only-(un)marked,
;;       *-bmenu-filter-alist-by-regexp, *-bmenu-reverse(-multi-sort)-order,
;;       *-bmenu-change-sort-order:
;;         Do not bind or set *-called-from-inside-p.
;;     *-bmenu-show-only-*, *-bmenu-show-all, *-bmenu-filter-alist-by-regexp:
;;       Set *-bmenu-filter-function, *-bmenu-title.
;;     *-bmenu-show-all:
;;       Set *-latest-bookmark-alist to bookmark-alist.
;;     *-bmenu-refresh-menu-list: Fix so that it in fact refreshes.
;;       Do not use *-bmenu-surreptitiously-rebuild-list and *-bmenu-check-position.
;;       Bind bookmark-alist to last alist (filtered or not), and call *-bmenu-list.
;;     *-bmenu-surreptitiously-rebuild-list:
;;       Do not use save*-excursion.  Do not get current title and pass it to *-bmenu-list.
;;     *-file-alist-only:
;;       Removed optional arg.  We have *-local-file-alist-only for that.
;;     *-regexp-filtered-alist-only, *-bmenu-filter-alist-by-regexp:
;;       Remove REGEXP arg - use bookmarkp-bmenu-filter-pattern.
;;     *-bmenu-filter-incrementally:
;;       Raise error if not in *Bookmark List*.
;;       Use just bookmarkp-bmenu-filter-alist-by-regexp in timer - pass no regexp arg.
;;     Added: bookmarkp-some, *-bmenu-filter-(file-name|tags)-incrementally,
;;            *-bmenu-filter-alist-by-(file-name|tags)-regexp,
;;            *-regexp-filtered-(file-name|tags)-alist-only.
;;     Renamed: *-bmenu-filter-incrementally to *-bmenu-filter-bookmark-name-incrementally,
;;              *-bmenu-filter-alist-by-regexp to *-bmenu-filter-alist-by-bookmark-name-regexp,
;;              *-regexp-filtered-alist-only to *-regexp-filtered-bookmark-name-alist-only.
;;     Bound these commands to P B, P F, and P T.  Updated bookmark-bmenu-mode doc string.
;; 2009/10/18 dadams
;;     Added: *-bmenu-filter-(incrementally|delay|prompt|pattern|timer|alist-by-regexp),
;;            *-bmenu-read-filter-input, *-regexp-filtered-alist-only,
;;            *-bmenu-cancel-incremental-filtering.
;;     *-bmenu-execute-deletions: Don't update menu list if this is a no-op.
;;     Updated Commentary.
;;     Thx to Thierry Volpiatto.
;;     Added: *-marked-cp, *-bmenu-sort-marked-before-unmarked.  Bound to s >.
;;     *-define-sort-command: Use copy-sequence for default value.
;; 2009/10/17 dadams
;;     Added: *-read-tags-completing, *-set-union, *-tag-history, *-describe-bookmark,
;;            *-bmenu-describe-this-bookmark.  Bound *-bmenu-describe-this-bookmark to C-h RET.
;;     Use *-read-tags-completing instead of *-read-tags.
;;     *-sort-orders-for-cycling-alist: Use copy-sequence.
;;     *-bmenu-change-sort-order: Use member, not memq.
;;     *-get-bookmark: Handle case of non-string, non-cons. Document NOERROR in doc string.
;;     *-bmenu-execute-deletions: Fix so marks aren't removed if when delete.  Thx to Thierry.
;;     Convert recorded time to an Emacs time spec:
;;       *-make-record-default, -add-or-update-time: Use current-time, not bookmark-float-time.
;;       *-get-visit-time: Convert a deprecated time entry to an Emacs time spec.
;;       *-bookmark-last-access-cp: Convert recorded time to a number for comparison.
;;     Added: *-bmenu-show-filenames (redef of vanilla: put props on whole line, fit frame).
;;     Removed: old-bookmark-insert-location.
;;     *-insert-location: Do not call original.  Redefined: do not add text properties.
;;     *-bmenu-list, *-bmenu-hide-filenames: Put properties on line up to max width.
;;     *-bmenu-goto-bookmark-named: Allow trailing whitespace, since we use the whole line now.
;;     *-bmenu-list: Use pop-to-buffer, not switch-to-buffer.  Use do-list, not mapcar.
;;     *-bmenu-hide-filenames: fit-frame-if-one-window.
;;     *-bmenu-propertize-item: Better help-echo text.
;;     Updated bookmark-alist doc string to mention visits, time, and tags entries.
;; 2009/10/16 dadams
;;     Added tags feature.
;;       Added: *-(get|read)-tags, *-has-tag-p, *-remove(-all)-tags(-from-all),
;;              *-bmenu-remove-tags-from-marked, *-add-tags(-to-marked), *-rename-tag,
;;              *-bmenu-(un)mark-bookmarks-tagged-(all|none|some|not-all),
;;              *-bmenu-mark/unmark-bookmarks-tagged-(all/none|some/not-all).
;;       Bound to prefix key T.
;;       *-bmenu-mode: Updated doc string.
;;     Added: bookmarkp-default-bookmark-name.  Use as default instead of *-current-bookmark.
;;     Renamed: *-maybe-save-bookmark to *-maybe-save-bookmarks.
;;     Menu-list commands: Raise an error if command is used outside the menu list.
;; 2009/10/15 dadams
;;     Added: *-bmenu-(search|query-replace)-marked-bookmarks-regexp.  Bound to M-a, M-q.
;;     Renamed: *-non-marked-bookmarks-only to *-unmarked-bookmarks-only,
;;              *-bookmark-marked-alist to *-bmenu-marked-bookmarks.
;;     *-increment-visits, *-add-or-update-time:
;;       Set *-bmenu-called-from-inside-p to t, so we don't remove marks.
;;     Redefined *-bmenu-bookmark to get name from *-latest-sorted-alist.  Thx to Thierry V.
;;       *-bmenu-surreptitiously-rebuild-list, *-bmenu-list:
;;         Removed optional arg DONT-TOGGLE-FILENAMES-P.
;;       *-bmenu-execute-deletions, *-bmenu-toggle-show-only-(un)marked, *-bmenu-(un)mark-all,
;;         *-bmenu-regexp-mark, *-bmenu-toggle-marks:
;;           Do not bother with *-bmenu-toggle-filenames and rebuilding the menu list.
;; 2009/10/14 dadams
;;     Added: *-bmenu-delete (redefinition), *-isearch-bookmarks,
;;            *-bmenu-isearch(-marked)-bookmarks(-regexp), *-isearch-next-bookmark-buffer.
;;            Bound multi-isearch commands to M-s a C(-M)-s.
;; 2009/10/13 dadams
;;     Added: *-make-dired-record, *-jump-dired, *-dired-bookmark-p, *-dired-alist-only,
;;            *-bmenu-show-only-dired.  Bound *-bmenu-show-only-dired to M-d.
;;     bookmarkp-file-bookmark-p: Include bookmarks that have the Dired handler.
;;     Moved *-sort-orders-for-cycling-alist defcustoms after *-define-sort-command calls.
;;     Call bookmarkp-msg-about-sort-order only when interactive.
;;     *-add-or-update-time, *-increment-visits: Do not save each time we access a bookmark.
;;     Updated doc string of bookmark-alist and Commentary.
;; 2009/10/09 dadams
;;     Added: bookmarkp-bmenu-delete-marked.  Bound it to D.
;;            bookmarkp-sort-orders-for-cycling-alist.
;;     Renamed: bookmarkp-sort-functions-alist to bookmarkp-sort-orders-alist,
;;              bookmarkp-sort-function to bookmarkp-sort-comparer.
;;     bookmark-bmenu-execute-deletions: Added optional arg, for *-bmenu-delete-marked.
;;     *-sort-function: Changed default value to sorting by bookmark type (`s k').
;;     *-bmenu-change-sort-order: Use *-sort-orders-for-cycling-alist, not all sort orders.
;;     Updated Commentary and doc string (bookmark-bmenu-mode).
;; 2009/10/08 dadams
;;     Added: *-bmenu-sort-by-(w3m-url|gnus-thread), *-(gnus|w3m)-cp, *-cp-not,
;;            *-local-file-(accessed|updated)-more-recently-cp, *-bmenu-sort-by-bookmark-type.
;;     Renamed: *-bmenu-sort-by(-last)-file-(size|type|access|update) to
;;              *-bmenu-sort-by(-last)-local-file-(size|typeaccess|update),
;;              *-file-visited-more-recently-cp to *-local-file-accessed-more-recently-cp,
;;              *-file-(size|type)-cp to *-local-file-(size|type)-cp.
;;     Removed: *-file-(device|gid(-chg)|inode|last-(access|update|status-change)|links|modes
;;                            |uid)-cp.
;;     Bound *-bmenu-sort-by-bookmark-type to `s k'.
;;     *-define-file-sort-predicate: Use *-file-bookmark-p, not *-local-file-bookmark-p.
;;     *-msg-about-sort-order: Added optional arg PREFIX-ARG.  Use in: *-show-(all|only-*).
;; 2009/10/07 dadams
;;     Renamed: *-bmenu-sort-by-last-visit-time to *-bmenu-sort-by-last-bookmark-access,
;;              *-bmenu-sort-by-visit-frequency to *-bmenu-sort-by-bookmark-visit-frequency,
;;              *-visited-more-recently-cp to *-bookmark-last-access-cp.
;; 2009/10/06 dadams
;;     Added: bookmarkp-msg-about-sort-order.
;;     bookmark-completing-read: Simple sort when use menu-bar menu.
;; 2009/10/05 dadams
;;     Added: *-make-plain-predicate, *-reverse-multi-sort-order, *-multi-sort,
;;            *-define-file-sort-predicate, *-bmenu-sort-by-file-*, *-file-attribute-*-cp,
;;            and aliases *-file-*-cp, *-current-sort-order.
;;     Redefined sorting to allow multi-predicates:
;;       Redefined: *-sort-function, *-sort-and-remove-dups, *-define-sort-command,
;;                  *-sort-functions-alist.
;;     Bound keys with `s f' prefix to file-sorting commands
;;     *-current-sort-order: Use rassoc instead of rassq now.
;;     Swap keys s and S.  S is now bookmark-bmenu-save.  s is not the sorting prefix key.
;;     bookmark-bmenu-mode: Mention S key explicitly here (even though it is also
;;                          mentioned in the vanilla part of the doc string).
;; 2009/10/04 dadams
;;     *-bmenu-change-sort-order-repeat: Require repeat.el.
;;     Renamed: bookmarkp-current-sec-time to bookmarkp-float-time.
;;     *-float-time: Added arg, so it's the same as float-time (for Emacs 20).
;;     Bind *-reverse-sort-order to `S R'.
;;     *-remote-file-bookmark-p: Removed extra rem-file in last and.
;;     *-non-file-bookmark-p: Ensure it's not a remote file, before calling file-exists-p.
;; 2009/10/03 dadams
;;     Added: bookmarkp-file-remote-p, bookmarkp-buffer (face).
;;     bookmarkp-non-file (face): Changed to gray.
;;     *-default-handler, *-bmenu-propertize-item, *-(info|file)-bookmark-p:
;;       Support Emacs 20-21 Info-node bookmarks.
;;     bookmarkp-bmenu-propertize-item: Use different face for existing buffers.
;;                                      Use bookmarkp-non-file-filename.
;;     bookmarkp-non-file-bookmark-p: Include buffer bookmarks for nonexistent buffers.
;;     bookmarkp-remote-file-bookmark-p: Use bookmarkp-file-remote-p.
;;     bookmark-handle-bookmark:
;;       Redefine for all Emacs versions.  Handle buffer (non-file) bookmarks.
;;     Reordered some function definitions.
;; 2009/10/02 dadams
;;     Added: bookmarkp-bmenu-goto-bookmark-named, bookmarkp-latest-sorted-alist.
;;     *-sort-and-remove-dups: Set *-latest-sorted-alist (not used yet).
;;     *-define-sort-command, *-bmenu-change-sort-order, *-reverse-sort-order:
;;       Bind *-bmenu-called-from-inside-p to t, to prevent losing marks.
;;       Restore cursor position to same bookmark after sorting - use *-goto-bookmark-named.
;;     *-bmenu-surreptitiously-rebuild-list, *-bmenu-list: Added arg DONT-TOGGLE-FILENAMES-P.
;;     *-bmenu-execute-deletions, *-bmenu-toggle-show-only-(un)marked:
;;       Call *-surreptitiously-* with arg DONT-TOGGLE-FILENAMES-P.
;;     *-bmenu-hide-filenames: Simplify - don't get to position by searching backward.
;;     *-handle-region-default: Use forward-line, not goto-line.
;;     Thx to Thierry V.
;; 2009/10/01 dadams
;;     Added: bookmarkp-some-unmarked-p.
;;     Renamed: *-bmenu-toggle-show-only-<TYPE> to *-bmenu-show-only-<TYPE>,
;;              *-bmenu-called-from-inside-flag to *-bmenu-called-from-inside-p.
;;     bookmarkp-some-marked-p: Do not test bookmarkp-bookmark-marked-alist.
;;                              Arg must be required (explicit).  Changed calls accordingly.
;;     bookmark-bmenu-mode: Cleaned up doc string.
;;     bookmark-bmenu-((un)mark|rename|edit-*|toggle-marks|surreptitiously-rebuild-list),
;;       bookmarkp-root-or-sudo-logged-p, bookmarkp-jump-w3m-(new-session|only-one-tab),
;;       bookmarkp-some-marked-p:
;;         Inline let vars used only once.
;;     bookmarkp-bmenu-toggle-show-only-marked:
;;       Test bookmarkp-some-unmarked-p, not bookmarkp-some-marked-p,
;;            and include *-before-hide-unmarked in the test.
;;     bookmarkp-bmenu(-toggle)-show-only-*: Display status message.
;;     bookmarkp-bmenu-toggle-show-only-(un)marked: Fit frame.
;;     bookmark-prop-set: Fixed, so it handles old bookmark format also.
;; 2009/10/01 Thierry Volpiatto
;;     Removed: bookmarkp-bmenu-restore-marks.
;;     bookmark-bmenu-list:
;;       Do the mark restoration in line, at the same time as the annotation * restoration.
;;       Simplify use of START and END.
;; 2009/09/30 dadams
;;     bookmarkp-bmenu-regexp-mark: Remove binding of bookmark-alist.
;;     bookmark-bmenu-(un)mark, bookmarkp-bmenu-edit-bookmark (remove first call only),
;;       bookmark-bmenu-other-window, bookmark-bmenu-rename, bookmarkp-bmenu-restore-marks:
;;         Remove bookmark-bmenu-check-position (done by bookmark-bmenu-bookmark anyway).
;;     bookmark-insert-location: Fix interactive spec for Emacs < 22.
;;     bookmark-location: Return "" instead of raising error, if no location found.
;;     bookmarkp-current-sec-time: Move the let: do not call current-time unless we need to.
;;     bookmarkp-bmenu-unmark-all: forward-line only 1, not 2.  Thx to Thierry.
;;     bookmark-bmenu-mode: Updated doc string - bindings and mention options.
;;     bookmarkp-bmenu-propertize-item: For buffer, check also against "   - no file -".
;; 2009/09/29 dadams
;;     bookmark-bmenu-unmark: Use delete, not remove.
;;     Removed: bookmark-bmenu-check-position, bookmarkp-maybe-sort.
;;     Added: bookmarkp-sort-and-remove-dups, bookmarkp-remove-assoc-dups,
;;            bookmarkp-face-prop, bookmarkp-bad-bookmark, bookmark-menu-heading (Emacs 20,21),
;;            bookmark-bmenu-bookmark (redefinition).
;;     *-bmenu-toggle-show-only-*: Do not call-interactively.
;;     bookmarkp-bmenu-(un)mark-all:
;;       Handle bookmark-bmenu-toggle-filenames (wrapper).
;;       Remove bookmark-bmenu-check-position - just ensure within menu list.
;;     bookmarkp-bmenu-mark-all: Move save-excursion so it applies to all movements.
;;                               Message stating number marked.
;;     bookmarkp-bmenu-unmark-all: Use with-current-buffer ensure in menu list.
;;                                 Use bookmark-bmenu-unmark.
;;     Fixed U bindings for bookmarkp-bmenu-unmark-all.
;;     bookmarkp-bmenu-regexp-mark:
;;       Remove bookmark-bmenu-check-position - just ensure in menu list.
;;     bookmarkp-bmenu-toggle-marks: Use forward-line 2, to ensure in menu list.
;;                                   Message stating number marked.
;;     bookmark-bmenu-list, bookmarkp-bmenu-propertize-item: Use bookmarkp-face-prop.
;;     bookmark-bmenu-list: Don't start applying the faces until column 2.
;;     Removed key bindings in bookmark-map for *-toggle-show-only-*.
;;     Redefined faces, esp. for a light background.
;;     Use font-lock-face or face property, depending on Emacs version.
;;
;; 2009-06-09 to 2009-09-27 Thierry Volpiatto and dadams
;;     New features, as follows.
;;       See also the change log at
;;         http://mercurial.intuxication.org/hg/bookmark-icicle-region/.
;;       2090-09-27 Rewrote sorting and unmarking code.  (+ Updates to doc, names.)
;;                    Unmarking is now like Dired & query-replace.
;;                    Sorting is via one sort function; sort predicates do all the sorting.
;;                    Can now cycle sort orders with S S S...
;;                    Sort cmds now cycle among normal, reverse, off.
;;                    Add: *-define-sort-command (macro), *-assoc-delete-all, *-upcase,
;;                         *-get-visits-count, *-get-visit-time, *-sort-functions-alist.
;;                  Remove docstring from defalias (for Emacs 20).
;;       2009-09-26 Fix *-bmenu-mode doc (defadvice).
;;       2009-09-25 *-bmenu-edit, *-bmenu-sort-1: Fix bmk retrieval code.
;;                  Redefine *-bmenu-unmark.  Add: *-bmenu-toggle-marks.
;;                  Bind *-bmenu-unmark-all-bookmarks to M-DEL.  Reorder code.
;;                  Rename: *-bmenu-unmark-all-(bookmarks|(delete|mark)-flag)',
;;                          *-bmenu-unmark-all-bookmarks-1.
;;                  Change sort predicates in defalias.  Rename bmk entry visit to visits.
;;                  Remove: *-bmenu-show-number-of-visit.
;;       2009-09-22 Rewrote sorting code.  Renamed unmarking fns.
;;       2009-09-21 Rename mark/unmark cmds to have -bmenu.
;;                  Add: *-bmenu-called-from-inside-flag - set it in (un)hide marks fns.
;;       2009-09-20 *-write-file: Remove text properties before saving.
;;                  Remove all marks only in current display.
;;       2009-09-19 *-current-sec-time: Protect with fboundp for Emacs 20.
;;                  *-bmenu-sort-1: Add msg showing sort method.
;;                  Change key S-A to S-S (A is annotations).
;;       2009-09-18 Improve sorting by visit frequency.  Always increment when jump.
;;                  Fix increment visit fn.  Allow sorting by last visited.
;;                  When visit values are equal, sort with string-lessp.
;;                  Add TIME bookmark-alist entry.  *-make-record-default: Add time entry.
;;                  Fix: bad parens, errors in sorting predicate.  Rename fns.  
;;                  Use single fn to sort using diff methods.
;;                  Add: *-bmenu-refresh-alist (bind to g).
;;       2009-09-16 Add: *-toggle-sorting-by-most-visited, *-reset-visit-flag,
;;                       *-bmenu-show-number-of-visit.
;;                  Redefine *-prop-set.  Improve *-toggle-sorting-by-most-visited.
;;                  Add auto-doc to header.  *-bmenu-mode: Add missing key.
;;                  Update menu-list after jumping.
;;                  Increment save counter when jump with visit flag.
;;       2009-09-15 Record number of visits.  Added sorting by most visits.
;;       2009-09-14 Add doc string.  Update defadvice doc string wrt keys.
;;       2009-09-12 Add: fns to mark all, unmark D or > or all, *-bmenu-reset-alist.
;;                  Fix keymap (Emacs 20).  *-unmark-all-bookmarks1: Move the save-excursion.
;;       2009-09-11 Add: *-bmenu-check-position (redef to improve performance),
;;                       *-unmark-all-bookmarks, *-current-list-have-marked-p,
;;                       *-bookmark-marked-p, *-(non-)marked-bookmarks-only.
;;                  *-bmenu-hide-unmarked: Add toggling.  Restore lost fn.
;;                  Reorder code.  Bind cmds in *-bmenu-mode-map.
;;                  *-bmenu-hide-marked: Do not hide if no marked in current filter.
;;                  Improve: *-bmenu-hide(-not)-marked-bookmark, (un)hide marked fns.
;;       2009-09-10 Fix *--restore-all-mark, *-bmenu-regexp-mark.
;;                  *-bmenu-list: Add *-restore-all-mark.
;;                  *-bmenu-mark: Push marked bmk to marked list.
;;                  Add: bookmarkp-bookmark-marked-list, *-bmenu-quit.
;;       2009-09-09 *-maybe-sort-alist: Use copy-sequence.
;;                  So remove fixes for *-rename, *-edit-bookmark.
;;                  *-yank, *-rename', *-set: Fix yanking.
;;                  Remove non-file bmks from file-only list.
;;                  Add: *-bmenu-list-only-non-file-bookmarks, *-maybe-message (Emacs 20),
;;                       *-bmenu-mark-bookmark-matching-regexp, *-bmenu-hide-marked-bookmark,
;;                       *-bmenu-hide-not-marked-bookmark, *-bmenu-mark (redefinition).
;;                  *-write-file: Improve performance.
;;                  *-non-file-alist-only: Remove unused def.
;;                  Fix: hide marked/unmarked with toggle-filenames, keymap for Emacs 20.
;;                  Improve comments, doc strings.
;;                  *-bmenu-mark-bookmark-matching-regexp: Fix while loop.
;;       2009-09-08 bookmark-store: Remove duplicate setq of *-current-bookmark.
;;       2009-09-07 Reorganize (reorder), add comments, improve doc strings.
;;                  Change binding of *-bmenu-relocate from R to M-r.
;;       2009-09-06 bookmark-rename: Redefine with new arg BATCH.
;;                  *-bmenu-rename: Go to new pos, not old.
;;                  *-edit-bookmark, bookmark-rename: Fix display update for Emacs 20.
;;       2009-09-05 Add: *-edit-bookmark, *-bmenu-edit-bookmark, *-maybe-save-bookmark.
;;       2009-09-04 Require cl.  Allow RET in Emacs 20.  Add doc string.
;;                  *-fix-bookmark-alist-and-save:
;;       2009-09-03 Fix *-fix-bookmark-alist-and-save:
;;                    Use error, not message.  Change value for setcdr.
;;                    Do not use push with non-var (cl).
;;                  bookmark-delete:
;;                    Redefine, to fix vanilla bug: increment count even when batch.
;;                  *-non-file-name: Change to - no file -.  *-bmenu-list: Add arg FILTER-ON.
;;                  *-bmenu-execute-deletions: Use delete, not remove.
;;                  Add: *-replace-regexp-in-string.
;;                  bookmark-set: Fix *-yank-point for region case.  Fix bad parens.
;;       2009-09-02 Add: *-non-file-filename.  *-fix-bookmark-alist-and-save: Fix msg.
;;                  Require cl (gensym).  *-at-bol/eol' -> line-*-position (for Emacs 20).
;;                  bookmark-delete: increment *-count if batch arg (fixes vanilla bug).
;;                  Redefine *-bmenu-execute-deletions,
;;                           *-bmenu-surreptitiously-rebuild-list. 
;;                  Update current filtered display - do not reload & display all bmks.
;;                  Add redefinitions of *-bmenu-rename', *-yank-word to fix vanilla bugs:
;;                    *-bmenu-rename: Do not call *-bmenu-list twice.
;;                  *-yank-word: Do not insert whitespace.
;;                  Rename *-last-bookmark-alist-in-use to *-latest-bookmark-alist.
;;       2009-09-01 Fix: Loading of new doc for bookmark-alist (add vacuous defvar).
;;                       *-bmenu-(list|hide-filenames): start -> end, end -> start.
;;                  Removed extraneous quote mark that caused problems.
;;                  Save only if condition-case exits OK.
;;       2009-08-31 Fixes: Test for non-file bmk.  Filename for Gnus bmks.
;;                         Compatibility of bookmark-alist with vanilla Emacs.
;;                         Require info.el and ffap.el when needed.
;;                  Add: *-line-number-at-pos (for Emacs 20),
;;                       *-bmenu-other-window (redefinition).
;;                  Rename *-propertize-bookmark-list to *-propertize-bmenu-item.
;;       2009-08-30 Fix: Increment *-alist-modification-count when relocate region,
;;                       and maybe save.
;;                  Move code adding properties to bookmarks out of *-bmenu-list.
;;                  mapc -> mapcar.  *-bmenu-hide-filenames: Redefine.
;;       2009-08-29 Remove refresh code.
;;       2009-08-27 Added: *-local-directory-bookmark-p, *-local-file-alist-only,
;;                         *-non-file-alist-only.
;;                  *-file-bookmark-p: Redefined to exclude bmks with handlers.
;;                  Renamed fns and faces.
;;       2009-08-25 Fit frame after display menu list.
;;                  Refresh list when toggle filename visibility.
;;       2009-08-24 Fix: *-bmenu-list for remote files, bookmark-set, *-remote-file-bookmark-p.
;;                  Ensure arg to file-remote-p is not nil.
;;                  Recenter region only if it is visible.
;;       2009-08-23 Remove old *-location.  *-bmenu-list: Add isw3m.
;;                  bookmark-set:
;;                    Redefine for older Emacs. Use a default prompt for gnus, w3m.
;;                    Use *-name-length-max for title when region is active.
;;                    Ensure bookmark is on one line.
;;       2009-08-22 Try to handle tramp ftp files.
;;                  Do not fail if bookmark has empty filename entry.
;;                  Show region end pos using exchange-point-and-mark.
;;       2009-08-21 Remove all cl stuff (list*, loop, etc.).  Add *-remove-if(-not).
;;                  Remove compile-time require of cl.
;;                  Add predicates *-(region|gnus|w3m|info|(remote-|local-)file)-bookmark-p.
;;                  Redefine alist-only functions to optimize and use new preds.
;;       2009-08-20 *--jump-via: Fix to show relocated region before ask whether to save.
;;                  *-relocate-region: Fix ar-str.  Rename a var.  Add: *-region-activated-p.
;;                  Revert chgs.
;;       2009-08-19 Update/fix commentary: bookmark-alist, linkd.
;;                  *-default-handler, *-handle-region-default:
;;                    Get bmk record from name only once.
;;                  *-save-new-region-location: Move t inside progn.
;;       2009-08-16 Use prefix bookmarkp where appropriate.
;;       2009-08-15 Improve comments, doc strings.  Rename fns, vars.
;;                  Add :group with prefix bookmarkp.
;;       2009-08-09 Fix doc strings.
;;       2009-08-08 bookmark-set: Update doc string.  Show keys in C-h m.
;;       2009-08-07 *-jump: Fix to jump in same window (thx to Henry Atting).
;;                  *-at-bol/eol' -> line-*-position.
;;       2009-08-01 bookmark-store: Fix for Emacs 20.
;;       2009-07-27 Ensure we start on an empty w3m buffer.  Add: *-w3m-allow-multi-tabs.
;;       2009-07-24 *-bmenu-mode-map: Define some new keys.
;;       2009-07-23 *-bmenu-list: Add path to file in help-echo.
;;       2009-07-19 Fix title underline.  Highlight bookmark if root logged with tramp.
;;                  Add face for sudo.
;;       2009-07-18 Add: filter functions, option for bookmark-bmenu-list.
;;                  Remove toggle region.
;;                  *-bmenu-list-only-files-entries: Add prefix arg to show remote.
;;       2009-07-14 Add a forgotten test.
;;       2009-07-13 Fix errors.  Give pos in msg even if no search.
;;                  *-from-bob/eob: Fixed like old strict.
;;                  Remove *-relocate-region-(method|from-limits).
;;                  Remove unused code in record fns.  Add: *-relocate-region-function.
;;       2009-07-12 Do not pass args to relocation routines.  Remove use of flet.
;;                  Use skip-chars-*ward.  Use forward-line, not beginning-of-line.
;;                  Remove save-excursion around message.  Correct typo f(ree var BMK).
;;       2009-07-11 Fix *-relocate-region-strict.  Rename fns, adjust defcustom.
;;                  Save relocated region after pushing mark.  Add comments.
;;       2009-07-10 New retrieve fn.  Add looking-* fns.
;;       2009-07-08 Simplify record fns.  Add doc strings.  Add: *-save-relocated-position.
;;                  Fix: updating when relocate (wrt new record fns), string closing,
;;                       free vars, parens, names.
;;       2009-07-06 Fix: *-bmenu-list wrt windows, Info colors.
;;       2009-07-04 Rename fns to record, vars, args of retrieve fns.  Big changes, fixes.
;;       2009-07-01 Fix comments.  *-retrieve-region-strict: improve when out of context.
;;       2009-06-30 Fix: free vars, *-retrieve-region, provide (name).
;;       2009-06-29 Fix: w3m handler, file name for non-file, *-simple-retrieve-position.
;;                  Add: *-retrieve-region-strict.
;;       2009-06-28 Add: *-retrieve-region-method-is, *-retrieve-region-lax,
;;                       fns to retrieve regions.
;;                  Use buffer again, not buffer-name.
;;       2009-06-27 Fix wrong-type error no such file.  Renamed faces.  Add: *-prop-set.
;;                  Load gnus at compile time.  Protect tramp-file-name-regexp with boundp.
;;       2009-06-25 Fixes for older Emacs compatibility.
;;       2009-06-24 Improve *-default-handler.
;;                  Add: *-always-save-relocated-position, *-prop-get.
;;       2009-06-23 Use search-regexp, not re-search-regexp.  Add Gnus bmks.  Add doc.
;;                  Fix *-bmenu-list.
;;       2009-06-21 Fix: *-default-handler for Info.  Improve doc strings, commentary.
;;                  Fixes to be compatible with Emacs 20-22.
;;                  Use defcustom for *-list-only-regions-flag.
;;                  *jump: Put prefix arg in interactive spec.  Use buffer-name, not buffer.
;;                  Remove require of Tramp and inline Tramp fns.
;;                  Remove tests for display-(color|mouse)-p.
;;                  w3m-bookmark-(jump|make-record): require w3m.
;;       2009-06-20 Update context strings when relocate.
;;                  Fix: bookmark-get-*: search from point-min.
;;       2009-06-19 Fix: *-make-record-default, *-toggle-use-only-regions, *-default-handler,
;;                       bookmarking Dired.
;;                  Handle 4 positions in *-default-handler.
;;       2009-06-17 Fix: case where some bookmarked text was removed, *-use-region.
;;       2009-06-15 Fix *-region-alist-only, *-get-buffername, *-location,
;;                      non-file (buffer) bookmarks.
;;                  Support w3m similar to Info.
;;       2009-06-14 Fix bookmark+version, region relocation.  Info support.  Error handling.
;;       2009-06-13 Fix: *-list-only-regions, *-region-handler, *-make-record, keymap, faces.
;;                  Put region & info handling in *-default-handler, not separate handlers.
;;                  Merge *-make-record-region to *-make-record-default.
;;                  *-jump now negates *-use-region if prefix arg.  Raise error if bad bmk.
;;       2009-06-12 Add: *-get-endposition, *-region-alist-only-names.
;;                  Add filter to show only region bookmarks.
;;                  Faces for menu list.  Change region color.
;;       2009-06-11 Add: *-region-search-size, *-get-buffername, *-use-region.
;;                  Redefine *-handle-bookmark, *-jump, to fit bookmark-use-region.
;;                  Add condtions to bookmark-make-record.  Support w3m.  Support t command.
;;       2009-06-10 Fix search regexp.  Fix region in Info. Save bookmark if region moves.
;;       2009-06-09 Added: bookmark-make-record(-region), bookmark-region-handler.
;;                  Relocation.
;; 2009/05/25 dadams
;;     Added redefinition of bookmark-get-bookmark-record.
;; 2008/10/16 dadams
;;     bookmark-jump-other-window: Don't define it for Emacs 23+ (not needed).
;; 2008/04/04 dadams
;;     bookmark-jump-other-window: Updated wrt Emacs 22.2.
;; 2007/10/07 dadams
;;     Added: bookmark-completing-read, bookmark-delete, bookmark-insert(-location),
;;            bookmark-jump, bookmark-relocate, bookmark-rename.
;;     bookmark-jump-other-window: Use new bookmark-completing-read.
;; 2007/07/13 dadams
;;     Replaced Emacs version tests to reflect Emacs 22 release.
;; 2006/03/08 dadams
;;     bookmark-jump-other-window: Handle nil arg.
;; 2005/05/17 dadams
;;     Updated to work with Emacs 22.x.
;; 2004/11/20 dadams
;;     Refined to deal with Emacs 21 < 21.3.50 (soon to be 22.x)
;; 2004/10/26 dadams
;;     Different menu-bar command, depending on Emacs version.
;; 2004/09/21 dadams
;;     Only define bookmark-menu-jump-other-window if < Emacs 22.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;; You need not load this file.  It contains only documentation.

(provide 'bookmark+-doc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bookmark+-doc.el ends here

