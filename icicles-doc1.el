;;; icicles-doc1.el --- Minibuffer input completion and cycling.
;;
;; Filename: icicles-doc1.el
;; Description: Minibuffer completion and cycling.
;; Author: Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 1996-2017, Drew Adams, all rights reserved.
;; Created: Tue Aug  1 14:21:16 1995
;; Last-Updated: Sun Sep 17 10:14:28 2017 (-0700)
;;           By: dradams
;;     Update #: 28601
;; URL: https://www.emacswiki.org/emacs/download/icicles-doc1.el
;; Doc URL: https://www.emacswiki.org/emacs/Icicles
;; Keywords: internal, extensions, help, abbrev, local, minibuffer,
;;           keys, apropos, completion, matching, regexp, command
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x, 24.x, 25.x
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  Icicles documentation, part 1.
;;
;;  Files `icicles-doc1.el' and `icicles-doc2.el' contain the doc for
;;  Icicles, including how to install and use Icicles.  You can also
;;  read the Icicles doc, in formatted form, on the Emacs-Wiki Web
;;  site: https://www.emacswiki.org/emacs/Icicles.  Emacs Wiki also
;;  has a few addtional pages about Icicles.  In particular, if you
;;  are new to Emacs, as well as Icicles, see this page:
;;  https://www.emacswiki.org/emacs/EmacsNewbieWithIcicles.
 
;;(@* "Installing Icicles")
;;
;;  To use this library:
;;
;;    Add this to your initialization file (~/.emacs or ~/_emacs):
;;
;;      (require 'icicles) ; Load this library.
;;      (icicle-mode 1)    ; Turn on Icicle mode.
;;
;;    It is best to add this code *after* any code that creates or
;;    changes key bindings, so Icicles can pick up all of your key
;;    definitions (bindings).  However, if you make new bindings, you
;;    can always exit and then reenter Icicle mode to pick them up.
;;
;;    You will need all of these libraries (loaded by `icicles.el'):
;;
;;     `icicles-chg.el'  (not loaded - change logs only)
;;     `icicles-cmd1.el'
;;     `icicles-cmd2.el'
;;     `icicles-doc1.el' (not loaded - doc only)
;;     `icicles-doc2.el' (not loaded - doc only)
;;     `icicles-face.el'
;;     `icicles-fn.el'
;;     `icicles-mac.el'
;;     `icicles-mcmd.el'
;;     `icicles-mode.el'
;;     `icicles-opt.el'
;;     `icicles-var.el'
;;
;;    The following libraries are recommended but optional (loaded by
;;    `icicles.el' if in your `load-path').  They are enhanced by
;;    Icicles, or Icicles is enhanced by them, or both.  They are all
;;    available at Emacs Wiki,
;;    https://www.emacswiki.org/emacs/ElispArea.
;;
;;     `apropos-fn+var.el' - Apropos enhancements for fns and vars.
;;     `bookmark+.el' etc. - Many bookmark enhancements.  Includes:
;;                           `bookmark+-1.el', `bookmark+-bmu.el',
;;                           `bookmark+-doc.el', `bookmark+-key.el',
;;                           `bookmark+-lit.el', `bookmark+-mac.el'.
;;     `col-highlight.el'  - Required by `crosshairs.el'.  Emacs 22+
;;     `crosshairs.el'     - Highlight target positions.  Emacs 22+
;;     `doremi.el' and
;;     `doremi-frm.el'     - Incremental changes.
;;     `frame-cmds.el'     - Frame and window commands.
;;     `fuzzy-match.el'    - Fuzzy completion (matching).
;;     `hexrgb.el'         - Color manipulation.
;;     `hl-line+.el'       - Required by `crosshairs.el'.  Emacs 22+
;;     `icomplete+.el'     - Enhancements to `icomplete.el'
;;     `info+.el'          - Enhancements to `info'.
;;     `lacarte.el'        - Keyboard access to the menubar menus.
;;     `mb-depth+.el'      - Minibuffer depth indicator.  Emacs 22+
;;     `pp+.el'            - Pretty-printing for `M-:'.
;;     `synonyms.el'       - A hypertext thesaurus.
;;     `thingatpt+.el'     - Better defaults for commands, `M-.'.
;;     `vline.el'          - Required by `crosshairs.el'.  Emacs 22+
;;
;;    Be aware that some of these libraries in turn require other
;;    libraries.  For example, library `frame-cmds.el' requires
;;    library `frame-fns.el'.
;;
;;    Depending on your platform, if you use Icicles in a text
;;    terminal (that is, without a window system/manager), then you
;;    might need to change some of the key bindings, if some of the
;;    default bindings are not available to you.
;;
;;    If on your platform, for example, Emacs in a text terminal does
;;    not recognize a key such as `S-TAB' (as something different from
;;    `TAB'), then you will want to change that key binding.  To
;;    customize Icicles key bindings, see
;;    (@file :file-name "icicles-doc2.el" :to "Customizing Key Bindings").
;;    You might also want to customize some of the Icicles faces,
;;    since a text terminal is sometimes limited in the colors it can
;;    handle.
;;
;;    It is of course best to byte-compile all of the libraries
;;    (except `icicle-chg.el', `icicles-doc1.el', and
;;    `icicles-doc2.el').  You will likely get some byte-compiler
;;    warning messages.  These are probably benign - ignore them.
;;    Icicles is designed to work with multiple versions of Emacs, and
;;    that fact provokes compiler warnings.  If you get byte-compiler
;;    errors (not warnings), then please report a bug, using `M-x
;;    icicle-send-bug-report'.
;;
;;    Whenever you update Icicles (i.e., download new versions of
;;    Icicles source files), I recommend that you do the following:
;;
;;      1. Delete all existing byte-compiled Icicles files
;;         (icicles*.elc).
;;      2. Load Icicles (`load-library' or `require').
;;      3. Byte-compile the source files.
;;
;;    In particular, always load `icicles-mac.el' (not
;;    `icicles-mac.elc') before you byte-compile new versions of the
;;    files, in case there have been any changes to Lisp macros (in
;;    `icicles-mac.el').
;;
;;    After startup, you can turn Icicle mode on or off at any time
;;    interactively, using command `icy-mode' (aka `icicle-mode' -
;;    prefix `icy' is unique to this command, so it is easier to
;;    complete).
;;
;;    Note: If you turn on Icicle mode in your init file, it's
;;    generally best to do so as late as possible - after you or any
;;    libraries that you load do any key binding.  This is because
;;    Icicles uses the current global key bindings to determine which
;;    keys to bind for minibuffer completion and cycling.  To pick up
;;    the latest bindings at any time, you can of course enter Icicle
;;    mode interactively using command `icy-mode' (if necessary, exit,
;;    then re-enter).
;;
;;    Note: Icicles redefines some functions when you are in Icicle
;;    mode (it restores them when you leave Icicle mode).  It
;;    generally does not use `defadvice' to alter the functions; it
;;    redefines them instead.  Because of this, there can be
;;    incompatibilities with other libraries that also change the same
;;    functions (using `defadvice' or otherwise).  An example is Viper
;;    mode.  If you load Viper before Icicles, then you will run into
;;    problems with function `read-file-name' because it is tweaked by
;;    both Viper and Icicles.  If you load Icicles before Viper, you
;;    should not encounter this problem (but you might encounter other
;;    problems: both Icicles and Viper try to control the minibuffer).
 
;;(@* "Index")
;;
;;  Index
;;  -----
;;
;;  If you have library `linkd.el' and Emacs 22 or later, load
;;  `linkd.el' and turn on `linkd-mode' now.  It lets you easily
;;  navigate around the sections of this doc.  Linkd mode will
;;  highlight this Index and render it more readable.  Likewise, for
;;  the cross-references and section headings throughout this file.
;;  You can get `linkd.el' here:
;;  https://www.emacswiki.org/emacs/download/linkd.el.
;;
;;  (@* "Documentation in File `icicles-doc1.el'")
;;  ----------------------------------------------
;;
;;  (@> "Nutshell View of Icicles")
;;    (@> "README for NON-Readers")
;;    (@> "Menus")
;;    (@> "README")
;;    (@> "Flashy Demo to Pique Your Curiosity")
;;      (@> "First Example: Multi-Inputs")
;;      (@> "Second Example: Multi-Completions")
;;      (@> "Third Example: Narrowing a Manual")
;;      (@> "Fourth Example: Tagged Files")
;;
;;    (@> "Top Level to Minibuffer ... and Back Again")
;;    (@> "Toggle Options on the Fly")
;;    (@> "Cycle Completion Candidates")
;;    (@> "Display Completion Candidates")
;;    (@> "Prefix Completion and Apropos Completion")
;;    (@> "Chains of Simple Match Patterns - Progressive Completion")
;;    (@> "Chip Away the Non-Elephant")
;;    (@> "Choose Before You Act")
;;    (@> "Help on Completion Candidates")
;;    (@> "Perform Multiple Operations in One Command")
;;    (@> "Perform Alternative Operations on the Fly")
;;    (@> "Completion Status Indicators")
;;    (@> "Icicles Search")
;;    (@> "Complete Key Sequences Too")
;;    (@> "Available for Almost Any Input")
;;    (@> "Component Icicles Libraries")
;;    (@> "If You Are an Emacs-Lisp Programmer")
;;
;;  (@> "Inserting Text Found Near the Cursor")
;;    (@> "FFAP: Find File At Point")
;;    (@> "Proxy Candidates, `M-.'")
;;    (@> "Repeat `M-.' To Grab More or Different")
;;    (@> "Resolve File Names")
;;
;;  (@> "Background on Vanilla Emacs Input Completion")
;;  (@> "Cycling Completions")
;;  (@> "Traversing Minibuffer Histories")
;;  (@> "Apropos Completions")
;;  (@> "Expanded-Common-Match Completion")
;;  (@> "Progressive Completion")
;;    (@> "`M-*' and `S-SPC': Matching Additional Regexps")
;;    (@> "Successive Approximation...")
;;    (@> "`M-&': Satisfying Additional Predicates")
;;
;;  (@> "Regressive Completion")
;;  (@> "Completion On Demand")
;;  (@> "Moving Between the Minibuffer and Other Buffers")
;;  (@> "Inserting a Regexp from a Variable or Register")
;;  (@> "Special Characters in Input Patterns")
;;  (@> "Exiting the Minibuffer Without Confirmation")
;;  (@> "Ido and IswitchB")
;;  (@> "*Completions* Display")
;;  (@> "Icompletion")
;;    (@> "Using Icicles with Icomplete Mode")
;;    (@> "Icompletion in *Completions*: Apropos and Prefix Completion")
;;    (@> "Incremental Completion (Input Expansion) in the Minibuffer")
;;    (@> "Icicles Highlights the Input that Won't Complete")
;;
;;  (@> "Sorting Candidates and Removing Duplicates")
;;    (@> "Changing the Sort Order")
;;    (@> "Defining New Sort Orders")
;;    (@> "Different Sorts for Different Sorts of Uses")
;;
;;  (@> "A Propos d'Apropos")
;;    (@> "Get Help on Completion Candidates")
;;      (@> "Use Candidate Help Like You Use Emacs Command `apropos'")
;;
;;    (@> "Icicles Apropos Commands")
;;      (@> "Replacements for Standard Apropos Commands")
;;      (@> "Documentation-Apropos Multi-Commands")
;;      (@> "Type-Aware Variable-Apropos Multi-Commands")
;;      (@> "Value-Aware Variable-Apropos Multi-Commands")
;;
;;  (@> "Multi-Commands")
;;    (@> "What Is a Multi-Command?")
;;    (@> "How Does a Multi-Command Work?")
;;
;;  (@> "Multi-Completions")
;;    (@> "Icicles Multi-Completion Commands")
;;    (@> "Mode-Line Lighter Indication of Multi-Completion")
;;    (@> "How Multi-Completions Work")
;;    (@> "Multi-Completions vs `completing-read-multiple'")
;;    (@> "Sorting Candidates by Their Second Part")
;;    (@> "Multi-Completions with a Part You Never See")
;;
;;  (@> "Chapter & Verse: Searching Named Containers")
;;  (@> "Dot, Dot, Dot")
;;
;;  (@> "More about Multi-Commands")
;;    (@> "Alternative Actions")
;;    (@> "Deleting Objects")
;;    (@> "Option `icicle-use-C-for-actions-flag'")
;;    (@> "Accessing Saved Locations (Bookmarks) on the Fly")
;;
;;  (@> "Multi-Inputs")
;;
;;  (@> "Icicles Tripping")
;;    (@> "Highlighting the Destination")
;;
;;  (@> "Key Completion")
;;    (@> "Completing Keys")
;;    (@> "`S-TAB' Is Everywhere - Start With It")
;;    (@> "Completing Keys By Name")
;;    (@> "Completing Prefix Keys")
;;    (@> "Top-Level Key Completion from a Prefix Key")
;;    (@> "Navigate the Key-Binding Hierarchy")
;;    (@> "Local Bindings and Menu Items Are Highlighted")
;;    (@> "Completing Keys By Just Hitting Them")
;;    (@> "Key and Command Help")
;;    (@> "`S-TAB' Is a Multi-Command")
;;    (@> "Possible Source of Confusion")
;;    (@> "Three-Key Emacs")
;;    (@> "Entering Special and Foreign Characters")
;;    (@> "Handling Keymaps That Are Inaccessible From the Global Map")
;;    (@> "Automatic Key Completion")
;;
;;  (@> "Icicles Multi `M-x'")
;;    (@> "Examples of Using Multi `M-x'")
;;      (@> "What about describe-variable and describe-function?")
;;
;;    (@> "Multi `M-x' Turns Every Command into a Multi-Command")
;;
;;  (@> "Choose All Completion Candidates")
;;  (@> "Sets of Completion Candidates")
;;    (@> "Saving and Retrieving Completion Candidates")
;;    (@> "Saving or Retrieving Additional Candidates")
;;    (@> "Different Places for Saving and Retrieving Candidates")
;;    (@> "Set Operations")
;;
;;  (@> "Google Matching")
;;    (@> "Domain of Discourse")
;;    (@> "Global Filtering")
;;    (@> "Word Matching and String Matching")
;;    (@> "AND Matching and OR Matching")
;;    (@> "NOT Matching")
;;
;;  (@> "Buffer-Name Input")
;;
;;  (@> "File-Name Input and Locating Files Anywhere")
;;    (@> "Function `read-file-name'")
;;    (@> "Function `completing-read'")
;;    (@> "Icicles Commands that Read File Names")
;;      (@> "`icicle-file', `icicle-find-file', `icicle-find-file-absolute'")
;;      (@> "Match File Names and File Content Too")
;;      (@> "Visit Recent Files or Files for Emacs Tags")
;;      (@> "Find Files Anywhere, Without Knowing Where")
;;
;;    (@> "Absolute File Names and Different Directories")
;;
;;  (@> "Persistent Sets of Completion Candidates")
;;    (@> "Saving Candidates in Cache Files")
;;    (@> "Filesets and Icicles Saved Completion Sets")
;;    (@> "Improving Performance with Persistent Sets")
;;      (@> "Avoid Remote File-Name Completion")
;;      (@> "Avoid Generating A Large Completion Set")
;;
;;  (@> "Dealing With Large Candidate Sets")
;;  (@> "History Enhancements")
;;    (@> "What Input, What History?")
;;    (@> "Overview of Minibuffer History Enhancements")
;;    (@> "Using Completion to Insert Previous Inputs: `M-o'")
;;    (@> "Putting Previous Candidates First: `C-M-,'")
;;    (@> "Matching Only Historical Candidates: `M-h' and `M-pause'")
;;    (@> "Using Other Histories; Commands Any Which Way")
;;      (@> "Completing Against All Interactive Commands")
;;      (@> "Using an Alternative History")
;;
;;    (@> "Cleaning Up History Lists")
;;
;;  (@> "Isearch Enhancements")
;;    (@> "Content-Matching Pattern as Isearch Regexp")
;;    (@> "Launch Occur using the Isearch Search String")
;;    (@> "Launch Icicles Search using the Isearch Search String")
;;
;;
;;  (@file :file-name "icicles-doc2.el" :to "Documentation in File `icicles-doc2.el'")
;;  -----------------------------------------------------------
;;
;;  (@file :file-name "icicles-doc2.el" :to "Icicles Search Commands, Overview")
;;    (@file :file-name "icicles-doc2.el" :to "Introduction: On Beyond Occur...")
;;    (@file :file-name "icicles-doc2.el" :to "Icicles Search Key Bindings")
;;    (@file :file-name "icicles-doc2.el" :to "How Icicles Search Works")
;;    (@file :file-name "icicles-doc2.el" :to "Why Use 2 Search Patterns?")
;;    (@file :file-name "icicles-doc2.el" :to "Search Outside the Defined Search Contexts?")
;;    (@file :file-name "icicles-doc2.el" :to "Search Multiple Buffers, Files, and Bookmarks")
;;    (@file :file-name "icicles-doc2.el" :to "User Options for Icicles Searching")
;;    (@file :file-name "icicles-doc2.el" :to "Using Regexps with Icicles Search")
;;
;;  (@file :file-name "icicles-doc2.el" :to "Search and Replace")
;;  (@file :file-name "icicles-doc2.el" :to "Other Icicles Search Commands")
;;    (@file :file-name "icicles-doc2.el" :to "Icicles Imenu")
;;      (@file :file-name "icicles-doc2.el" :to "Type-Specific Imenu Commands")
;;      (@file :file-name "icicles-doc2.el" :to "Imenu Commands that Search Full Definitions")
;;      (@file :file-name "icicles-doc2.el" :to "Icicles Imenu Combines Benefits of Imenu and Emacs Tags")
;;
;;    (@file :file-name "icicles-doc2.el" :to "Compile/Grep Search")
;;    (@file :file-name "icicles-doc2.el" :to "Input Reuse in Interactive Interpreter Modes")
;;    (@file :file-name "icicles-doc2.el" :to "Define Your Own Icicles Search Commands")
;;
;;  (@file :file-name "icicles-doc2.el" :to "Icicles Bookmark Enhancements")
;;    (@file :file-name "icicles-doc2.el" :to "Using Tagged Files")
;;      (@file :file-name "icicles-doc2.el" :to "`icicle-find-file-tagged'")
;;      (@file :file-name "icicles-doc2.el" :to "Jumping to Tagged Files (Other)")
;;    (@file :file-name "icicles-doc2.el" :to "Saving Regions and Selecting Them")
;;    (@file :file-name "icicles-doc2.el" :to "Setting a Bookmark and Jumping to a Bookmark")
;;    (@file :file-name "icicles-doc2.el" :to "Jumping to a Bookmark")
;;    (@file :file-name "icicles-doc2.el" :to "Searching Bookmarked Objects")
;;    (@file :file-name "icicles-doc2.el" :to "Bookmarking Icicles Search Hits")
;;    (@file :file-name "icicles-doc2.el" :to "Acting on Bookmark Properties")
;;  (@file :file-name "icicles-doc2.el" :to "Icicles Enhancements for Emacs Tags")
;;    (@file :file-name "icicles-doc2.el" :to "`icicle-find-tag': Find Tags in All Tags Tables")
;;    (@file :file-name "icicles-doc2.el" :to "`icicle-find-first-tag': Find First Tag in Current Table")
;;    (@file :file-name "icicles-doc2.el" :to "`icicle-tags-search': Search and Replace Using Tags")
;;
;;  (@file :file-name "icicles-doc2.el" :to "Icicles Shell-Command Enhancements")
;;    (@file :file-name "icicles-doc2.el" :to "Shell Command Completion as File-Name Completion")
;;    (@file :file-name "icicles-doc2.el" :to "Gotcha: `$' in Shell Commands")
;;    (@file :file-name "icicles-doc2.el" :to "Known Shell Commands as Proxy Candidates")
;;  (@file :file-name "icicles-doc2.el" :to "Icicles Dired Enhancements")
;;    (@file :file-name "icicles-doc2.el" :to "Search-and-Replace Marked Files")
;;    (@file :file-name "icicles-doc2.el" :to "Save Marked Names as Completion Candidates")
;;      (@file :file-name "icicles-doc2.el" :to "Save Marked Names Here and Below")
;;    (@file :file-name "icicles-doc2.el" :to "Open Dired for a Set of File and Dir Names")
;;    (@file :file-name "icicles-doc2.el" :to "Marked Files and Dirs as a Project")
;;    (@file :file-name "icicles-doc2.el" :to "Shell Commands on Marked Files")
;;
;;  (@file :file-name "icicles-doc2.el" :to "Icicles Info Enhancements")
;;    (@file :file-name "icicles-doc2.el" :to "Icicles Completion for Info")
;;    (@file :file-name "icicles-doc2.el" :to "Highlighting Index Topics for Visited Info Nodes")
;;    (@file :file-name "icicles-doc2.el" :to "Virtual Info Books")
;;    (@file :file-name "icicles-doc2.el" :to "Finding Nodes Containing Some Text")
;;    (@file :file-name "icicles-doc2.el" :to "Using Icicle-Search With Info")
;;
;;  (@file :file-name "icicles-doc2.el" :to "Support for Projects")
;;    (@file :file-name "icicles-doc2.el" :to "Bookmarks for Project Access and Organization")
;;    (@file :file-name "icicles-doc2.el" :to "A Tags File Can Define a Project")
;;    (@file :file-name "icicles-doc2.el" :to "Navigating Among Code Definitions")
;;    (@file :file-name "icicles-doc2.el" :to "Searching Project Files")
;;    (@file :file-name "icicles-doc2.el" :to "Defining and Saving Sets of Files or Buffers")
;;    (@file :file-name "icicles-doc2.el" :to "Retrieving and Reusing a Saved Project")
;;    (@file :file-name "icicles-doc2.el" :to "Semantics? Roll Your Own?")
;;
;;  (@file :file-name "icicles-doc2.el" :to "Using Complex Completion Candidates")
;;  (@file :file-name "icicles-doc2.el" :to "Icicles OO: Object-Action Interaction")
;;    (@file :file-name "icicles-doc2.el" :to "Apropos Completion as OO")
;;    (@file :file-name "icicles-doc2.el" :to "M-RET")
;;    (@file :file-name "icicles-doc2.el" :to "`icicle-object-action' and `icicle-anything'")
;;    (@file :file-name "icicles-doc2.el" :to "Icicles with Anything")
;;
;;  (@file :file-name "icicles-doc2.el" :to "Completion Methods and Styles")
;;    (@file :file-name "icicles-doc2.el" :to "Vanilla Emacs Styles and Option `completing-styles'")
;;    (@file :file-name "icicles-doc2.el" :to "Prefix Completion Method `vanilla'")
;;    (@file :file-name "icicles-doc2.el" :to "Icicles Completion Methods")
;;    (@file :file-name "icicles-doc2.el" :to "Changing Completion Method")
;;    (@file :file-name "icicles-doc2.el" :to "Command-Specific Completion Methods")
;;    (@file :file-name "icicles-doc2.el" :to "Fuzzy Completion")
;;      (@file :file-name "icicles-doc2.el" :to "Scatter-Match (Flex) Completion")
;;      (@file :file-name "icicles-doc2.el" :to "Swank (Fuzzy Symbol) Completion")
;;      (@file :file-name "icicles-doc2.el" :to "Fuzzy-Match Completion")
;;      (@file :file-name "icicles-doc2.el" :to "Levenshtein Completion")
;;      (@file :file-name "icicles-doc2.el" :to "Jaro-Winkler Completion")
;;
;;  (@file :file-name "icicles-doc2.el" :to "Completion in Other Buffers")
;;    (@file :file-name "icicles-doc2.el" :to "Dynamic Completion Using `dabbrev.el'")
;;    (@file :file-name "icicles-doc2.el" :to "Dynamic Completion Using `completion.el'")
;;    (@file :file-name "icicles-doc2.el" :to "Thesaurus Lookup and Completion")
;;    (@file :file-name "icicles-doc2.el" :to "BBDB Completion")
;;    (@file :file-name "icicles-doc2.el" :to "Completion in Comint Modes")
;;
;;  (@file :file-name "icicles-doc2.el" :to "Customization and General Tips")
;;    (@file :file-name "icicles-doc2.el" :to "Using Icicles with Delete Selection Mode")
;;    (@file :file-name "icicles-doc2.el" :to "Icicles User Options and Faces")
;;
;;  (@file :file-name "icicles-doc2.el" :to "File-Name and Directory-Name Completion Tips")
;;  (@file :file-name "icicles-doc2.el" :to "Key Bindings")
;;    (@file :file-name "icicles-doc2.el" :to "Global Bindings")
;;    (@file :file-name "icicles-doc2.el" :to "Icicles-Mode Bindings")
;;    (@file :file-name "icicles-doc2.el" :to "Minibuffer Bindings")
;;
;;  (@file :file-name "icicles-doc2.el" :to "Customizing Key Bindings")
;;    (@file :file-name "icicles-doc2.el" :to "Customizing Global Bindings")
;;    (@file :file-name "icicles-doc2.el" :to "Customizing Icicle Mode Bindings")
;;    (@file :file-name "icicles-doc2.el" :to "Customizing Minibuffer Bindings")
;;
;;  (@file :file-name "icicles-doc2.el" :to "Icicles Redefines Some Standard Functions")
;;  (@file :file-name "icicles-doc2.el" :to "Debugging and Reporting Icicles Bugs")
;;    (@file :file-name "icicles-doc2.el" :to "Debugging Tips")
;;  (@file :file-name "icicles-doc2.el" :to "Programming with Fancy Candidates")
;;  (@file :file-name "icicles-doc2.el" :to "Programming Multi-Completions")
;;    (@file :file-name "icicles-doc2.el" :to "Variable icicle-list-use-nth-parts")
;;    (@file :file-name "icicles-doc2.el" :to "Variable icicle-candidate-properties-alist")
;;    (@file :file-name "icicles-doc2.el" :to "What You See Is Not What You Get")
;;
;;  (@file :file-name "icicles-doc2.el" :to "Candidates with Text Properties")
;;    (@file :file-name "icicles-doc2.el" :to "Using Property icicle-special-candidate")
;;    (@file :file-name "icicles-doc2.el" :to "Applying Text Properties to a Candidate String")
;;
;;  (@file :file-name "icicles-doc2.el" :to "Defining Icicles Commands (Including Multi-Commands)")
;;    (@file :file-name "icicles-doc2.el" :to "Nothing To It!")
;;    (@file :file-name "icicles-doc2.el" :to "Multi-Commands Are Easy To Define Too")
;;    (@file :file-name "icicles-doc2.el" :to "Are Users Dependent on Icicles To Use Multi-Commands?")
;;
;;  (@file :file-name "icicles-doc2.el" :to "Defining Icicles Tripping Commands")
;;  (@file :file-name "icicles-doc2.el" :to "Defining Multiple-Choice Menus")
;;  (@file :file-name "icicles-doc2.el" :to "Defining Icicles Multi `M-x'")
;;    (@file :file-name "icicles-doc2.el" :to "How Multi `M-x' is Defined")
;;
;;  (@file :file-name "icicles-doc2.el" :to "Defining Multi-Commands the Hard Way")
;;  (@file :file-name "icicles-doc2.el" :to "Global Filters")
;;  (@file :file-name "icicles-doc2.el" :to "Specifying Match Functions for Commands")
;;  (@file :file-name "icicles-doc2.el" :to "Defining Buffer-Text Completion for Comint Modes")
;;  (@file :file-name "icicles-doc2.el" :to "Note to Programmers")
;;  (@file :file-name "icicles-doc2.el" :to "La Petite Histoire")
;;  (@file :file-name "icicles-doc2.el" :to "Note on Non-`nil' `pop-up-frames' on MS Windows")
 
;;(@* "Nutshell View of Icicles")
;;
;;  Nutshell View of Icicles
;;  ------------------------
;;
;;(@* "README for NON-Readers")
;;  ** README for NON-Readers **
;;
;;  Load library `icicles.el', turn on Icicle mode, and you're good to
;;  go.  You can turn Icicle mode off or on at any time with command
;;  `icy-mode'.  When you turn it off, you're back in vanilla Emacs.
;;
;;  Beyond that, the most important thing to know about Icicles is
;;  that you can get help on Icicle mode during minibuffer input.  You
;;  can do that by using item Help of the menu-bar Icicles menu or
;;  Minibuf menu, or by hitting `M-?' (`icicle-minibuffer-help').
;;
;;  In addition to `M-?', you can get help on all of the keys that
;;  Icicles binds when you are in the minibuffer.  This is important,
;;  particularly during completion, because keys are available that
;;  change the behavior of completion or that provide additional help
;;  on completion candidates.  To see the keys bound in the
;;  minibuffer, use `M-S-TAB'.  This actually *completes* keys in the
;;  minibuffer, so you can use it also as an entry point to executing
;;  commands whose keys or names you don't recall.  (If your window
;;  manager steals `M-S-TAB' then try `ESC S-TAB' or customize option
;;  `icicle-key-complete-keys-for-minibuffer'.)
;;
;;  So just remember those two keys, `M-?' and `M-S-TAB', to be able
;;  to remind yourself about all of the crazy stuff that Icicles lets
;;  you do in the minibuffer.
;;
;;  You now know enough to use Icicles.  If you have doc-phobia or are
;;  easily overwhelmed by explanations, then *read no more* - just try
;;  it!
;;
;;  If you want a little more explanation than the help page (`M-?'),
;;  then read the rest of section (@> "Nutshell View of Icicles"), but
;;  no more.  It shows a sample of what you can do in Icicle mode.
;;
;;  If you want to know more about Icicles by reading instead of just
;;  trying, then read beyond section (@> "Nutshell View of Icicles").
;;  There is a lot you can learn, but there is not much that you need
;;  to learn, to use Icicles usefully.  Do not be afraid to get in and
;;  get wet.  Above all, do not be overwhelmed by the doc - if it
;;  helps, fine.
;;
;;(@* "Menus")
;;  ** Menus **
;;
;;  One good way to start with Icicles is to explore menu-bar menus
;;  `Icicles' and `Minibuf'.  You can access most Icicles features
;;  using these menus, without at the same time struggling to learn
;;  new key bindings.
;;
;;  Menu `Icicles' is available all of the time (that is, whenever you
;;  are in Icicle mode).  Menu `Minibuf' is available whenever the
;;  minibuffer is active.  It is enhanced for Icicle mode.
;;
;;  Menu `Icicles' offers top-level commands of all sorts that are
;;  available in Icicle mode.  Instead of grouping these all in the
;;  same menu, `Icicles', by default they are spread among appropriate
;;  existing menus such as `File' and `Edit', on `Icicles' submenus.
;;  Whether this is done is controlled by user option
;;  `icicle-touche-pas-aux-menus-flag'.  When it is done, menu-bar
;;  menu `Icicles' contains only menu items that do not fit elsewhere.
;;
;;  During minibuffer input completion, you can also press Control and
;;  right-click (`C-mouse-3') on a completion candidate in buffer
;;  `*Completions*', and choose from a contextual popup menu,
;;  `Completion'.
;;
;;  You can customize this menu.  By default, its submenu `This
;;  Candidate' has menu items that apply to the candidate you clicked
;;  to pop up the menu.  The other submenus have items that apply to
;;  all candidates, candidates you have selected (the region in
;;  `*Completions*'), or candidates you have saved (marked).
;;
;;  Altogether, there are many menu items in the popup menu.  Think of
;;  the menu as a learning device and a way to remind you of possible
;;  operations on completion candidates and the keys they are bound
;;  to.  In this it is similar to the help you get when you use `M-?'
;;  in the minibuffer, but with the menu you can also act, not just be
;;  reminded.
;;
;;  See Also: (@> "*Completions* Display").
;;
;;(@* "README")
;;  ** README **
;;
;;  Icicles enhances minibuffer completion.  It does so generally,
;;  throughout Emacs.  It does this by improving the basic Emacs
;;  functions that read minibuffer input with completion:
;;  `completing-read', `read-file-name', and `read-from-minibuffer'.
;;
;;  This means that Icicles affects every kind of minibuffer
;;  completion, not just buffers or files or commands or variables or
;;  faces or...  Every kind.  (You can even complete key sequences.)
;;  Lisp code need not call a special completion function to get this
;;  advantage - each call to `completing-read' etc. benefits
;;  automatically.
;;
;;  Icicles has this effect only when you are in Icicle minor mode.
;;  Turn off Icicle mode and Emacs behaves as usual (vanilla).
;;
;;  To best understand what Icicles offers you, you need to think a
;;  bit differently about minibuffer completion.
;;
;;  During (vanilla) Emacs completion:
;;
;;  v1. You type some input.  Then you ask Emacs (`TAB') to look for a
;;      match of your input against the domain of input possibilities:
;;      the completion candidates.  If a match is found, your input is
;;      completed and you hit `RET' to accept it (commit it as your
;;      final input).  If there are several matches you type some more
;;      text and recomplete ... until there is only one match.  Then
;;      you hit `RET'.
;;
;;  v2. The goal here is to complete and then enter your input, as
;;      easily and rapidly as possible.
;;
;;  v3. The process starts with a domain of possible inputs: the
;;      initial set of candidates.  This domain can be realized
;;      intensionally, via a function, or more commonly extensionally,
;;      via an explicit set.
;;
;;  v4. Sometimes completion is "lax" instead of "strict", meaning
;;      that you can input any text you like in the end (`RET'), but a
;;      set of candidates is still provided for the convenience of
;;      completion (`TAB').
;;
;;  v5. When you hit `TAB' to complete, your current input in the
;;      minibuffer (the contents) filters the domain, narrowing it
;;      down to the set of matching candidates, which is shown in
;;      buffer `*Completions*'.
;;
;;  See Also: (@> "Background on Vanilla Emacs Input Completion").
;;
;;  For Icicles it is the completion *process* that is important, and
;;  this process can serve several goals.  So instead of focusing on
;;  the goal of entering a final input (v2), let's consider the
;;  overall process: There is a set (domain) of possible candidates
;;  (v3) that you filter with your input (v1).
;;
;;  If you edit your input and recomplete using `TAB', then the domain
;;  is refiltered to a different set of matching completion
;;  candidates.  Each time you do this your input corresponds to a
;;  different set.  The process of completion thus provides a great
;;  way to filter a set interactively.
;;
;;  Now if all you do in the minibuffer at the end of the day is enter
;;  your final input (`RET'), then changing your input and
;;  recompleting (`TAB') is pretty much a waste of time, a detour.
;;  But if you can do more along the way, if you can make use of the
;;  current set of matches in some way, then the story is different.
;;
;;  This is the key to understanding what Icicles offers.  Think of
;;  minibuffer completion as a pattern-matching user interface, a
;;  general way to interact with sets of named objects.
;;
;;  No doubt you have already used completion sometimes as more than
;;  just a means toward the end of inputting a value.  Perhaps you
;;  have used `TAB' during file-name completion to take a look at the
;;  nearby directory structure, possibly even finishing with `C-g'
;;  instead of entering any file name definitively (`RET').  In that
;;  scenario, you are exploring the set of file-name candidates, using
;;  minibuffer completion as a help feature.
;;
;;  Icicles binds different keys in the minibuffer keymaps to provide
;;  different ways to interact with the current set of matches
;;  (completion candidates).  To exploit the possibilities of
;;  filtering a set dynamically by pattern matching, Icicles provides
;;  different features that work together.
;;
;;  These are the most important Icicles features:
;;
;;  i1. Incremental completion.  By default, each change you make to
;;      your minibuffer input automatically rematches and recompletes
;;      it (v5).  IOW, the set of current candidates is updated
;;      dynamically, incrementally.  You can always see (in the
;;      `*Completions*' window) what your current input (the
;;      minibuffer content) matches.  This is a help and exploration
;;      feature.
;;
;;  i2. Cycling and the current candidate.  You can cycle among the
;;      current set of candidates (minibuffer matches).  The current
;;      candidate is placed in the minibuffer as you do this, for
;;      possible editing.  You can of course hit `RET' to commit the
;;      current candidate, edited or not, as your final input.
;;
;;  i3. Help, multiple actions, alternative actions.  But you can also
;;      instead hit a key to provide information (help) about the
;;      current candidate; hit a key to act on it (accept it as input
;;      but without ending the minibuffer); hit a key to perform some
;;      alternative action on it (without ending the minibuffer); and
;;      so on.  Candidate help is perhaps the Icicles feature used
;;      most often.
;;
;;  i4. Multi-commands and multi-inputs.  A "multi-command" is one
;;      that lets you act on a completion candidate without exiting
;;      the minibuffer, so that you can thus act on several
;;      candidates.  Not every command is a multi-command.  In
;;      addition to acting on multiple candidates that you choose
;;      (perhaps all of them), you can act on multiple inputs in the
;;      same minibuffer (called "multi-inputs"), all at once.
;;
;;  i5. Multi-completions.  Use completion to match two different
;;      things at once. The completion candidates are multi-part, so
;;      your input can also be multi-part or can match only particular
;;      parts. An example of using multi-completions is matching both
;;      file name and file content, in parallel.
;;
;;  i6. Set operations.  You can hit a key to act not on any
;;      particular matching candidate but on each of them individually
;;      or on all of them collectively.  Among the collective set
;;      operations are union, intersection, difference/complementing,
;;      and saving for later reuse.
;;
;;  i7. Progressive completion.  Set intersection can take a couple of
;;      forms.  The most useful is "progressive completion": use the
;;      current set of matching candidates as the domain for a
;;      recursive minibuffer.  That is, start over and match different
;;      inputs against a subset of the original domain that was
;;      defined by the previous matching operation.  This is analogous
;;      to piping `grep' outputs through additional `grep' filters.
;;
;;  i8. More powerful matching.  Using your input (minibuffer content)
;;      as a dynamic filter is very important in Icicles.  In line
;;      with this, you can employ different matching methods.  The
;;      most powerful is regexp matching (which includes substring
;;      matching).
;;
;;  i9. Candidate sorting.  You can sort the current candidates on the
;;      fly in different ways, so that you can cycle them in different
;;      orders.  The possible sort orders at any time depend on the
;;      context and type of candidates.  You can define your own sort
;;      orders.
;;
;;  Most of these features are unique to Icicles.  Others were
;;  original with Icicles but have since been copied by vanilla Emacs
;;  or other libraries.  But only Icicles offers these features (and
;;  more) together, combining them cooperatively.
;;
;;  Icicles is very general.  It affects many aspects of minibuffer
;;  completion.  As you learn to take advantage of its features it can
;;  change how you use Emacs.  But Icicles also stays out of the way
;;  if you do not ask for its help.  If you just use `TAB' completion
;;  and you use none of the keys bound specially during completion
;;  (for cycling, progressive completion, etc.), then you will hardly
;;  notice Icicles.
;;
;;  All Lisp code that uses minibuffer completion automatically takes
;;  advantage of Icicles.  But some other libraries go beyond this by
;;  defining Icicles multi-commands or otherwise making some special
;;  use of Icicles features.  None of these libraries are required in
;;  order to use Icicles, but they are recommended because of the
;;  synergy they provide.  See (@> "Installing Icicles").
;;
;;(@* "Flashy Demo to Pique Your Curiosity")
;;  ** Flashy Demo to Pique Your Curiosity **
;;
;;  This section shows a few examples to get you started and hopefully
;;  interested.  Subsequent sections in this nutshell view show more
;;  Icicles features in more detail, but still giving an overview.
;;
;;(@* "First Example: Multi-Inputs")
;;  *** First Example: Multi-Inputs ***
;;
;;  In vanilla Emacs you can do this to visit all files whose names
;;  match the glob pattern `ici*.el':
;;
;;    C-x 4 f  i c i * . e l  RET
;;
;;  In Icicle mode you can also do the following, to visit all
;;  `ici*.el' files, file `my file.txt', and file `bookmark+.el', just
;;  as if you had used `C-x 4 f' three separate times [*]:
;;
;;    C-x 4 f  ici*.el  "my file.txt"  bookmark+.el  M-R  C-g
;;
;;  Your multi-input here is split into separate file-name patterns,
;;  which are then acted on individually.  You wrap the second pattern
;;  with "..." because the file name contains a space character, which
;;  is otherwise used to separate patterns.
;;
;;  The `C-g' at the end just ends the command.  Without it you can
;;  continue to enter the names of more files to visit.  This is
;;  because `C-x 4 f' is bound to a multi-command
;;  (`icicle-file-other-window').
;;
;;  [*] (For brevity, spaces were not used here to separate each
;;  printable character typed: `ici*.el' instead of `i c i * . e l'.)
;;
;;(@* "Second Example: Multi-Completions")
;;  *** Second Example: Multi-Completions ***
;;
;;  Suppose that you do this:
;;
;;    C-x 4 f   i c i   C-M-j   c o l o r   S-SPC
;;
;;  The completion candidates, shown in `*Completions*', are the files
;;  (a) whose name contains `icicles' (completed from `ici') and (b)
;;  whose content includes `color' - that is, the files themselves
;;  contain the text `color'.
;;
;;  The file content is in fact part of the two-part completion
;;  candidate, but of course it is not shown.  It is used only for
;;  filtering.  Only the file names are shown in `*Completions*'.
;;
;;  Then you do this, with the previous candidates still current:
;;
;;    c m   C-M-j   i n e - f
;;
;;  As you type, the candidates shown in `*Completions*' are updated.
;;  When you finish typing, the set of candidates is reduced to just
;;  `icicles-cmd1.el' and `icicles-cmd2.el', the files whose name
;;  contains also `cm' and whose content contains also `ine-f' (as in
;;  `icicle-define-file-command').
;;
;;  Then you hit `C-!' to visit each of those files.
;;
;;  (Yes, it's a toy example to show you some features.  Clearly you
;;  could just as well have used only `icicles-cm' as your input, or
;;  even `ici.*cm', or `s-cm', or perhaps only `cm', depending on the
;;  files in your directory.)
;;
;;  You have just used the following Icicles features:
;;
;;  * incremental completion - candidate set updated as you type
;;
;;  * apropos completion - matching not just a completion-candidate
;;    prefix but a substring (or even a regexp)
;;
;;  * multi-completion matching - matching in parallel two different
;;    things at once (in this case, file name and file content),
;;    separating the patterns with `C-M-j'
;;
;;  * progressive completion - matching sequentially two different
;;    inputs (each a multi-completion in this case), separated by
;;    `S-SPC'
;;
;;  * multi-command action - acting on more than one matching
;;    candidate, in this case all (both) of them: `C-!'
;;
;;  Using `C-M-j' followed by a content-matching pattern in your input
;;  is optional.  If you do not provide it then no content-searching
;;  is done.
;;
;;  The file-name pattern is also optional: you can search content
;;  without having any idea about the file names - just start with
;;  `C-M-j'.  Obviously, content-searching is much slower than
;;  file-name matching.  If you can match names, that reduces the
;;  number of files whose content needs to be searched.
;;
;;  Most of the behavior you see in this example is available for most
;;  Icicles commands.  The name+content behavior is available for only
;;  some Icicles commands, including those that choose files, buffers,
;;  or some other type of object that has contents (a "container").
;;
;;  See Also:
;;
;;  * (@> "Icompletion")
;;  * (@> "Prefix Completion and Apropos Completion")
;;  * (@> "Multi-Completions")
;;  * (@> "Progressive Completion")
;;  * (@> "Multi-Commands")
;;
;;(@* "Third Example: Narrowing a Manual")
;;  *** Third Example: Narrowing a Manual ***
;;
;;  The second example showed how you can match either the name or the
;;  contents of a file, or both.  The same behavior of matching both
;;  the name and the contents of a container is available for visiting
;;  an Info node (chapter of a manual) or a buffer.  In Icicle mode,
;;  by default, `g' in Info mode is bound to `icicle-Info-goto-node',
;;  and `C-x b' is bound to `icicle-buffer'.
;;
;;  These multi-commands act just as described in the second example,
;;  but using buffer-name/contents and node-name/contents candidates
;;  instead of file name/contents.
;;
;;  In this example you look up information about indenting text to a
;;  column in the Emacs manual.
;;
;;  Using the indexes of the manual is always a good way to start.
;;  And Icicles helps with that too, by letting you match index
;;  entries using patterns (e.g., `indent') that are substrings and
;;  regexps.
;;
;;  You could use incremental search to search the whole manual for
;;  terms like `indent', but that would be quite laborious.  (You can
;;  also use Icicles search to search a manual - see
;;  (@file :file-name "icicles-doc2.el" :to "Using Icicle-Search With Info").)
;;
;;  But this example uses the same approach as in the second one:
;;  match node names and node content.
;;
;;  1. Start by searching all nodes of the Emacs manual for `indent':
;;
;;       C-h r
;;
;;       g C-M-j indent S-TAB
;;
;;     That takes a moment, and shows lots of matching nodes.
;;
;;  2. Search also for `column':
;;
;;       S-SPC C-M-j column
;;
;;     There are a lot fewer nodes in `*Completions*' now.  But still
;;     too many to browse.
;;
;;  3. There are a bunch of nodes whose names all start with
;;     `Glossary', and they just get in the way, so get rid of them.
;;     First match only those nodes, by name:
;;
;;       S-SPC loss S-TAB
;;
;;     Then toss those nodes out and ask to match something else:
;;
;;       C-~
;;
;;   4. You're down to just these nodes now, in `*Completions*':
;;
;;      Acknowledgments      Basic Indent        Command Index
;;      Comment Commands     Concept Index       Directory Variables
;;      Directory Variables-Footnote-1           Enriched Indentation
;;      Fill Prefix          ForIndent Cont      ForIndent Num
;;      Fortran              Fortran Autofill    Fortran Columns 
;;      Fortran Comments     Fortran Indent      Indentation Commands
;;      Just Spaces          Key Index           Left Margin Paren
;;      Lisp Indent          Multi-Line Comments Multi-line Indent
;;      Options for Comments Picture Mode        Program Modes
;;      Selective Display    Tab Stops           Table Conversion 
;;      Text                 Top                 Variable Index 
;;
;;   5. There's a lot of Fortran stuff there, which you can remove:
;;
;;        S-SPC fortr S-TAB C-~
;;
;;   6. Now selectively remove some of the other ones individually.
;;      You can do that by clicking them with `S-mouse-2' or by
;;      cycling to them and hitting the `delete' key.  Click
;;      `Acknowledgments' and `Top' to begin with.
;;
;;   7. Not being too sure about some of the others, you can take a
;;      quick look at them and hit `delete' if they don't look
;;      interesting.  Use `down' or `up' to cycle among them.  Use
;;      `C-RET' to take a look at the node that is the current
;;      candidate (highlighted in `*Completions*').  Use `C-M-s' in
;;      the Info window to check occurrences of the content pattern,
;;      if you want.  Use `delete' (in the minibuffer) to remove the
;;      current candidate node if not pertinent.
;;
;;   8. Now you're down to just these node names:
;;
;;      Basic Indent        Command Index         Comment Commands
;;      Concept Index       Enriched Indentation  Indentation Commands 
;;      Just Spaces         Key Index             Multi-line Indent
;;      Program Modes       Tab Stops             Text
;;      Variable Index 
;;
;;      That might seem like a lot of trouble, but each of the
;;      operations is simple and you will soon make use of them
;;      everywhere in Icicles.
;;
;;      What you have now is essentially a customized, mini-manual
;;      about indenting text.  You can visit any of its nodes by
;;      cycling to them and hitting `C-RET', or by just clicking
;;      `C-mouse-2' on the node name in `*Completions*'.
;;
;;   9. You can "save", or "mark", this set of nodes, so you can
;;      easily come back and reuse it later.
;;
;;        C-M->
;;
;;      The nodes are now shaded/highlighted, showing that they are
;;      "saved" for the current Emacs session.  Hit `RET' to end at
;;      one of the nodes or repeat `C-g' or `C-]' to cancel the
;;      command and return to `Top'.
;;
;;   10. To restore the mini-manual:
;;
;;        g C-M-<
;;
;;   11. You can save the mini-manual persistently, so you can come
;;      back to it in a later Emacs session.  Retrieve the node-name
;;      candidates, then use `C-}' to save them.  You are prompted for
;;      a saved-candidates-set name and the cache file location, which
;;      can be anything you like.
;;
;;        g C-M-< C-} Indentation Topics RET
;;
;;        /here/IndentationTopics.icy RET
;;
;;   12. To restore the mini-manual in any Emacs session:
;;
;;        g C-{
;;
;;      Alternatively, you could have just saved the candidates
;;      persistently to begin with, using `C-}' instead of `C-M->'.
;;
;;  These are the features used in this example, which you did not use
;;  in the second example:
;;
;;  * removing selected candidates from the current matching set
;;    (`delete' key, `S-mouse-2', and `C-~')
;;
;;  * saving and retrieving a set of candidates (`C-M->', `C-}',
;;    `C-M-<', `C-{')
;;
;;  See Also:
;;
;;  * (@> "Chip Away the Non-Elephant")
;;  * (@> "Persistent Sets of Completion Candidates")
;;  * (@file :file-name "icicles-doc2.el" :to "Icicles Info Enhancements")
;;  * (@file :file-name "icicles-doc2.el" :to "Virtual Info Books")
;;  * (@file :file-name "icicles-doc2.el" :to "Using Icicle-Search With Info")
;;  * (@> "Buffer-Name Input")
;;
;;(@* "Fourth Example: Tagged Files")
;;  *** Fourth Example: Tagged Files ***
;;
;;  This feature works only if you also use library Bookmark+, which
;;  lets you tag files with arbitrary labels (delicious-style tags)
;;  that you come up with.
;;
;;  Suppose you have previously tagged some files, and now you want to
;;  visit one or more of the files that have both of the tags `2013'
;;  and `mountains'.  If you also have some idea of the file names or
;;  file contents, then you can match those too, as explained in the
;;  second example - see (@> "Second Example: Multi-Completions").
;;  But let's suppose you do not and you just want to match tags.
;;
;;    C-x 4 f TAB C-x C-t *  2013  RET  mountains  RET RET
;;
;;  During file-name completion, `C-x C-t *' narrows the current
;;  candidates to those naming files that have been tagged with each
;;  of the tags you enter.  You type each tag to be matched, followed
;;  by `RET', then you add a second `RET' after the last tag.
;;
;;  In this case, the current candidates before using `C-x C-t *'
;;  included all files in the current directory (you hit `TAB'
;;  immediately, without typing anything to match file names or file
;;  contents).
;;
;;  Besides narrowing candidates during ordinary file-visiting
;;  commands (e.g., `C-x C-f'), you can use multi-command
;;  `icicle-find-file-tagged' (`C-x j t C-f C-f') to directly visit a
;;  file that has tags matching the second part of your
;;  multi-completion input, the part after `C-M-j'.  For example:
;;
;;    C-x j t C-f C-f C-M-j  2013  S-SPC  mountains  C-! C-g C-g C-g
;;
;;  (The `C-M-j' is not needed if neither tag matches a file name.)
;;
;;  Tagging the files in the first place is also easy.  Here is how to
;;  tag the files whose names match both `ici' and `doc' with the tags
;;  `Icicles' and `help' (this should really be shown on a single
;;  line):
;;
;;    C-x p t + a  Icicles  RET  help  RET RET
;;                 ici  S-SPC  doc  C-! C-g C-g
;;
;;  `C-x p t + a' runs multi-command `icicle-tag-a-file', which adds
;;  the tags you type (ending the last with `RET RET' instead of
;;  `RET') to the files whose candidate names you act on.  In this
;;  case, you hit `C-!', which acts on all candidates, which in this
;;  case are the file names matching both `ici' and `doc'.
;;
;;  You can also tag files on the fly during file-name completion.  To
;;  tag the current candidate, hit `C-x a +', then enter the tags to
;;  add.
;;
;;  See (@file :file-name "icicles-doc2.el" :to "Using Tagged Files").
;;
;;(@* "Top Level to Minibuffer ... and Back Again")
;;  ** Top Level to Minibuffer ... and Back Again **
;;
;;  While the minibuffer is active, waiting for you to finish your
;;  input, you can optionally perform various side actions,
;;  interacting with Emacs in pretty much the usual ways.  For
;;  example, you can move to another buffer, type some text there,
;;  check the definition of a key using `C-h k', and then return to
;;  typing your input in the minibuffer.  This is true of vanilla
;;  Emacs, not just Icicles.
;;
;;  In other words, the Emacs minibuffer is not "modal", in the sense
;;  that while it is interacting with you it does not force you to
;;  stop interacting with Emacs in other ways.  You can select some
;;  other window, edit some text there, hit keys that perform various
;;  actions, and so on.
;;
;;  You can tell by now that Icicles uses the minibuffer heavily.  It
;;  takes advantage of and enhances such side-trip interaction.  It
;;  does this in part by binding lots of keys in the minibuffer
;;  keymaps.  The examples seen so far exhibit some of those keys:
;;
;;    M-?          Show Icicles minibuffer help
;;    M-R          Act on multiple minibuffer inputs
;;    C-M-j        Separate multi-completion component patterns
;;    S-SPC        Initiate progressive completion
;;    C-!          Act on each matching candidate, in turn
;;    S-TAB        Apropos-complete (regexp or substring matching)
;;    down, up     Cycle to the next or previous candidate
;;    C-RET        Act on the current candidate
;;    delete       Remove the current candidate from the matches
;;    C-~          Remove all of the current candidates (complement)
;;    C-M->        Save (mark) the current candidates, for later reuse
;;    C-M-<        Retrieve (reuse) the saved (marked) candidates
;;    C-}          Save the current candidates persistently
;;    C-{          Retrieve a persistently saved set of candidates
;;    C-x C-t *    Narrow to files having each of the tags you specify
;;    C-x a +      Add tags to the current file-name candidate
;;
;;  (How to remember all of this?  You don't need to.  You will learn
;;  some of the keys little by little.  And `M-?' in the minibuffer
;;  lists many of them.  And the keys you don't know won't hurt you.
;;  You can also use `C-h M-k' from library `help-fns+.el' to list the
;;  keys in any keymap, including the minibuffer maps.)
;;
;;  Sometimes this temporary, side-track, or parallel interaction can
;;  itself involve inputting something different in a separate
;;  minibuffer.  That is, in some cases it can mean that you are
;;  prompted for minibuffer input at a different level.
;;
;;  A recursive minibuffer is used for this interaction: the original
;;  minibuffer interaction disappears from view temporarily, replaced
;;  by the new, recursive one.  The original minibuffer interaction is
;;  not shown, but it is still there, waiting for the new, recursive
;;  minibuffer interaction to finish before showing itself again.
;;
;;  Node `Recursive Editing Levels' in the Emacs manual covers the
;;  general topic of recursive editing levels, of which recursive
;;  minibuffer levels is a particular case.  You will want to read
;;  this.
;;
;;  Unfortunately however, though it is worth reading it says next to
;;  nothing about the case of recursive minibuffers, because vanilla
;;  Emacs makes little use of this feature.  The Elisp manual, node
;;  `Recursive Mini', says a little about the topic, but still not
;;  much.
;;
;;  Whereas Emacs provides a special indication of recursive editing
;;  in the mode line, using nested `[...]' to show the current
;;  recursive-edit depth, It offers less to show you the depth of
;;  minibuffer recursion.  To get it to show anything at all, you need
;;  to turn on `minibuffer-depth-indicate-mode'.  Then it prepends a
;;  depth indicator to the minibuffer prompt.
;;
;;  Emacs bug #14147 asks Emacs Dev to turn on
;;  `minibuffer-depth-indicate-mode' by default.  In any case, it is
;;  turned on automatically whenever you are in Icicle mode, because a
;;  recursive minibuffer is not uncommon in Icicles.  In addition, if
;;  you use library `mb-depth+.el' then you can customize the
;;  appearance of the depth indicator.
;;
;;(@* "Canceling Minibuffer Interaction")
;;  *** Canceling Minibuffer Interaction ***
;;
;;  Whether or not the minibuffer is active, `C-g' cancels the current
;;  Emacs operation, essentially the current command.  If the current
;;  command is reading your input from the minibuffer, then `C-g'
;;  exits the minibuffer.  If this is a recursive minibuffer then you
;;  pop up a level, either to a shallower minibuffer or, if there is
;;  none, to the top level of Emacs (no active minibuffer).
;;
;;  But sometimes when the minibuffer is active the current command is
;;  something other than the command that initiated reading input from
;;  the minibuffer.  This is common in Icicles, but not so common in
;;  vanilla Emacs.  When you hit a key during minibuffer input, the
;;  command bound to that key becomes the current command until it is
;;  finished.  If it is a self-inserting key like `e' then the command
;;  (`self-insert-command') finishes immediately, of course.
;;
;;  But sometimes the command expects more interaction on your part,
;;  or perhaps it performs an operation that takes a while.  If you
;;  then hit `C-g', it is that command, the now current command, that
;;  is interrupted (canceled), and not the minibuffer.
;;
;;  Repeating `C-g' a few times can in many cases cancel the
;;  minibuffer as well.  But you can always cancel a minibuffer
;;  immediately and pop up to the previous level by hitting `C-]'
;;  (`icicle-abort-recursive-edit').
;;
;;  So `C-g' cancels the current command, which sometimes means
;;  canceling the minibuffer.  And `C-]' cancels the current
;;  minibuffer, popping up to the previous one or to the top level if
;;  there is no other minibuffer level.
;;
;;  But suppose you are in a fairly deep recursive minibuffer (for
;;  example, you have used progressive completion several times, where
;;  each time pushes you down another minibuffer level).  How can you
;;  jump directly all the way up to the top level, instead of hitting
;;  `C-]' once for each minibuffer level?
;;
;;  Answer: use `C-M-T' (that is, `C-M-S-t'), which is bound to
;;  `icicle-top-level'.  That brings you directly to the top level
;;  ("without passing GO"), but it first allows Icicles multi-commands
;;  to perform any necessary cleanup.
;;
;;  So you have three degrees of canceling when you are in the
;;  minibuffer: `C-g' (cancel the current command), `C-]' (cancel the
;;  current minibuffer), and `C-M-T' (cancel all minibuffers).
;;
;;  Which you use depends on what you want.  Sometimes you want to
;;  cancel the current operation and resume editing the same
;;  minibuffer.  Sometimes you want to cancel the current minibuffer
;;  level.  And sometimes you want to get out of the minibuffer
;;  altogether.
;;
;;  For example, suppose you are using `g' in Info to look up some
;;  information and you use progressive completion with a content
;;  search, such as in (@* "Third Example: Narrowing a Manual").  You
;;  might decide that the current search is taking too long, or that
;;  perhaps you change your mind about which pattern to search for.
;;
;;  In this case, you might hit `C-g' to cancel the current matching
;;  operation.  After `C-g', you can type the pattern you really want
;;  and hit `S-TAB' again.  Alternatively, you could hit `C-]' to
;;  cancel the current minibuffer, and then hit `S-SPC' to create
;;  another recursive minibuffer.
;;
;;  The effect is the same: you abandon the current search and start
;;  another, without starting over from scratch.  You keep the
;;  progressive-completion filtering already accomplished at previous
;;  minibuffer levels.  If you instead decide to stop searching
;;  altogether then you hit `C-M-T'.
;;
;;  Finally, there is another wrinkle to using `C-g' (as
;;  `icicle-abort-recursive-edit') if you use Delete Selection mode.
;;  If the region is active in `delete-selection-mode' when you hit a
;;  self-inserting key or a deleting key, then the region is deleted
;;  (killed, actually) so that it can be replaced by your edits.
;;  That's a good thing, in general.
;;
;;  When you cycle among completion candidates, depending on your
;;  value of options `icicle-point-position-in-candidate' and
;;  `icicle-mark-position-in-candidate', the text between point and
;;  the mark is selected: the region is active.  And you might
;;  otherwise activate the region in the minibuffer while editing.
;;
;;  In this situation, `C-g' in the minibuffer just deactivates the
;;  region, so you can edit without losing the region text.  In
;;  particular, this means that when you cycle you can more easily
;;  edit the text of the current candidate (e.g. during lax
;;  completion).  Just repeat `C-g' to get its usual behavior (since
;;  the region has been deactivated).
;;
;;  See Also: (@> "Recompleting Using the Original Domain").
;;
;;(@* "Toggle Options on the Fly")
;;  ** Toggle Options on the Fly **
;;
;;  There are many user options that affect the behavior of Icicles
;;  features.  Some of these are Boolean (on/off) or allow for simple
;;  alternation or cycling of the possible values.  Many of those
;;  options are associated with toggle commands that are bound to keys
;;  whenever the minibuffer is active, or at least active for
;;  completion.
;;
;;  Throughout this doc you will see references to such options and
;;  their toggles.  For example: "You can toggle case-sensitivity at
;;  any time using `C-A' (that is, `C-S-a') in the minibuffer."
;;
;;  The reason for making it so easy to change the values of these
;;  options on the fly is that different kinds of completion, in
;;  different contexts, can take advantage of different option values.
;;  Icicles completion is very general, and a single option value is
;;  not always optimal for all contexts.
;;
;;  You will become acquainted with a few of these toggle keys and
;;  remember them, but you will forget others.  What is important to
;;  point out here is that `M-?' (`icicle-minibuffer-help') is your
;;  friend in the minibuffer.  During completion, the help it displays
;;  includes, near the top, a list of the toggle keys and the
;;  corresponding current values of their options.
;;
;;  Whenever you use an Icicles toggle command, a momentary message
;;  shows you the new option value.  So as an alternative to using
;;  `M-?' to see the current value of an option, you can just toggle
;;  it twice.
;;
;;  In addition, all of the Icicles toggle commands, besides generally
;;  having their own minibuffer key bindings also have bindings on
;;  prefix key `M-i' during completion.  You can use `M-i S-TAB' to
;;  complete them, or you can use `M-i C-h' to see a list of them.
;;  For example, besides using `C-A', you can use `M-i A' to toggle
;;  case-sensitivity.
;;
;;  Finally, `M-i M-i' during completion invokes the general command
;;  `icicle-toggle-option', which prompts you for a user option (any
;;  option, Icicles or not), and toggles it.  With a prefix argument
;;  you can toggle options and other variables whose values are
;;  generalized Booleans: `nil' or non-`nil' (not just `t').
;;
;;(@* "Cycle Completion Candidates")
;;  ** Cycle Completion Candidates **
;;
;;   M-x  t o o l  next
;;
;;  That is, type "tool" and then hit the `next' key, which is often
;;  labeled "Page Down".  Each time you hit `next', another match for
;;  your input (`tool') replaces it in the minibuffer:
;;
;;   M-x ediff-toggle-use-toolbar  next
;;   M-x scroll-bar-toolkit-scroll next
;;   M-x tool-bar-mode             next
;;   M-x tooltip-mode              next
;;   M-x ediff-toggle-use-toolbar ; Back to the beginning
;;
;;  Keys `next' and `prior' ("Page Up") cycle among all of the
;;  commands that contain (match) the minibuffer input - `tool', in
;;  this case.  Just hit `RET' (Return) when you get to the command
;;  you want.
;;
;;  (Note: The particular candidates shown here and in other examples
;;  might be different from what you see, depending on your version of
;;  Emacs and what other libraries you might have loaded.)
;;
;;  You can use a regular expression, to narrow the field of matching
;;  inputs:
;;
;;   M-x  i s e . + c h a r          next
;;   M-x isearch-delete-char         next
;;   M-x isearch-other-control-char  next
;;   ...
;;
;;  Note that when you cycle, a one-line description of the current
;;  candidate is shown in the mode line (of buffer `*Completions*' if
;;  visible; otherwise of the current buffer).  You can get more
;;  detailed info about individual candidates by holding the Control
;;  and Meta keys (e.g. `C-M-next') while you cycle - see
;;  (@> "*Completions* Display") and (@> "Help on Completion Candidates").
;;
;;  Try cycling candidates for `C-h v' for instance, using `next'.
;;  Look for their descriptions in the mode line.  Now try cycling
;;  using `C-M-next' - complete candidate help is shown in buffer
;;  `*Help*'.
;;
;;  See (@> "Cycling Completions") for more about cycling completion
;;  candidates.
;;
;;(@* "Display Completion Candidates")
;;  ** Display Completion Candidates **
;;
;;  You can display all of the matches for the current minibuffer
;;  input, in the `*Completions*' buffer, using `S-TAB' (Shift TAB).
;;  So, for instance, `S-TAB' with `M-x ise.+char' in the minibuffer
;;  displays all commands whose names contain `ise' followed
;;  (somewhere) by `char'.
;;
;;  See (@> "*Completions* Display") for more about using the
;;  `*Completions*' window.
;;
;;(@* "Prefix Completion and Apropos Completion")
;;  ** Prefix Completion and Apropos Completion **
;;
;;  You can get the standard Emacs "prefix" completion, instead of the
;;  "apropos" completion just described, by using `TAB' instead of
;;  `S-TAB'.
;;
;;  Icicles documentation always refers to the key that does this as
;;  `TAB'.  But actually it is only `TAB' by default.  You can
;;  customize it, using option `icicle-prefix-complete-keys'.
;;  Similarly, you can customize which keys perform apropos completion
;;  using option `icicle-apropos-complete-keys' (`S-TAB' by default).
;;
;;  You can also swap the keys used for these two modes of completion
;;  dynamically, using command `icicle-toggle-completion-mode-keys',
;;  which is bound to `C-S-TAB' during completion.  This also toggles
;;  the value of option `icicle-default-cycling-mode' (see next).
;;  When it switches away from whatever persistent values you have for
;;  the completion-mode key options, it asks you whether you want to
;;  save the new values.  (That means that using `C-S-TAB' is an easy
;;  way to customize all five of these options.)
;;
;;  You can cycle prefix-completion candidates by using the
;;  `end' and `home' keys instead of `next' and `prior'.  (All four of
;;  these keys are typically together in a central keypad to the right
;;  of the main keyboard.)
;;
;;  You can also cycle candidates according to the current completion
;;  mode, prefix or apropos, using either the mouse wheel or the arrow
;;  keys `down' and `up'.  These are all called the "modal" cycling
;;  keys because they respect the current completion mode.
;;
;;  The current completion mode is determined by the last completion
;;  key you used, `TAB' or `S-TAB'.  If you have not used either so
;;  far during the current minibuffer invocation, then the modal keys
;;  cycle according to the value of option
;;  `icicle-default-cycling-mode'.  By default the option value is
;;  `prefix', which means that you can use `down', `up', or the mouse
;;  wheel to cycle prefix completions without needing to first hit
;;  `TAB'.
;;
;;  The non-modal cycling keys, `next', `prior', `end', and `home'
;;  automatically set the completion mode and update the candidate
;;  completions.  The modal cycling keys just cycle according to the
;;  current completion mode, whether it is apropos or prefix.
;;
;;  To cycle using the mouse wheel, the mouse must not be over the
;;  `*Completions*' window; if it is, then the wheel scrolls that
;;  window instead of cycling candidates - see (@> "*Completions* Display").
;;
;;  As an alternative to using `end' or `next', you can cycle
;;  candidates downward (forward) by just repeating the same
;;  completion key: `TAB' or `S-TAB'.  For example:
;;
;;   M-x tool           TAB ; Display candidates with prefix `tool'
;;   M-x tool-bar-mode  TAB
;;   M-x tooltip-mode   TAB
;;   M-x tool-bar-mode      ; Back to the beginning
;;
;;  Or:
;;
;;   M-x tool                      S-TAB ; Display matching candidates
;;   M-x ediff-toggle-use-toolbar  S-TAB
;;   M-x scroll-bar-toolkit-scroll S-TAB
;;   M-x tool-bar-mode             S-TAB
;;   M-x tooltip-mode              S-TAB
;;   M-x ediff-toggle-use-toolbar        ; Back to the beginning
;;
;;  Changing to a different completion key (`TAB' to `S-TAB' or vice
;;  versa) always switches completion type and completes, but you can
;;  then repeat that new completion key to cycle among the candidates.
;;
;;  Note: In vanilla Emacs, repeating `TAB' scrolls the
;;  `*Completions*' window.  In Icicles, you can use `C-v' to scroll
;;  `*Completions*' down and `M-v' to scroll up.  You can also use the
;;  mouse wheel to scroll `*Completions*'.
;;
;;  See (@> "Apropos Completions") for more about apropos and prefix
;;  completion.
;;
;;(@* "Chains of Simple Match Patterns - Progressive Completion")
;;  ** Chains of Simple Match Patterns - Progressive Completion **
;;
;;  To see which functions contain `char', `delete', and `back' in
;;  their names, in any order:
;;
;;   C-h f  c h a r  S-TAB - Display all function names that contain
;;   `char'.
;;
;;   M-*  d e l e t e  - Narrow that set of names to those that also
;;   contain `delete'.
;;
;;   M-*  b a c k  - Narrow the set of matching names further, to
;;   those that also contain `back'.
;;
;;  This displays a list of functions like this in buffer
;;  `*Completions*' (your list might be somewhat different):
;;
;;    backward-delete-char        backward-delete-char-untabify
;;    delete-backward-char        icicle-backward-delete-char-untabify
;;    icicle-delete-backward-char
;;    quail-conversion-backward-delete-char
;;
;;  Since you are completing input to `C-h f', you can then cycle to a
;;  name using `next' and hit `RET', or click `mouse-2', to see the
;;  doc for that function.  If, instead, you were completing input to
;;  `M-x', you could choose a command to execute.  And so on.
;;
;;  The thing to notice here is that you can use `M-*' to input chains
;;  of multiple simple regexps, to narrow down the set of completion
;;  candidates progressively.  This is analogous to piping the result
;;  of `grep' to another `grep', and piping that result to another
;;  `grep'...
;;
;;  Here are a couple others to try (I'm always forgetting the order
;;  in these compound names):
;;
;;   C-h f  w i n d o w  S-TAB M-*  f r a m e
;;
;;   C-h f  w i n d o w  S-TAB M-*  b u f f e r
;;
;;  As a shortcut, you can use just `S-SPC' instead of `S-TAB M-*'.
;;  See (@> "Progressive Completion") for more about progressive
;;  completion with `M-*'.
;;
;;(@* "Chip Away the Non-Elephant")
;;  ** Chip Away the Non-Elephant **
;;
;;  There's a joke about a sculptor who, when asked how he created
;;  such a life-like statue of an elephant, said that he just chipped
;;  steadily away, removing marble that did not resemble an elephant.
;;  (Actually, Michelangelo supposedly said something similar.)
;;
;;  Icicles lets you sculpt this way too - it is in fact a common
;;  Icicles usage idiom.  There are two ways to say, "I do not want
;;  that" when it comes to possible completions:
;;
;;  * The `delete' key or `S-mouse-2' says, "Get rid of this
;;    completion candidate."
;;
;;  * `C-~' says "I want all possible completions *except* those that
;;    are the current matches."  That is, "Remove all of this, and let
;;    me see what's left."  `C-~' takes the complement of the current
;;    set of matches, using the initial set of possible candidates as
;;    the universe of discourse.
;;
;;  In other words, instead of coming up with input that you want a
;;  completion to match, get rid of one or all of the candidates that
;;  do match.  You can keep clicking `mouse-2' while holding Shift, or
;;  keep hitting `delete' (without Shift), to chip away at the set of
;;  possible completions.  If there are several candidates in a row
;;  that you want to eliminate, just hold down the `delete' key until
;;  they're gone.
;;
;;  So that you can use `delete' this way to delete candidates one
;;  after the other, in order, the next candidate is chosen each time
;;  you delete one.  This means that it becomes the current candidate
;;  in the minibuffer.  You can, however, use `M-k' to clear the
;;  minibuffer or use `C-l' (bound to command
;;  `icicle-retrieve-previous-input') to clear the minibuffer and
;;  retrieve your last real input - see (@> "History Enhancements").
;;
;;  `delete' works well to delete isolated candidates or groups of
;;  candidates that are in order (the current sort order), one right
;;  after the other, and you can of course combine it with positive
;;  matching.
;;
;;  Note: In Emacs releases prior to Emacs 22, `delete' has no real
;;  effect on file-name completion candidates (but it works fine on
;;  non file-name candidates).  It removes them temporarily, but they
;;  are not really removed as possible candidates, so `TAB' and
;;  `S-TAB' will still show them as candidates.
;;
;;  `C-~' is particularly handy in combination with progressive
;;  completion (`M-*' or `S-SPC') to narrow down a set of candidates,
;;  especially when you are not exactly sure what you are looking for.
;;  You can repeat `C-~' with different inputs to eliminate everything
;;  matched by each of them.  In other words, this is a variable-size
;;  chisel, and you can use it to remove very large chips.
;;
;;  For instance, suppose you are looking for a standard Emacs command
;;  involving buffers.  You try `M-x buff S-TAB', but that shows
;;  zillions of matches.  Suppose that you know you do not want a
;;  command in some 3rd-party package.  You proceed to eliminate
;;  those, progressively, using something like this:
;;
;;    S-SPC ediff C-~ ibuffer C-~ icicle C-~ Buffer-menu C-~ ps- C-~
;;          ido C-~ search-buffers C-~ moccur C-~ swbuff C-~
;;
;;  And so on.  That is, instead of using `M-*' or `S-SPC' repeatedly
;;  to specify multiple patterns that candidates must match, you use
;;  `C-~' repeatedly (after an initial `M-*' or `S-SPC'), to chip away
;;  candidates you do not want.  You could, alternatively, hold down
;;  the `delete' key to eliminate each of these groups of command
;;  names.  There are over 100 commands whose names begin with
;;  `ediff', however, so `S-SPC C-~' can be quicker in that case.  It
;;  can definitely be quicker when apropos matching is involved.  And
;;  you can of course combine the fine chiseling of `delete' with the
;;  variable-size chiseling of `C-~'.
;;
;;  See (@> "Sets of Completion Candidates") for more about `C-~'.
;;
;;(@* "Choose Before You Act")
;;  ** Choose Before You Act **
;;
;;  The opposite operation from chipping away at a set of candidates
;;  to refine it is to build up a set of candidates that you want to
;;  act on.  This too is easy with Icicles.
;;
;;  In some user interfaces, including Dired in Emacs, you can mark
;;  items in a checklist and then, when you've selected the items you
;;  want and verified the list, act on those that are selected.  You
;;  might do this, for instance, if you were deleting some files.
;;  Icicles lets you interact with completion candidates this same
;;  way.
;;
;;  You do this by building up a saved set of candidates, and then
;;  retrieving these saved candidates later.  You can use the
;;  retrieved candidates just as you would any current set of
;;  candidates.  One of the things you can do is act on all of them,
;;  that is, act on each, in turn.  You do that with `C-!'.
;;
;;  Of course, if you can use a regexp to match exactly the candidates
;;  you want to act on, then you need not bother to save and retrieve
;;  them, before acting on them: you can see them all alone in buffer
;;  `*Completions*'.  Here's an exercise in choosing candidates to
;;  save with the mouse in `*Completions*':
;;
;;  C-x C-f  i c i  TAB - Match all file names that begin with `ici'.
;;
;;  Click `mouse-1' inside (or to the left of) `icicles-face.el'. [*]
;;  Click `mouse-3' inside (or to the right of) `icicles-mode.el'.
;;  Click `mouse-3' again, in the same place.
;;  Click `M-S-mouse-2' on each of `icicles.el' and `icicles-cmd1.el'.
;;
;;  [* If you click `mouse-1' on a candidate and (starting with Emacs
;;  22) `mouse-1-click-follows-link' is an integer, then you will need
;;  to hold the mouse button depressed longer than that many seconds,
;;  or else that candidate will simply by chosen.  If the value is
;;  `t', then this will not work at all.  Any other value presents no
;;  problem.  (Personally, I use `nil'.)]
;;
;;  The candidates that you selected - those between `icicles-face.el'
;;  and `icicles-mode.el', inclusive, plus `icicles.el' and
;;  `icicles-cmd1.el', are highlighted specially in buffer
;;  `*Completions*', and feedback in the minibuffer tells you that
;;  they were "saved", which you can also think of as "marked".
;;
;;  Next, use `C-M-<'.  This retrieves the set of saved candidates;
;;  that is, it replaces the current set of candidates with the saved
;;  candidates.  If you now use `C-!', it applies the action to each
;;  candidate.  In this case, the action is to visit the file (`C-x
;;  C-f').
;;
;;  The combination of saving (marking) candidates and then retrieving
;;  only those you have saved is like progressive completion or
;;  chipping away: it is another way of progressively narrowing the
;;  set of candidates that you act on.
;;
;;  See (@> "Choose All Completion Candidates") for more about `C-!'.
;;  See (@> "Sets of Completion Candidates") for more about saving and
;;  retrieving sets of candidates.
;;
;;(@* "Help on Completion Candidates")
;;  ** Help on Completion Candidates **
;;
;;  Sometimes, you'd like to be able to ask for help about individual
;;  completion candidates while you're in the process of choosing one.
;;  That is the purpose of the Icicles `C-M-' key bindings available
;;  during completion.
;;
;;  The simplest such bindings are `C-M-RET' and `C-M-mouse2'.  They
;;  each do the same thing: provide help on the current candidate.
;;  You can use them during cycling or whenever you've narrowed the
;;  choice down to a single candidate.  You can check this way, before
;;  you execute a command you're unsure of.
;;
;;  During completion, you can also cycle among the doc strings for
;;  the candidates that match your input, by holding `C-M-' while
;;  using any of the cycling keys:
;;
;;  - `C-M-down', `C-M-up', or `C-M-' + wheel - current-mode matching
;;  - `C-M-next', `C-M-prior'                 - apropos matching
;;  - `C-M-end',  `C-M-home'                  - prefix matching
;;
;;  See (@> "Prefix Completion and Apropos Completion")).
;;
;;  This gives you a very useful on-the-fly apropos feature - use it
;;  while you're completing a command, to check the difference between
;;  several possible commands.  Or just use it to browse doc strings,
;;  to learn more about Emacs.
;;
;;  See (@> "Get Help on Completion Candidates") for more about this.
;;
;;(@* "Perform Multiple Operations in One Command")
;;  ** Perform Multiple Operations in One Command **
;;
;;    C-x C-f  i c i  TAB - Find a file whose name starts with `ici'.
;;
;;    down (that is, the down arrow) ... until you get to candidate
;;                                       `icicles-cmd1.el'
;;
;;    RET - Open file `icicles-cmd1.el'.
;;
;;  Nothing new here.  Now try the same thing, but use `C-RET'
;;  (`C-return') instead of `RET' (`return).  The command is not
;;  ended, and you can continue to choose files to open:
;;
;;    C-x C-f  i c i  TAB - Find a file whose name starts with `ici'.
;;
;;    down ... until you get to `icicles-cmd1.el'
;;
;;    C-RET - Open file `icicles-cmd1.el'.
;;
;;    down ... until you get to `icicles-opt.el'
;;
;;    C-RET - Open file `icicles-opt.el'.
;;
;;    down ... until you get to `icicles.el'
;;
;;    RET - Open file `icicles.el' (end).
;;
;;  You just opened three files in a single command.  Command
;;  `icicle-file' (`C-x C-f') is an Icicles multi-command.  You can
;;  tell if a command is a multi-command when you execute it - if so,
;;  the input prompt is prefixed by `+'.  So, for example, when you
;;  used `C-x C-f', the prompt was "+ File or directory:".  Icicles
;;  menu items that are multi-commands are also prefixed by `+'.
;;
;;  In addition to using `down' (or `end' or `next') and choosing
;;  (acting on) candidates with `C-RET', you can combine these
;;  operations by using `C-down' (or `C-next'): act on candidates in
;;  succession.  And, as mentioned, you can use `C-!'  to act on all
;;  candidates at once.
;;
;;  There are many possible uses of multi-commands.  They all make use
;;  of the same key bindings, which begin with `C-'.  These keys are
;;  analogous to the `C-M-' keys that provide help on completion
;;  candidates.
;;
;;  See (@> "Multi-Commands") for more information about Icicles
;;  multi-commands.
;;
;;(@* "Match Multiple Things Together")
;;  ** Match Multiple Things Together **
;;
;;  Some Icicles commands that ask for your input provide completion
;;  candidates that are multi-part, called "multi-completions".
;;
;;  For instance, with a non-positive prefix argument (or with `C-u
;;  C-u'), command `icicle-locate-file' lets you find files anywhere
;;  under a given directory (or under each of a set of directories you
;;  choose) by matching the file name or the last-modification date,
;;  or both.  The first part of the multi-completion is the file name;
;;  the second part is the date.  The two-part candidates are shown in
;;  `*Completions*' like this:
;;
;;    c:/foo/bar.el
;;    2012 09 24 16:20:14
;;
;;    c:/foo/bar-none.el
;;    2012 08 06 10:02:17
;;
;;  You use the key sequence `C-M-j' to separate the two parts of your
;;  input.  So if you type `.* C-M-j .*14 S-TAB' then your input
;;  matches only the first candidate above.  The first `.*' matches
;;  any file name, and the `.*14' matches the date of only the first
;;  candidate.
;;
;;  If you instead type `.*on S-TAB' then only the second candidate is
;;  matched, by its file name this time.  In this case you do not
;;  bother matching a date - no `C-M-j' or second part, just a first
;;  part to match.
;;
;;  Notice that I said `S-TAB', not `TAB'.  You use apropos, not
;;  prefix, completion with multi-completion candidates, at least
;;  whenever you use an input pattern that matches other than just the
;;  first multi-completion part.
;;
;;  Command `icicle-fundoc' describes a function.  You match the
;;  function name or the function documentation or both, as the two
;;  multi-completion parts.  Sometimes it is easy to choose by name,
;;  sometimes by doc content, sometimes by a combination.
;;
;;  By default, `C-x b' is bound to `icicle-buffer' in Icicle mode.
;;  Here, the second multi-completion part is never shown.  The first
;;  part matches buffer names, so if you do not input a second part
;;  then `C-x b' just matches buffer names, as you are used to (except
;;  that Icicles completion is available - apropos, progressive,
;;  etc.).
;;
;;  The second multi-completion part is buffer content.  Matching
;;  searches the available buffers (whose names match the first part
;;  of your input, if present), and only those whose contents match
;;  the second part of your input remain candidates.  You see the
;;  buffer-name matches in `*Completions*', but not the content
;;  matches.
;;
;;  For example:
;;
;;    C-x b foo             ; Match buffer names against `foo'
;;    C-x b C-M-j toto      ; Match buffer contents against `toto'
;;    C-x b foo C-M-j toto  ; Match both buffer name and contents
;;
;;  See (@> "Multi-Completions").
;;
;;(@* "Perform Alternative Operations on the Fly")
;;  ** Perform Alternative Operations on the Fly **
;;
;;  (If this section seems a bit weird or advanced to you, just skip
;;  it the first time through.)
;;
;;    C-x C-f  i c i TAB - Find a file whose name starts with `ici'.
;;
;;    down ... until you get to candidate `icicles-cmd1.el'
;;
;;    C-S-RET - You are prompted to choose a function to apply.
;;
;;    f i n d e TAB RET - Choose function `finder-commentary'.
;;
;;    down ... until you get to candidate `icicles-fn.el'
;;
;;    C-S-RET TAB TAB ... until you get to `find-file-read-only'.
;;
;;    RET - Visit file `icicles-fn.el' in read-only mode.
;;
;;    C-k TAB - Kill rest of input, then complete the prefix `ici'.
;;
;;    C-|  b y t e - c TAB - Byte-compile all files matching `ici'.
;;
;;    TAB ... until you get to `icicles-doc1.el', then RET to visit.
;;
;;  What's going on?  `C-S-RET' (`C-S-return') invokes an alternative
;;  action on the current completion candidate.  Here, you do this, in
;;  turn, for the file-name candidates `icicles-cmd1.el' and
;;  `icicles-fn.el'.  `C-|' invokes an alternative action on *each* of
;;  the current completion candidates.  Here, you do this for each
;;  file name that begins with `ici'.  Finally, you cycle to
;;  `icicles-doc1.el' and hit RET to visit that file.
;;
;;  The alternative action for `C-x C-f' (command `icicle-file')
;;  prompts you for a function to apply to the current completion
;;  candidate (for `C-S-RET') or to all candidates (for `C-|').
;;
;;  Here, you choose function `finder-commentary' to visit the
;;  Commentary of file `icicles-cmd1.el', function
;;  `find-file-read-only' to visit file `icicles-fn.el' in read-only
;;  mode, and function `byte-compile-file' to byte-compile all files
;;  whose names start with `ici'.
;;
;;  You can use `C-u' with a function you choose, to pretty-print its
;;  result (in buffer `*Pp Eval Output*', if too large for the echo
;;  area).  That is useful for functions that have no side effects.
;;  For this to work, use `C-RET', not `RET', to choose the function.
;;
;;  Each command defines its own alternative action, but many Icicles
;;  commands have the behavior described here for `icicle-file': their
;;  alternative action is to let you apply any function that is
;;  appropriate for the given type of candidate (here, file names).
;;
;;  You can even enter an appropriate lambda expression, instead of
;;  completing to one of the function candidates provided.  For
;;  example, you could use `C-|' with the following input to copy all
;;  Icicles libraries to directory `ICICLES':
;;
;;    (lambda (f) (copy-file f "ICICLES" t))
;;
;;  Note that function `copy-file' is effectively curried here, to
;;  create a function of a single argument on the fly.
;;
;;  See Also: (@> "Alternative Actions").
;;
;;(@* "Completion Status Indicators")
;;  ** Completion Status Indicators **
;;
;;  You can always know whether completion is possible when you are
;;  inputting text in the minibuffer and, if so, what kind of
;;  completion.  Completion status is indicated in two places: (1) at
;;  the beginning of the minibuffer prompt and (2) in the `Icy'
;;  minor-mode lighter in the mode line.  The second is optional,
;;  controlled by option `icicle-highlight-lighter-flag'.
;;
;;  Whenever input completion is available, the prompt is prefixed by
;;  `.' or `+', indicating simple or multi-command completion,
;;  respectively.  If completion is strict (your input must match one
;;  of the candidates), then this character is enclosed in a box.  If
;;  completion is lax (permissive), there is no box.
;;
;;  The `Icy' minor-mode lighter text is highlighted red during
;;  completion.  `+' is appended to the lighter (`Icy+') for
;;  multi-command completion, `||' is appended if completion
;;  candidates are multi-completions, and the lighter is boxed for
;;  strict completion.  When minibuffer input is read without Icicles
;;  completion, the lighter is not highlighted in any way.
;;
;;  If the list of candidates shown in `*Completions*' is truncated
;;  (because of option `icicle-max-candidates'), then the lighter text
;;  is suffixed by `...'.  So if you see `...' then you know that if
;;  you increase `icicle-max-candidates' (e.g. by using `C-x #' during
;;  completion) then more candidates will be available.  See
;;  (@file :file-name "icicles-doc2.el" :to "Customization and General Tips")
;;  for info about `C-x #' and option `icicle-max-candidates'.
;;
;;  In addition, the lighter text (with or without `+', `||', and
;;  `...') is `Icy' if completion is case-sensitive and `ICY' if not.
;;  You can toggle case-sensitivity at any time using `C-A' (that is,
;;  `C-S-a') in the minibuffer.
;;
;;  The faces used for this highlighting in the minibuffer and the
;;  mode line are `icicle-completion',
;;  `icicle-multi-command-completion', and
;;  `icicle-mustmatch-completion'.  Consult their doc strings for more
;;  information.  These faces are combined to produce the various
;;  highlighting effects - keep that in mind if you customize them.
;;
;;  When you are inputting, keep an eye out for this highlighting.  If
;;  you do not see it when you are prompted for input, it means that
;;  Icicles input completion is not available.  This in turn means
;;  that `S-TAB' is available, not for input completion, but for key
;;  completion - see (@> "Key Completion").
;;
;;(@* "Icicles Search")
;;  ** Icicles Search **
;;
;;  Icicles provides a unique way of searching incrementally.  Command
;;  `icicle-search' (`C-c `') is a multi-command.  In this case, the
;;  completion candidates are the buffer occurrences that match a
;;  regexp that you input.  `C-RET' visits a search-hit candidate, and
;;  `C-next' visits a candidate and prepares to visit the next in
;;  succession.  If you visit file `icicles-doc1.el', which contains
;;  the text you are reading now, do this in that buffer:
;;
;;    C-c `
;;
;;    Search within contexts (regexp): . * r e c u r s i v e . *  RET
;;
;;    Search within contexts defined by the regexp `.*recursive.*'.
;;
;;    Choose an occurrence: S-TAB - Show the search hits, in buffer
;;    `*Completions*' (optional).
;;
;;    C-next ... - Cycle among the search hits, navigating to them in
;;    turn.
;;
;;    S-TAB next ... - Cycle among the search hits without navigating.
;;
;;    next ... C-RET next ... C-RET - Cycle to particular hits and
;;    visit (only) those hits.
;;
;;    next ... RET - Cycle to a hit and stay there (end).
;;
;;
;;    C-c `
;;
;;    Search within contexts (regexp): M-p RET
;;
;;    Search again within `.*recursive.*' (`M-p' uses input history).
;;
;;    S-TAB e d i t C-next ... - Search for the substring `edit'
;;    within all search hits for `.*recursive.*'.  Cycle among the
;;    matches.  The substring `edit' is highlighted inside the
;;    (differently) highlighted `.*recursive.*' hits.  Whatever you
;;    type filters the initial set of hits.
;;
;;    M-k - Empty the minibuffer, then S-TAB.  All `.*recursive.*'
;;    hits are restored as candidates.  Again, whatever your input is
;;    (nothing, now), the set of candidates is dynamically updated to
;;    match it.
;;
;;    t \ w + n S-TAB C-next ... - Search for matches of the regexp
;;    `t\w+n' within all search hits for `.*recursive.*' - that is,
;;    `t' followed by at least one other word character, followed by
;;    `n'.  Whatever the regexp `t\w+n' matches (`thin', `then',
;;    `traighten', `tion') is highlighted inside each candidate.
;;
;;    RET - Stop searching at the current candidate (end).
;;
;;  Now try the same thing, but first use `C-^' in the minibuffer
;;  (e.g. after you enter `.*recursive.*').  That toggles an Icicles
;;  search option for highlighting your input matches.  The behavior
;;  is the same as before, except that all matches to your input are
;;  highlighted at once, not just the current match.  And not only the
;;  exact match is highlighted, but the longest common match among all
;;  input matches is highlighted: If your input is `edi', then `edi'
;;  is highlighted (there is no longer common match), but if you input
;;  the four characters `e d i t', then ``abort-recursive-edit'' is
;;  highlighted.  You can use `C-^' at any time during searching to
;;  change the highlighting behavior.
;;
;;  Now try the same thing, but first select some text.  The search is
;;  confined to the active region (selection) instead of the entire
;;  buffer.
;;
;;  Now try the same thing (without a region), but use a negative
;;  prefix argument such as `C--' with `C-c `'.  This time, after you
;;  input the regexp to search for, you are prompted for one or more
;;  files to search.  This too is multi-command input: you can input
;;  any number of file names, using completion.
;;
;;    C-- C-c `
;;
;;    Search within contexts (regexp): . * r e c u r s i v e . *  RET
;;
;;    Search within contexts defined by the regexp `.*recursive.*'.
;;
;;    Choose file (`RET' when done): i c i TAB - Choose among file
;;    candidates that begin with `ici' (shown in `*Completions*').
;;
;;    C-! - Choose all matching file names: icicles-cmd1.el,
;;    icicles-cmd2.el, icicles-doc1.el, icicles-doc2.el,
;;    icicles-face.el, icicles-fn.el, icicles-mac.el, icicles-mcmd.el,
;;    icicles-mode.el, icicles-opt.el, icicles-var.el, and icicles.el.
;;
;;    Choose an occurrence: S-TAB - Show the hits in buffer
;;    `*Completions*' (optional).
;;
;;    C-next ... - Cycle among the search hits in all chosen
;;    files...
;;
;;  Just as you can choose particular search hits to visit, using
;;  `C-RET', so you can use `C-RET' to choose particular files (whose
;;  names match the input, e.g. ici) to search.  Just as you can visit
;;  search hits in order, using `C-next' (or `C-end' or `C-down'), so
;;  you can use `C-next' (or `C-end' or `C-down') to choose files to
;;  visit, one after the other.
;;
;;  When you input the initial regexp (`.*recursive.*' in the example
;;  above) to `icicle-search', you can use completion to retrieve a
;;  regexp that you entered previously.
;;
;;  You can use `C-`' in the minibuffer to toggle escaping of regexp
;;  special characters.  Use that if you want to find a literal string
;;  - for example, if you want to search for the string `form.' and
;;  not text that matches the regexp `form.' (`form' followed by any
;;  character except newline).  If you use `C-`' during Icicles
;;  search, start the search over again for the toggle to take effect.
;;
;;  Oh, can you use progressive completion with Icicles search?  Sure.
;;  And chipping away the non-elephant (complementing)?  Yep.  Try
;;  using vanilla Emacs incremental search to find a line that
;;  contains a given set of words in any (unknown) order and that also
;;  does not contain another given set of words.  No can do.  But that
;;  is simple using Icicles search.  (Yes, you can do it using
;;  `grep'.)
;;
;;  And while you're searching, you can perform on-the-fly, on-demand
;;  replacement.  You tell Emacs whenever you want to replace text,
;;  instead of replying to an endless litany of `query-replace'
;;  queries.  Unlike `query-replace', you need not visit search
;;  matches successively or exhaustively.  You can visit and replace
;;  selected matches in any order.  And you can even change the order
;;  (using `C-,') in which search hits appear and are navigated
;;  sequentially.
;;
;;  In addition to Icicles search (which is also incremental), Icicles
;;  offers some enhancements to the standard Emacs incremental search,
;;  Isearch:
;;
;;  * You can reuse previous Isearch search strings using Icicles
;;    completion.  There are two ways you can do this:
;;
;;    . `M-TAB' or `C-M-TAB': Complete the current search string,
;;       choosing a previous one to replace it.
;;
;;    . `M-o': Append one or more previous search strings to the
;;      current one.  This is similar to `M-o' in the minibuffer
;;      (`icicle-insert-history-element'), except that a prefix
;;      argument has no effect here: no candidate is wrapped with
;;      "...", and no space character is appended.
;;
;;   (The actual keys for this are those defined by user options
;;   `icicle-isearch-complete-keys' and
;;   `icicle-isearch-history-insert-keys', respectively.)
;;
;;  * You can start Icicles search from Isearch.  The current Isearch
;;    search string becomes the starting point for the Icicles search
;;    regexp.  You can edit it or type something different.  And you
;;    can complete what you type against the Isearch regexp history.
;;    You can optionally define search contexts with a regexp and then
;;    search for the Isearch string within those contexts.
;;
;;  See Also:
;;
;;  * (@file :file-name "icicles-doc2.el" :to "Icicles Search Commands, Overview")
;;    for more about searching with Icicles.
;;
;;  * (@file :file-name "icicles-doc2.el" :to "Search and Replace")
;;    for information about replacing selected search hits.
;;
;;  * (@> "Expanded-Common-Match Completion") for more about Icicles
;;    expansion of your input to a common match among all candidates.
;;
;;  * (@> "Isearch Enhancements")
;;
;;  * (@> "Using Completion to Insert Previous Inputs: `M-o'") for
;;    more about `M-o' - you can use it anywhere to complete against
;;    previous inputs.
;;
;;(@* "Complete Key Sequences Too")
;;  ** Complete Key Sequences Too **
;;
;;  Try `S-TAB' at the top level (without first invoking a command
;;  that reads input).  Icicles presents all of the possible keys and
;;  their bindings in the current context - for completion.  For
;;  example, if you are in Dired mode, the completion candidates
;;  include all key sequences in the global map and the Dired-mode map
;;  (and any current minor-mode maps, such as Icicle mode).
;;
;;  (The documentation always refers to the key that performs key
;;  completion as `S-TAB'.  Actually, it is `S-TAB' only by default.
;;  You can customize it, using option `icicle-key-complete-keys'.)
;;
;;  You can then type part of a key name or a command name, and hit
;;  `S-TAB' again to apropos-complete your input.  You can navigate
;;  down the key-sequence hierarchy by completing a key sequence piece
;;  by piece.
;;
;;  Completion candidates are a key description, followed by the value
;;  of option `icicle-complete-keys-separator', followed by the
;;  associated command name.  The option value is "  =  " by default.
;;
;;    S-TAB to see the available keys at top level
;;
;;    Click (using `mouse-2') candidate `C-x  =  ...', to see the keys
;;    that start with `C-x'
;;
;;    Click `r  =  ...', to see the keys that start with `C-x r'
;;
;;    Click `b  =  bookmark-jump', to invoke that command and visit a
;;    bookmark
;;
;;  Whenever you're completing a prefix key, such as `C-x', you can
;;  click `..' to navigate back up the key-sequence hierarchy.  For
;;  instance, if you are completing `C-x p', click `..' to go back to
;;  completing `C-x', and then click `..' to go back to the top level.
;;
;;  The available keys at any level include the following important
;;  keys, which means that you can use Icicles key completion to do
;;  almost anything in Emacs:
;;
;;  * `M-x' - Execute an arbitrary command.
;;    `M-x' is treated as `ESC-x', so complete first `ESC  =  ...',
;;    then `x  =  icicle-execute-extended-command'.
;;
;;  * `M-:' - Evaluate any Emacs-Lisp expression.
;;    In Icicles, `M-:' gives you a quick pop-up mode for evaluating a
;;    Lisp sexp.  Most of the normal Emacs-Lisp mode bindings are in
;;    effect, except that `RET' evaluates the minibuffer contents and
;;    pretty-prints the result.  You can also use it with a prefix arg
;;    (`C-u M-:') to insert the result of such an on-the-fly Lisp
;;    evaluation into the current buffer (including the minibuffer).
;;
;;  * `menu-bar  =  ...' - Invoke any menu-bar menu.
;;    Continue completing, to navigate the entire menu hierarchy.
;;
;;  You can start directly with a key prefix, and then hit `S-TAB' to
;;  complete it - you need not start with `S-TAB'.  You can use
;;  Icicles key completion to learn key bindings - `C-M-mouse-2'
;;  displays help on any key.
;;
;;  Instead of clicking a completion candidate with `mouse-2', you can
;;  of course type part of the key name or command name, and then
;;  complete the name and enter it.  Gotcha: `S-TAB' uses apropos
;;  completion, by default, so remember that typing `.' matches any
;;  character (except a newline).  To match only `..' (to go up a
;;  level), either use prefix completion (`TAB') or escape the regexp
;;  special character: `\.\.' (or use `^\.').  Or cycle to it.
;;
;;  Icicles key completion is also available in the minibuffer, but
;;  there, since `S-TAB' performs apropos completion of your input,
;;  the key that initiates key completion is `M-S-TAB', not `S-TAB'.
;;  (If your window manager steals `M-S-TAB' then try `ESC S-TAB' or
;;  customize option `icicle-key-complete-keys-for-minibuffer'.)
;;
;;  See (@> "Key Completion") for more about Icicles key completion.
;;
;;(@* "Available for Almost Any Input")
;;  ** Available for Almost Any Input **
;;
;;  All of this works not only for the input of commands, with `M-x',
;;  but for the input of nearly anything.  For instance, you can use
;;  `C-x b' (`switch-to-buffer') and cycle among buffer names.  Or use
;;  `C-h v' (`describe-variable') and cycle among variable names.  It
;;  works whenever a command reads input with completion.
;;
;;  Whenever you're in Icicle mode, you see "Icy" in the mode-line.
;;
;;(@* "Component Icicles Libraries")
;;  ** Component Icicles Libraries **
;;
;;  Icicles is composed of the following libraries.  When you load the
;;  driver library, `icicles.el', the others are all loaded
;;  automatically .
;;
;;    `icicles.el'      - driver library
;;    `icicles-doc1.el' - first part of the doc (this!)
;;    `icicles-doc2.el' - second part of the doc
;;    `icicles-cmd1.el' - top-level commands (part 1)
;;    `icicles-cmd2.el' - top-level commands (part 2)
;;    `icicles-face.el' - faces
;;    `icicles-fn.el'   - non-interactive functions
;;    `icicles-mac.el'  - macros
;;    `icicles-mcmd.el' - minibuffer commands
;;    `icicles-mode.el' - Icicle mode definition
;;    `icicles-opt.el'  - user options (variables)
;;    `icicles-var.el'  - internal variables
;;
;;  Libraries `icicles-doc1.el' and `icicles-doc2.el' are not really
;;  libraries.  They contain only comments, with the Icicles doc.
;;
;;  Library `lacarte.el' is not part of Icicles, but it is especially
;;  useful when used with Icicles.
;;
;;(@* "If You Are an Emacs-Lisp Programmer")
;;  ** If You Are an Emacs-Lisp Programmer **
;;
;;  If you are an Emacs-Lisp programmer, this is the no-brainer,
;;  nutshell view of how to take advantage of Icicles in your own code
;;  that calls `completing-read' or `read-file-name':
;;
;;    Add this line to your library: (require 'icicles nil t)
;;
;;  That is really all you need to do.  And there is no consequence if
;;  users do not have Icicles (no load error is raised, because of the
;;  non-`nil' third argument).  In other words, there is no reason not
;;  to add this soft `require', unless your library somehow conflicts
;;  with Icicles features.  (Even then, users will not be affected
;;  unless they turn on Icicle mode.)
;;
;;
;;  For more (and there is a lot more), read on...
 
;;(@* "Inserting Text Found Near the Cursor")
;;
;;  Inserting Text Found Near the Cursor
;;  ------------------------------------
;;
;;  Most of Icicles is about completing text that you type in the
;;  minibuffer against some set of possible completion candidates.
;;  This feature is not.  It is related only in the sense that it is
;;  also about inputting text that is already available elsewhere.
;;
;;  Some Emacs commands provide, as the default value for minibuffer
;;  input, a word or other text at the cursor position (aka "point").
;;  You can insert this default value in the minibuffer with `M-n'.
;;  Icicles option `icicle-default-value' can be used to automatically
;;  insert the default value into the minibuffer as an initial value,
;;  if you prefer that optional behavior (I do; many people do not).
;;
;;
;;(@* "FFAP: Find File At Point")
;;  ** FFAP: Find File At Point **
;;
;;  Sometimes you would like to use the text at the cursor, but the
;;  command asking for input does not let you retrieve that text as
;;  the default value.  For example, if the text at point is a file
;;  name, you might like to use it with `C-x C-f' to open that file.
;;  Or, if the text is a URL, you might want to visit it using a Web
;;  browser.
;;
;;  Some Emacs-Lisp libraries, such as `ffap.el', have as their
;;  specific purpose to help you do this.  "Ffap" stands for
;;  `find-file-at-point', the main command in that library.  It tries
;;  to interpret the text at point and "do the right thing" with it:
;;  visit a file, open a URL in a Web browser, and so on.
;;
;;  If you like, you can use library `ffap.el' with Icicles.  All
;;  Icicles features are then available during file-name and URL
;;  completion.  And if you like `ffap.el', you might also like to try
;;  my extension library `ffap-.el'.  However, if you use ffap with
;;  Icicles, you might not want to use the ffap key bindings,
;;  preferring the Icicles bindings or the standard Emacs bindings for
;;  keys such as `C-x C-f'.  (In that case, do not call function
;;  `ffap-bindings'.)
;;
;;  Icicles provides a couple of simple ways to take advantage of
;;  `ffap-guesser', which is the ffap function that guesses which
;;  string at the cursor position you want to grab, without
;;  sacrificing any key bindings to ffap.  One way is to use `M-.'
;;  (command `icicle-insert-string-at-point') at any time in the
;;  minibuffer.  It grabs text at or near the cursor and yanks it into
;;  the minibuffer.  One of the alternative types of thing it grabs is
;;  whatever text `ffap-guesser' guesses.
;;
;;(@* "Proxy Candidates, `M-.'")
;;  ** Proxy Candidates, `M-.' **)
;;
;;  Another way is to use one of the proxy completion candidates
;;  `*point file name*' or `*mouse-2 file name*' whenever Emacs asks
;;  you to input a file name (provided option
;;  `icicle-add-proxy-candidates-flag' is non-`nil' - toggle with
;;  `C-M-_').  The former picks up the file name at point, just like
;;  `M-.'.  The latter lets you click a file name anywhere with
;;  `mouse-2' to pick up the name.
;;
;;  Using `M-.' or a proxy candidate on demand, instead of binding
;;  keys to ffap commands, lets you control which buffer text you use
;;  as minibuffer input and how that text should be interpreted (file
;;  name, URL, and so on).  You can change the behavior of `M-.'
;;  (which string-inserting functions are used) by customizing user
;;  option `icicle-thing-at-point-functions'.
;;
;;  See Also: (@> "*Completions* Display").
;;
;;(@* "Repeat `M-.' To Grab More or Different")
;;  ** Repeat `M-.' To Grab More or Different **
;;
;;  Actually, `M-.' acts differently if you use it successively.
;;  Successive uses of `M-.' grab and insert either 1) alternative
;;  bits of text (different text "things") or 2) successive bits of
;;  text.  The default behavior is #1, but you can change this choice
;;  by customizing option `icicle-default-thing-insertion' (setting it
;;  to `more-of-the-same', instead of `alternatives').
;;
;;  As an example of grabbing successive bits of text (#2), suppose
;;  that the cursor is at the beginning of the word "use" in the
;;  previous paragraph.  Then, during minibuffer input, suppose that
;;  you use `M-. M-. M-.'.  Each time you hit `M-.', another word is
;;  inserted in the minibuffer:
;;
;;    use
;;    use it
;;    use it successively
;;    ...
;;
;;  The rest of this section is a bit technical, so you might want to
;;  skip it if you are reading the Icicles doc for the first time.  It
;;  details the behavior and definitions of options
;;  `icicle-default-thing-insertion' and
;;  `icicle-thing-at-point-functions', and how to temporarily override
;;  those settings interactively.
;;
;;  Option `icicle-thing-at-point-functions' controls which text at or
;;  near the cursor `M-.' inserts into the minibuffer.  It is a cons
;;  cell, that is, an ordered pair:
;;
;;  * The car (first part) is a list of functions that grab different
;;    kinds of strings at or near point (#1, above).  Any number of
;;    functions can be used.  They are used in sequence by `M-.'.  I
;;    recommend that you also use library `thingatpt+.el', so that
;;    `M-.' can take advantage of the string-grabbing functions it
;;    defines.
;;
;;  * The cdr (second part) is a function that advances point one text
;;    thing (#2, above).  Each time command `M-.' is used
;;    successively, this is called to grab more things of text (of the
;;    same kind).  The default function grabs successive words.
;;
;;  If either the car or cdr is empty, then the other alone determines
;;  the behavior of `M-.'.  Otherwise, option
;;  `icicle-default-thing-insertion' determines whether the car or the
;;  cdr is used by `M-.'.
;;
;;  For example, if the value of `icicle-default-thing-insertion' is
;;  `alternatives' (the default value), then repeated use of `M-.'
;;  inserts a different kind of thing at point each time.  By default,
;;  these are the thing types, in order:
;;
;;    `non-nil-symbol-name-nearest-point' (*) or `symbol-at-point'
;;    `word-nearest-point' (*) or the word at point
;;    `list-nearest-point-as-string' (*), the first enclosing list
;;    `list-nearest-point-as-string' (*), the second enclosing list
;;    `list-nearest-point-as-string' (*), the third enclosing list
;;    `ffap-guesser'
;;    `thing-at-point-url-at-point'
;;
;;  The alternatives marked with an asterisk (*) are available only if
;;  you use library `thingatpt+.el'.  Alternative `ffap-guesser' is
;;  used only if you use library `ffap.el'.
;;
;;  The first alternative inserts text that has the syntax of an
;;  Emacs-Lisp symbol name.  In practice, this can also be a file
;;  name or a URL - it can include characters such as -, /, +, ., :,
;;  @, and _.
;;
;;  The second alternative inserts a word, which includes letters, ',
;;  and -.
;;
;;  The third, fourth, and fifth alternatives insert a (non-`nil')
;;  list that is around point - three different enclosing levels.
;;
;;  The sixth alternative inserts whatever `ffap-guesser' returns: a
;;  file name or a URL at point.
;;
;;  The seventh alternative inserts a URL at point, adding prefix
;;  "http://" if needed.
;;
;;  This means that you can quickly pick up a symbol name, a list, a
;;  file name, or a URL at point.
;;
;;  If you use library `thingatpt+.el' then the first two alternatives
;;  pick up the symbol or word nearest point - the cursor need not be
;;  exactly on the symbol or word.
;;
;;  You can of course add to or replace any of the alternatives that
;;  are provided by default.
;;
;;  If you set `icicle-default-thing-insertion' to `more-of-the-same'
;;  instead of `alternatives', then repeated use of `M-.' inserts
;;  successive words into the minibuffer, as shown in the example
;;  above.
;;
;;  You need not make a final choice once and for all between
;;  `alternatives' and `more-of-the-same'.  You can also make an
;;  interactive choice by using a prefix argument (`C-u') at any time
;;  to override the value of `icicle-default-thing-insertion'.
;;
;;  If you use plain `C-u', then `M-.' flips the behavior specified by
;;  `icicle-default-thing-insertion'.
;;
;;  If you use a numeric prefix argument N (not just plain `C-u'),
;;  then:
;;
;;  * If `M-.' would normally grab the next thing of the same type,
;;    then N such things are grabbed in succession.  If N is negative
;;    then the things are grabbed successively from the left, not the
;;    right.
;;
;;  * If `M-.' would normally grab an alternative thing of a different
;;    type, then numeric N tells Icicles to grab the particular thing
;;    at point and then evaluate it as a Lisp sexp and insert the
;;    result of that evaluation in the minibuffer.  (The use of a
;;    numeric prefix arg to evaluate a Lisp sexp does require that you
;;    know when the particular thing-grabbing function that you want
;;    is coming up next.)
;;
;;  So for example, returning to the example above, with the cursor at
;;  the beginning of the word "use" in the first paragraph of this
;;  section, if you used `M-- M-. M-. M-.', then the successive
;;  insertions would be as follows:
;;
;;  differently
;;  differently if
;;  differently if you
;;  ...
;;
;;  And if you instead used `M--3 M-.', then you would immediately
;;  insert `differently if you'.
;;
;;  There are thus lots of possibilities when you use `M-.'
;;  repeatedly.  You need not bother with them until you need them.
;;  You can build up to using them all gradually.
;;
;;(@* "Resolve File Names")
;;  ** Resolve File Names **
;;
;;  Finally, something that is not directly related to the topic of
;;  this page, but fits here as well as anywhere: you can use `C-x
;;  C-f' in the minibuffer to resolve a file name to its true,
;;  absolute name.  Yes, that's the same key that is bound at top
;;  level to `icicle-file' or `find-file' or whatever, but this is
;;  about its use when you are already in the minibuffer.
;;
;;  `C-x C-f' (`icicle-resolve-file-name'), replaces a file name at or
;;  near point (in the minibuffer) with its true, absolute name.  (For
;;  it to work near point, as well as at point, you need library
;;  `thingatpt+.el'.)  If the file name is relative, it first converts
;;  it to absolute (using the default directory).  It then converts an
;;  absolute name that is a symbolic link to its target name.  You can
;;  use this anytime in the minibuffer, and you can use it on multiple
;;  parts of the same minibuffer input (e.g. shell command arguments).
;;  (This feature does not work for Emacs 20 or 21.)
;;
;;  See Also:
;;
;;  * (@> "Inserting a Regexp from a Variable or Register") for
;;    information on inserting text saved in a variable or register.
;;
;;  * (@> "Moving Between the Minibuffer and Other Buffers") for
;;    another way to insert buffer text in the minibuffer.
;;
;;  * (@> "Multi-Inputs") for ways to act on multiple minibuffer
;;    insertions all at once.
 
;;(@* "Background on Vanilla Emacs Input Completion")
;;
;;  Background on Vanilla Emacs Input Completion
;;  --------------------------------------------
;;
;;  This section reviews standard Emacs behavior regarding input
;;  completion.  It does not describe any Icicles completion features.
;;  See also (@> "README").
;;
;;  When you are prompted in the minibuffer to enter something, you
;;  are sometimes presented with a default value.  This might be
;;  automatically inserted after the prompt, initially.  If not, you
;;  can retrieve the default value yourself, using `M-n' (Emacs 21 or
;;  later).
;;
;;  Often, there is more than one reasonable default value that might
;;  make sense.  Depending on what you're being asked to enter, these
;;  "candidate default values" might be command names, buffer names,
;;  existing file names, variable names, and so on.
;;
;;  For most Emacs functions that prompt you for input, the person who
;;  wrote the function decided on the reasonable set of default
;;  values, and passed these to an "input-completing function" such as
;;  `completing-read' or `read-file-name', which prompts you and reads
;;  your input.  The programmer also decided whether you will be
;;  *required* to pick one of the default values or you will be free
;;  to enter something else.  The programmer might also have told the
;;  input-completing function to require that your input pass some
;;  special test (predicate).
;;
;;  Be aware that standard Emacs terminology does not refer to such a
;;  set of default values as "default values"; they are called
;;  "completions".  By "default value", standard Emacs terminology
;;  means only the values that you can access via `M-n'.  Icicles
;;  refers to all such potential inputs indifferently as "default
;;  values", "completions", "completion candidates", and "candidates".
;;  Whenever completion is not requiring you to pick one of the
;;  available candidates, they are effectively only default choices.
;;
;;  So, how do you get access to the default values that the
;;  programmer has made available to you, in order to choose one?  You
;;  hit certain keys to complete the current contents of the
;;  minibuffer (excluding the prompt).  This current, partial input is
;;  considered as a prefix of one of the default values, and it is
;;  completed in the minibuffer to the entire default value
;;  (completion).
;;
;;  Keys `TAB', `RET' (Return), and `SPC' (Space) perform different
;;  degrees of this "prefix completion" in standard Emacs.  If you
;;  type a prefix of one of the available default values, you can
;;  complete the value this way in the minibuffer, and then enter
;;  (commit) it, using `RET'.
;;
;;  (Starting with Emacs 23, what Icicles calls "prefix completion" is
;;  controlled by vanilla Emacs option `completion-styles', and the
;;  matching it uses is not limited to the `basic' style, that is, to
;;  expanding a literal prefix of one of the completion candidates.
;;  If you prefer, you can think of "prefix completion" as `TAB'
;;  completion (but you can change which key effects it).
;;
;;  But if your partial input matches more than one default value,
;;  then completion pops up the list of all matching completions for
;;  you to choose from (in buffer `*Completions*').  You choose a
;;  candidate by clicking it with `mouse-2' or placing the cursor on
;;  it and hitting `RET'.
;;
;;  Because this is the way you access the default values supplied to
;;  an input-completing function, I call those values
;;  "prefix-completion candidates".  If there is no partial input yet
;;  (the minibuffer is empty), then the entire list of default values
;;  supplied to the input-completing function appears in the pop-up
;;  `*Completions*' buffer.  See the Emacs manual (`C-h r') for more
;;  on this general completion mechanism.
;;
;;  Calls to `completing-read' and `read-file-name' are not the only
;;  places where input completion is used.  When you use `M-x'
;;  (command `execute-extended-command'), completion is also
;;  available.
 
;;(@* "Cycling Completions")
;;
;;  Cycling Completions
;;  -------------------
;;
;;  Icicles lets you use the `end' and `home' keys to cycle through
;;  the list of candidate prefix completions that match whatever input
;;  is present in the minibuffer (or all candidate completions, if
;;  there is no input in the minibuffer).  In the minibuffer, each
;;  candidate replaces your partial input, in turn, when you cycle.
;;  The prefix (root) that was completed is underlined in the
;;  minibuffer completion candidate.
;;
;;  As an alternative to using `end' to cycle forward, you can hit
;;  `TAB' repeatedly.  See (@> "Prefix Completion and Apropos Completion").
;;
;;  Suppose you use `C-x b' (command `switch-to-buffer').  You can
;;  then use `end' until the right buffer name appears in the
;;  minibuffer, then hit `RET'.  Or you can type some text that begins
;;  one or more of the buffer names, and then use `end' to cycle among
;;  those names that match that input.  If there are many candidates,
;;  typing part of the name to narrow the field can save time.
;;
;;  Another example: Suppose you use `C-h v' (`describe-variable') and
;;  type `cal'.  Use `end' to cycle among all variables that start
;;  with `cal', until you find the one you want (then hit `RET').
;;
;;  In other words, the current partial input in the minibuffer
;;  determines a matching set of default values, and those are the
;;  values that you can cycle through.  You can at any time erase or
;;  change the partial input - the list of matching candidates
;;  automatically reflects the change.
;;
;;  This also means that it's good to have a quick way to clear the
;;  minibuffer of any input, so Icicles also provides minibuffer key
;;  binding `M-k' to do that.
;;
;;  A visible and audible signal lets you know when you have reached
;;  one end of the list of completion candidates, but you can of
;;  course continue to cycle, wrapping around.
;;
;;  If the completion candidates are already displayed in buffer
;;  `*Completions*' when you try to cycle among them (because you hit
;;  `TAB'), then the current candidate is highlighted in
;;  `*Completions*' as you access it in the minibuffer with the `home'
;;  and `end' keys.  If you change the minibuffer input, then the
;;  `*Completions*' list is updated accordingly, to reflect the new
;;  set of matching candidates.  The root that was completed (the
;;  minibuffer input) is highlighted in each candidate of the
;;  `*Completions*' display.  The `*Completions*' window is
;;  automatically scrolled as needed, to show the current candidate.
;;
;;  A numeric prefix argument N means cycle forward or backward N
;;  candidates (default = 1).  A plain prefix argument (`C-u') means
;;  use the first candidate directly, instead of cycling forward or
;;  backward.
;;
;;  Do not become a cycling drone!  Input some text to narrow the set
;;  of candidates, before cycling among them to choose one.  This is a
;;  good habit to adopt, generally, in Icicles.  Most of the power of
;;  Icicles comes in your ability to filter a set of candidates.  This
;;  is especially true when it comes to regexp filtering (see
;;  (@> "Apropos Completions")).
;;
;;  Cycling and filtering work hand in hand.  If the set of candidates
;;  is small to begin with, then just cycling might be quick enough -
;;  that is the case if you move among a small set of buffers, for
;;  instance.  But with Icicles you can profitably use cycling on even
;;  a very large set of candidates - by filtering the set first.
;;
;;  Tip: Whenever you type or delete text in the minibuffer, your
;;  partial input is remembered.  When you cycle completion
;;  candidates, your input is replaced by each candidate, but you can
;;  at any time refresh the minibuffer to retrieve what you last
;;  typed.  You do this with `C-l', which is bound in the minibuffer
;;  to command `icicle-retrieve-previous-input'.  Editing a completion
;;  candidate that you have cycled into the minibuffer counts as
;;  input.  Editing tells Icicles to remember what is in the
;;  minibuffer as your last real input.  If you want to replace the
;;  candidate and go back to editing the input you had already typed
;;  before cycling, then use `C-l' - do not just delete characters
;;  from the candidate.  See (@> "History Enhancements").
;;
;;  You can change the keys that are bound to completion-candidate
;;  cycling.  And you can change whether `down' and `up' start off by
;;  cycling prefix completions or apropos completions.
;;  See (@file :file-name "icicles-doc2.el" :to "Customizing Key Bindings").
;;
;;  Finally, you can use the mouse wheel (Emacs 22 or later) to cycle
;;  candidates according to the current completion mode (prefix or
;;  apropos).  See (@> "Prefix Completion and Apropos Completion").
;;
;;  Mouse-wheel cycling works also with modifier keys: `C-M-' for
;;  candidate help, `C-' for candidate actions, and `C-S-' for
;;  alternative candidate actions.  In particular, `C-' with the wheel
;;  gives you a very quick way to visit search hits during Icicles
;;  search (and `C-S-' works for search-and-replace).
;;  (See (@> "Icicles Search Commands, Overview").)
;;
;;  If you are an Emacs-Lisp programmer, then you can use
;;  `completing-read' and `read-file-name' to define your own
;;  commands, enabling them to take advantage of Icicles completion
;;  and cycling.  The definition of command `icicle-recent-file' is a
;;  good model to follow.  Emacs has a `recentf-mode' that lets you
;;  open recently accessed files.  But this mode makes you open a file
;;  using a menu interface.  Command `icicle-recent-file' lets you use
;;  the usual `find-file' minibuffer interface, with completion and
;;  cycling among your recent files.  See sections
;;  (@> "Defining Icicles Commands") and
;;  (@file :file-name "icicles-doc2.el" :to "Note to Programmers")
;;  for more on defining your own commands with `completing-read' and
;; `read-file-name'.
 
;;(@* "Traversing Minibuffer Histories")
;;
;;  Traversing Minibuffer Histories
;;  -------------------------------
;;
;;  Perhaps you are already used to accessing past inputs with vanilla
;;  Emacs using the `down' and `up' arrow keys (or `M-n', `M-p', and
;;  `next').  If not, try it (not in Icicle mode).  You can go
;;  backward and forward in the minibuffer histories (there are
;;  different history lists for different kinds of input).  You cannot
;;  really cycle them (with wraparound), but when you get to one end
;;  you can reverse the direction.
;;
;;  Anyway, the input-cycling behavior that Icicles offers is in
;;  addition to this standard traversal of histories.  Since there
;;  are, by default, several extra pairs of keys used for history
;;  traversal, rebinding some of them to use for Icicles completion is
;;  no real loss.
;;
;;  By default, Icicles rebinds the arrow keys `down' and `up' for
;;  current-mode completion cycling.  Icicles also rebinds `end' and
;;  `home' for prefix-completion cycling, and `next' and `prior' for
;;  apropos-completion cycling.  But you still have `M-n' and `M-p'
;;  available to access past inputs (history).  And the rebindings are
;;  only for minibuffer input; global bindings are not affected.
;;
;;  You can at any time switch back and forth between input-history
;;  traversal (`M-n', `M-p') and completion cycling (`down', `up',
;;  `next', `prior', `end', `home').
;;
;;  See Also:
;;
;;  * (@> "History Enhancements") for new ways to use Emacs history
;;    lists with Icicles
;;
;;  * (@file :file-name "icicles-doc2.el" :to "Customizing Key Bindings")
;;    for how to change the default Icicles key bindings, including
;;    the keys used for candidate cycling
 
;;(@* "Apropos Completions")
;;
;;  Apropos Completions
;;  -------------------
;;
;;  Besides the prefix completion that is carried out by `TAB', and
;;  which is similar to vanilla Emacs completion, Icicles offers
;;  another way to complete your partial input in the minibuffer.
;;
;;  This is similar in effect to using command `apropos' to find the
;;  "apropos completions" of a string (except it works also for file
;;  and buffer names), so that's the term I use for this: "apropos
;;  completion".
;;
;;  Just as with prefix completion, Icicles lets you cycle among the
;;  apropos candidates.  To do this, you use keys `next' and `prior'.
;;  The root that was completed is underlined in the minibuffer
;;  completion candidate.
;;
;;  For example, suppose you use `M-x' to enter a command.  You do not
;;  remember the exact command name, but it has something to do with
;;  lines, so you type `M-x line', then hit `next' repeatedly, until
;;  you see the right "line" command - `transpose-lines', perhaps.
;;  Vanilla prefix completion does not find this command, because
;;  "line" is not a prefix of "transpose-lines".
;;
;;  Because `M-x' expects a command name, only command names are
;;  inserted into the minibuffer as the apropos-completion candidates
;;  for `M-x'.  Likewise, in other contexts, where names of other
;;  kinds of object are expected, apropos completion inserts only
;;  names of objects of the appropriate type.  Prefix completion works
;;  the same way.
;;
;;  For example, using `next' and `prior' with `C-x b at' lets you
;;  cycle through all buffers (such as `*scratch*') that have "at" in
;;  their name - only buffer names appear as candidates.
;;
;;  As an alternative to using `next' to cycle forward, you can hit
;;  `S-TAB' (`S-tab') repeatedly.  Similarly, for prefix completion
;;  you can repeat `TAB' to cycle forward.  If you prefer, you can
;;  think of "apropos completion" as `S-TAB' completion (but you can
;;  change which key effects it).
;;  See (@> "Prefix Completion and Apropos Completion").
;;
;;  Apropos completion uses a regular expression (regexp) as its input
;;  string.  You can type `M-x \bes', for instance, to find commands
;;  with "es" at the start of a word within the command name (`\b'
;;  matches the start of a word).  It will find `eshell-test' and
;;  `color-theme-blue-eshell', but not `count-lines' - "es" does not
;;  start a word in `count-lines'.  Similarly, for file names, buffer
;;  names, and so on.
;;
;;  What if you want to see the list of all completion candidates that
;;  match the minibuffer input?  Instead of cycling candidates
;;  blindly, just hit `S-TAB' (Shift TAB) at any time to display the
;;  matching candidates in pop-up buffer `*Completions*'.  This is
;;  analogous to `TAB' for prefix completion.
;;
;;  (The documentation always refers to the key that performs apropos
;;  completion as `S-TAB'.  Actually, it is `S-TAB' only by default.
;;  You can customize it, using option
;;  `icicle-apropos-complete-keys'.)
;;
;;  Everything said in section (@> "Cycling Completions") about the
;;  `*Completions*' buffer for prefix completion is also true for
;;  apropos completion.  It is updated to reflect the current set of
;;  matching candidates, and the current completion is highlighted.
;;  The root that was completed is highlighted within each candidate
;;  (first occurrence only).  Root highlighting is more important in
;;  the case of apropos completion, because the match position is
;;  different in different candidates.  In the case of apropos
;;  completion, the root is not the input string, taken literally, but
;;  the part of a candidate that the input matches.  See
;;  (@> "*Completions* Display") for additional ways to use the
;;  minibuffer with `*Completions*'.
;;
;;  Regexp matching is one of the most powerful features of Icicles.
;;  Enjoy!  Explore!  You can at any time switch back and forth
;;  between prefix completion (`end', `home'), apropos completion
;;  (`next', `prior'), and input history traversal (`M-n', `M-p').
 
;;(@* "Expanded-Common-Match Completion")
;;
;;  Expanded-Common-Match Completion
;;  --------------------------------
;;
;;  Apropos (regexp) matching and prefix completion each match a
;;  pattern against a completion candidate.  This matching operation
;;  concerns only a single candidate; it does not take into account
;;  the fact that there are others.  Since the matching operation is
;;  repeated anyway for each candidate, however, we can also find an
;;  expanded string that includes the same match (apropos or prefix)
;;  for all candidates.
;;
;;  For prefix completion, Emacs completes your input to the longest
;;  common prefix match.  Icicles uses a similar notion for apropos
;;  completion.
;;
;;  For example, if you enter `M-x minib' and hit `TAB', vanilla Emacs
;;  completes your input to `minibuffer', which is the longest prefix
;;  match for `minib' among all command names.  The actual string that
;;  matches prefix `minib' among all candidates is, itself, `minib'.
;;
;;  In Icicles, if you hit `S-TAB' then each matching candidate
;;  contains a substring that matches your regexp input `minib'.  In
;;  this case, that substring is `minib', just as in the vanilla,
;;  prefix-matching case.  And just as in the prefix case, each
;;  matching candidate also includes a longer substring, `minibuffer',
;;  which includes what your input matches for each candidate.
;;
;;  Icicles replaces your regexp input in the minibuffer by a common
;;  substring.  It highlights this expanded common match in buffer
;;  `*Completions*' using face
;;  `icicle-common-match-highlight-Completions' (magenta foreground,
;;  by default).  What your input matches directly is highlighted in
;;  `*Completions*' using face `icicle-match-highlight-Completions'
;;  (red foreground, by default).
;;
;;  It is of course possible that a given regexp matches different
;;  candidates differently, so that there is no common match.  In that
;;  case, only the individual matches are highlighted in
;;  `*Completions*' - you will see only red, no magenta, highlighting.
;;  For example, if your regexp input is `min.*buf' then various
;;  different substrings (such as `minibuf' from `minibuffer-complete'
;;  and `mint-truncate-buf' from `comint-truncate-buffer') are
;;  highlighted in red, but these share no common substring.
;;
;;  You will also see only red highlighting if what your input matches
;;  directly is the same as the expanded common match.  For example,
;;  if a function `moccur-regexp-read-from-minibuf' is defined (it is
;;  in library `color-moccur.el'), and your input to `C-h f' is
;;  `min[^-]*buf', then only `minibuf' is highlighted in red.
;;
;;  Expanded-common-match completion is convenient, but when
;;  apropos-completing you often need to try variants of a regexp,
;;  editing it and observing which candidates match in
;;  `*Completions*', until you get the regexp right.
;;  Expanded-common-match completion has the disadvantage that you
;;  lose your regexp as input, which makes it hard to edit it!
;;
;;  To retrieve it, use `C-l' (`icicle-retrieve-previous-input')
;;  during completion.  You can repeat `C-l' to retrieve older
;;  completion inputs, cycling among them, and you can use `C-S-l'
;;  (that is, `C-L') to cycle previous inputs in the other direction -
;;  see (@> "History Enhancements").  You can use option
;;  `icicle-expand-input-to-common-match' to turn off
;;  expanded-common-match completion altogether, if you prefer.  You
;;  can cycle the value of this option from the minibuffer at any
;;  time, using `C-M-"'.
;;
;;  Sometimes the current completion candidates can be quite long, and
;;  so can their common match part.  In this situation it can
;;  sometimes help to tell Icicles to hide the common match part.
;;  User option `icicle-hide-common-match-in-Completions-flag'
;;  controls whether this part is hidden, in the `*Completions*'
;;  display.  You can toggle this option from the minibuffer anytime
;;  using `C-x .'
;;
;;  Some Icicles multi-commands that typically display long candidates
;;  automatically turn this option on, to hide the common match
;;  portion, for clarity.  This is the case for file-finding commands
;;  that expect an absolute file name.  Just use `C-x .' to turn
;;  hiding off again.
;;
;;  You can control which commands do this, by adding/removing
;;  property `icicle-hide-common-match' to/from the command symbol.
;;  For example, command `icicle-locate-file' has the non-`nil'
;;  property by default.  If you do not want this command to hide the
;;  common match then add this to your init file (`~/.emacs'):
;;
;;    (put 'icicle-locate-file 'icicle-hide-common-match nil)
;;
;;  But just what is meant by the "expanded common match" that Icicles
;;  finds?  It is the longest match of your input pattern that is
;;  common to all candidates and also contains the first input match
;;  in the first or second candidate, whichever is longer.
;;
;;  For apropos completion, this is not always the longest common
;;  match of your input, but in most cases it is, and it is quicker to
;;  compute.  In general, the longest common match does not
;;  necessarily contain the first match of your input with either the
;;  first candidate or the second candidate.  It might contain a
;;  different input match from the first in both the first and second
;;  candidates.
;;
;;  For example, with input `a' and candidates `abacb', `abbac', and
;;  `bacba' (in that order), `bac' is the longest common match.  But
;;  `a' is the longest common match that contains the first match in
;;  the first candidate.  It is the second match of `a' against
;;  `abacb' that yields `bac'.  Likewise for the second candidate: it
;;  is the second match of `a' against `abbac' that yields `bac'.
;;
;;  So in this case Icicles will use `a' as the expanded input and
;;  miss the longest common match.  If the candidate order is
;;  different, so that `bacba' is either the first or the second
;;  candidate, then Icicles finds the longest common match, because
;;  the first match of `a' against `bacba' yields `bac'.
;;
;;  The reason that Icicles common-match expansion typically finds the
;;  longest common match is that your input typically matches the
;;  first or the second candidate in only one place.  And the longer
;;  the input you type, the more likely this is.  In practice, it is
;;  only with very short input such as `a' that Icicles expansion
;;  sometimes misses the longest common match.  Icicles independently
;;  tries two candidates (first and second) as its starting point, to
;;  increase the probability of finding the longest common match.
;;
;;  It is also technically incorrect to speak of "the" longest common
;;  match: in general, there can be more than one.  For example, if
;;  the input is `a' and the candidates are `abab', `abba', and
;;  `baba', then both `ab' and `ba' are longest common substrings.
;;  Again, however, for typical input and typical candidates there is
;;  a single longest common match, and Icicles finds it.
;;
;;  Note that Icicles expanded common match is not just a common
;;  substring among all of the candidates that are matched by your
;;  input pattern.  It is a substring common to all candidates matched
;;  by your input, but a substring that also matches your input.  For
;;  example, with apropos completion input `a.z' and candidates `abz'
;;  and `apz', there is no expanded common match.  The substring `a'
;;  is common to both candidates, but it is not matched by the
;;  (complete) input pattern.
;;
;;  (Note that in Emacs 20 no common match is found if your input or
;;  any of the candidates contains binary data.  This is because
;;  function `string-match' cannot handle strings with binary data in
;;  Emacs 20.)
 
;;(@* "Progressive Completion")
;;
;;  Progressive Completion
;;  ----------------------
;;
;;  Perhaps the best way to explain this feature is to use a familiar
;;  analogy.  Unix or GNU/Linux command `grep' takes a
;;  regular-expression argument, and matches it against lines in
;;  files.  A common idiom that people use is to chain, or cascade,
;;  multiple calls to `grep', using the output of one as the input to
;;  the next.  For example:
;;
;;    grep plant *.txt | grep food | grep mineral
;;
;;  The output of the search for "plant" is used as the input for the
;;  search for "food", and the output of that search serves as the
;;  input for the search for "mineral".  The order of the three
;;  component searches can make a difference in terms of performance,
;;  but not in terms of the result, which is always the set of lines
;;  in files *.txt that match "plant" AND "food" AND "mineral", in any
;;  order.  Each of the `grep' operations defines a set of matches,
;;  and the chain of `grep' operations effects the intersection of
;;  those sets.
;;
;;  Of course, you could try to accomplish the same thing with a
;;  single call to `grep' using a complex regexp.  But why would you?
;;
;;  Moreover, it is in fact impossible to express such an unordered
;;  set intersection using a single regexp.  On their own, regular
;;  expressions cannot express set intersection (conjunction) or
;;  complementing (negation).  (However, most `grep' programs provide
;;  a way to obtain the lines that do not match a regexp.)
;;
;;  The same idea of combining multiple regexp matches is behind the
;;  Icicles feature of progressive completion: instead of trying to
;;  come up with a single complex regexp that does what you want, try
;;  getting there a step at a time:
;;
;;   1. Match an input regexp against the set of all possible
;;      completions.
;;
;;   2. Narrow the set of matched candidates by matching them against
;;      another input regexp (or by filtering them with a predicate).
;;
;;   3. Narrow those results down by matching them against a third
;;      input regexp (or by filtering them with another predicate).
;;
;;   4... And so on.
;;
;;(@* "`M-*' and `S-SPC': Matching Additional Regexps")
;;  ** `M-*' and `S-SPC': Matching Additional Regexps **
;;
;;  During completion, `M-*' is bound in the minibuffer to command
;;  `icicle-narrow-candidates', which prompts for a new regexp and
;;  matches it against the current set of completion candidates.
;;
;;  As is often the case in Icicles, you can think of the `*' in `M-*'
;;  as mnemonic for Boolean multiplication, that is, AND, or set
;;  intersection.  (It is more common and handier to use `S-SPC' than
;;  `M-*' - see (@> "Successive Approximation...").)
;;
;;  For example, suppose that you want to know about an Emacs function
;;  that deletes the character to the left of point (that is,
;;  backward).  You do not recall if it is `delete-character-back',
;;  `delete-backward-char', `character-delete-backward', or whatever.
;;  You take a guess that the name contains `delete', `char', and
;;  `back'.
;;
;;   1. `C-h f char S-TAB' displays function names that contain
;;      `char'.
;;
;;   2. `M-* delete' narrows that set of function names to those that
;;      also contain `delete'.
;;
;;   3. `M-* back' narrows the set of matching names further, to those
;;      that also contain `back'.
;;
;;  This displays a list of functions like this in `*Completions*'
;;  (your list might be somewhat different):
;;
;;    backward-delete-char        backward-delete-char-untabify
;;    delete-backward-char        icicle-backward-delete-char-untabify
;;    icicle-delete-backward-char
;;    quail-conversion-backward-delete-char
;;
;;  Then, of course, you can pick one (or you can use `C-M-next'
;;  repeatedly to view the doc of each of these functions in turn -
;;  see (@> "Get Help on Completion Candidates")).
;;
;;  You get the idea.  This feature is both very simple to use and
;;  very useful.  It's easy to appreciate using multiple simple
;;  matching steps (regexp or not) instead of a single regexp.  Try it
;;  once, and you'll be hooked.
;;
;;(@* "Successive Approximation...")
;;  ** Successive Approximation... **
;;
;;  You can use as many invocations of `M-*' (and of `M-&', described
;;  in the next section) as you like, in any order.  It works with
;;  both prefix completion and apropos completion.
;;
;;  You can, for instance, first use `TAB' to require the target to
;;  start with some string, and then use `M-*' to specify other
;;  patterns that parts of it must also match.  However, it of course
;;  makes no sense to use `TAB' instead of `S-TAB' after you use
;;  `M-*': once you've said that the target must start with "fo" there
;;  is no sense saying that it also starts with "ti"!
;;
;;  As a shortcut, instead of using `S-TAB' followed by `M-*', you can
;;  use `S-SPC' (command `icicle-apropos-complete-and-narrow') to do
;;  the same thing.  You can thus use only `S-SPC', any number of
;;  times, to choose a candidate by narrowing down the matches.
;;
;;  Note that if the current Icicles completion method is `basic' (see
;;  (@file :file-name "icicles-doc2.el" :to "Completion Methods and Styles"))
;;  and you initiate completion using prefix completion (`TAB'), then
;;  Icicles implicitly prefixes your minibuffer input with "^" if you
;;  then use progressive completion (e.g. `S-SPC').  No such
;;  convenience is possible if the completion method is not `basic',
;;  because completion does not necessarily match your input as a
;;  prefix.  In this case, if you want to treat your input as a prefix
;;  then you need to prefix it with "^" explicitly.
;;  
;;  With a prefix argument, `S-SPC' uses predicate narrowing, that is,
;;  `C-u S-SPC' is the same as `S-TAB' followed by `M-&' (described in
;;  the next section) .
;;
;;  I call this process of completion by successive approximation, or
;;  progressively narrowing the candidate set, "progressive
;;  completion".  If the name "incremental completion" (= icompletion)
;;  were not already taken to mean incremental completion *help*
;;  (which performs no completion), then that might be a good name for
;;  this.  This might also be called "stepped", "cascaded", or
;;  "piecewise" completion.
;;
;;  Another possible name for it would be "multiple completion", but I
;;  use that to stand for simultaneous (parallel) completion of
;;  multiple parts of a compound target, which is something different
;;  (see (@> "Multi-Completions")).
;;  Progressive completion is a set of mini-completions that are wired
;;  in series, not in parallel.
;;
;;  Note that when you use candidate narrowing (`M-*', `M-&', or
;;  `S-SPC') in the minibuffer, `completing-read' or `read-file-name'
;;  is invoked, which creates a recursive minibuffer.  That is, the
;;  minibuffer depth is increased.
;;
;;  In vanilla Emacs, there is no indicator of the current minibuffer
;;  depth, and this can sometimes be disorienting.  Each time you
;;  narrow the set of current candidates, you push down one level of
;;  minibuffer recursion (that is, minibuffer depth is incremented).
;;  Each time you use, say, `C-g', you pop up one level of minibuffer
;;  recursion (that is, minibuffer depth is decremented).
;;
;;  If you use library `mb-depth.el', which is included with Emacs 23
;;  and which also works with Emacs 22, Icicle mode takes advantage of
;;  this library by indicating the current depth in the minibuffer.  I
;;  recommend you also use my library `mb-depth+.el', which lets you
;;  customize the form and face of the depth indicator.
;;
;;  If you use my library `oneonone.el', then you get visual feedback
;;  on the current minibuffer depth.  One-On-One Emacs gives you a
;;  standalone minibuffer frame, and it changes the background hue
;;  (color) of that frame slightly with each change in minibuffer
;;  depth.  This is especially helpful with Icicles, where use of
;;  candidate narrowing (progressive completion) is common.
;;
;;  There is a slight difference in behavior between Icicles commands
;;  and some other Emacs commands when you accept input after
;;  narrowing.  When possible, Icicles accepts your input and passes
;;  it immediately to the top level, bypassing any intermediate
;;  recursive minibuffer levels that are waiting for input.  However,
;;  Emacs commands that are defined with literal-string `interactive'
;;  specs, such as (interactive "fFile: "), do not use
;;  `completing-read' or `read-file-name', so there is no way for
;;  Icicles to take this shortcut with them.  In that case, you will
;;  simply need to hit `RET' again to accept your input at each
;;  recursive minibuffer level, until you get back to the top level.
;;  Sorry for this inconvenience!  If you are an Emacs-Lisp
;;  programmer, note that this is one reason to use `completing-read'
;;  and `read-file-name' when you write commands that use completion.
;;
;;  Note: If you use progressive completion with file names in Emacs
;;  20 or 21 then candidate narrowing invokes `completing-read', not
;;  `read-file-name'.  This is because `read-file-name' does not
;;  accept a PREDICATE argument before Emacs 22.  The effect is that
;;  instead of there being a default directory for completion, the
;;  current directory at the time you narrow is tacked onto each file
;;  name, to become part of the completion candidates themselves.
;;  Yes, this is a hack.  It works, but be aware of the behavior.
;;
;;  Progressive completion lets you match multiple regexps, some of
;;  which could of course be literal substrings, with their regexp
;;  special characters, if any, escaped.  If you need to match such
;;  substrings at particular locations in the target completion
;;  candidate, then progressive completion will not do the job - it
;;  matches its component regexps independently.  You can regexp-quote
;;  (escape) parts or all of your input using `M-%'
;;  (`icicle-regexp-quote-input').
;;  See (@> "Quoting (Escaping) Special Characters")
;;
;;(@* "`M-&': Satisfying Additional Predicates")
;;  ** `M-&': Satisfying Additional Predicates **
;;
;;  If you use Icicles, then you will use candidate narrowing
;;  (progressive completion) very often.  This section describes `M-&'
;;  (`icicle-narrow-candidates-with-predicate'), which is like `M-*'
;;  (`icicle-narrow-candidates') except that it also restricts
;;  candidates by using a predicate for filtering.
;;
;;  (If you are new to Icicles or you are unfamiliar with Emacs Lisp,
;;  then you might want to just skim this section for now or skip it
;;  and come back to it later.)
;;
;;  Just as you can use `M-*' or `S-SPC' to narrow the set of
;;  candidates by matching an additional regexp, so you can use `M-&'
;;  or `C-u S-SPC' to narrow by satisfying an additional predicate.  The
;;  idea is the same; the only difference is that you are prompted for
;;  a predicate for the current candidates to satisfy.
;;
;;  This must be a Boolean function of a single completion candidate.
;;  At the prompt, you enter its name or its lambda-expression
;;  definition (anonymous function).
;;
;;  Completion is available for some existing predicate names
;;  appropriate for the current command.  For example, if you use `C-x
;;  4 f TAB M-&' then you can complete against the file-name
;;  predicates named in option `icicle-cand-preds-for-file'.  This
;;  lets you quickly filter by file type: directories, executables,
;;  compressed files, remote files, desktop files, and so on.
;;
;;  If you use a prefix arg with `M-&' then additional predicate
;;  completion candidates are available (they might or might not be
;;  appropriate for the current command).
;;
;;  The predicate you choose is used the same way as the PREDICATE
;;  argument to `completing-read' and `read-file-name'.  This means
;;  that the candidate argument to the predicate is a full completion
;;  candidate; it is not just a string such as you see in buffer
;;  `*Completions*'.
;;
;;  The type of full completion candidate expected by the predicate
;;  is, to start with, whatever is used in the original call to
;;  `completing-read' or `read-file-name'.  To provide an appropriate
;;  predicate, you must be familiar with the kind of candidate
;;  expected by the command you invoked before just before `M-&'.
;;
;;  For example:
;;
;;  * Command `describe-function' (`C-h f') uses candidates that are
;;    symbols.  An appropriate predicate would accept a symbol as
;;    argument.
;;
;;  * Command `icicle-search' (`C-c `') uses candidates that have this
;;    form: (CONTEXT . MARKER), where CONTEXT is a string, the search
;;    hit (search context), and MARKER is a buffer marker that locates
;;    the CONTEXT.  An appropriate predicate would accept such a
;;    candidate as argument.
;;
;;  The type of candidate expected by the current command might be a
;;  symbol, a string, or a cons with a string car.  It might even be
;;  an Icicles multi-completion, which in its full form is a cons with
;;  a list of strings as its car.
;;
;;  Knowing what kind of completions the original `completing-read' or
;;  `read-file-name' call expects is not sufficient, however.  You
;;  might use `M-&' after otherwise narrowing the set of candidates,
;;  and narrowing changes the full candidates to be conses whose car
;;  is a string.
;;
;;  For example, command `describe-variable' reads a variable name,
;;  using completion with Lisp symbols as its full candidates.  But if
;;  you narrow your input matches (e.g. using `S-SPC'), then the full
;;  candidates are no longer symbols; they are conses with symbol
;;  names (strings) as their cars.
;;
;;  So if you define your own predicate for use with a command such as
;;  `describe-variable' then it will need to work with either a symbol
;;  or a cons that has a symbol-name (string) as its car.
;;
;;  If you want to adapt an existing predicate that expects a
;;  `*Completions*' display candidate (a string) then you can use
;;  function `icicle-display-cand-from-full-cand' in your predicate
;;  definition.  If multi-completion is involved then you can use
;;  function `icicle-transform-multi-completion'.
;;
;;  In sum: if you want to adapt an existing predicate that expects an
;;  argument that is not a cons with a string car, then convert the
;;  car to what you need.  See the definition of function
;;  `icicle-custom-variable-p' for an example.
;;
;;  User option `icicle-cand-preds-all' defines the predefined
;;  candidate-filtering predicates, and these are grouped in user
;;  options named `icicle-cand-preds-for-TYPE', where TYPE is the name
;;  of a completion type (`bookmark', `buffer', `color', `face',
;;  `file', `package', `variable', `window',...).  You can add a named
;;  predicate to one of these options.
;;
;;  (Note: Some commands provide minibuffer key bindings that also
;;  narrow the set of candidates.  For example, during bookmark-name
;;  completion, there are keys to narrow to different types of
;;  bookmark.
;;  See (@file :file-name "icicles-doc2.el" :to "Minibuffer Bindings").)
;;
;;  For example, you can customize option
;;  `icicle-cand-preds-for-buffer', to add a buffer-name predicate
;;  that you can then enter using completion.  (You will also want to
;;  add it to option `icicle-cand-preds-all'.)
;;
;;  One of the completion-type options is
;;  `icicle-cand-preds-for-misc'.  This is a catch-all category of
;;  predicates that apply generally, to pretty much all completion
;;  types.  These predicates are included as candidates whenever you
;;  use `M-&'.
;;
;;  An important predicate in this catch-all group is
;;  `icicle-special-candidate-p' (and its opposite,
;;  `icicle-not-special-candidate-p').  This filter keeps only
;;  candidates that are (or are not) "special candidates".  These are
;;  candidates that are highlighted in `*Completions*' using face
;;  `icicle-special-candidate' (actually, other faces can also be
;;  used, in which case the doc of the particular command explains
;;  this).
;;
;;  For example, command `icicle-apropos' shows function names as
;;  special candidates, to help you distinguish them from variable
;;  names.  And Icicles key completion distiguishes local key bindings
;;  by showing them as special candidates.  And during file-name
;;  completion directory names are shown as special candidates.
;;
;;  Typing a lambda expression to define a predicate on the fly is
;;  handy and flexible, but it is of course much less convenient than
;;  choosing a predefined predicate by name.  (Remember though, that
;;  you can retrieve previously entered predicates, using `M-p' and so
;;  on.)
;;
;;  You can also use `C-M-&' (bound to
;;  `icicle-save-predicate-to-variable') at any time during completion
;;  to save the current predicate as a string-valued variable.  By
;;  default, the variable is `icicle-input-string'.  You can then
;;  retrieve the saved string later, using `C-=' at the prompt for
;;  `M-&'.  The current predicate is what is saved.  You can build up
;;  a complex predicate, and then save it for later use.
;;
;;  The inconvenience of typing an Emacs-Lisp sexp must be balanced
;;  against the power of applying predicates on the fly.  Whereas
;;  regexp matching is purely syntactic, with a predicate you can
;;  perform semantic tests.  During search, for instance, you can look
;;  not only for a syntax match; you can look for matching search
;;  candidates that also belong to a particular class of objects
;;  (e.g. function, variable, type) or that satisfy some other
;;  semantic property.
;;  See also (@file :file-name "icicles-doc2.el" :to "Icicles Search Commands, Overview").
;;
;;  See Also:
;;
;;  * (@> "Sets of Completion Candidates") for another way to perform
;;    a set intersection on sets of candidate completions.
;;
;;  * (@> "Recompleting Using the Original Domain") for how to cancel
;;    narrowing and recomplete your input using the original set of
;;    candidates.
;;
;;  * (@file :file-name "icicles-doc2.el" :to "Icicles Search Commands, Overview")
;;    for a way to search using two regexps - command `icicle-search'
;;    uses the same idea as that behind progressive completion.
;;
;;  * (@file :file-name "icicles-doc2.el" :to "Compile/Grep Search")
;;    for a way to grep files using multiple levels of regexps, and
;;    performing selected replacements.
 
;;(@* "Regressive Completion")
;;
;;  Regressive Completion
;;  ---------------------
;;
;;  Though generally less useful than progressive completion, you can
;;  also widen, instead of narrow, the current set of completion
;;  candidates, by providing an alternative pattern (regexp) to match.
;;  By analogy, I call this "regressive completion".
;;
;;  The analogy is not exact.  By definition, your current input is
;;  always matched against all candidates in the domain of discourse.
;;  With progressive completion, a recursive minibuffer is entered for
;;  each new pattern to match.  The candidates that matched the
;;  previous input of the progression become the new domain of
;;  discourse for the current act (recursive level) of completion.
;;
;;  That same technique is not applicable for widening.  Instead, you
;;  enter, using `RET', a new pattern to match as an alternative, and
;;  Icicles changes the current input to a regexp that matches either
;;  what the previous input matched or the alternative pattern.  In
;;  other words, it is just a short cut for typing a regexp that
;;  matches a choice: \(...\|...\).  The domain of discourse remains
;;  the same - in particular, there is no way to widen the domain of
;;  discourse like narrowing narrows it.
;;
;;  You use `M-+' (`icicle-widen-candidates') for regressive
;;  completion - think of the `+' as set union (OR), just as you think
;;  of the `*' in `M-*' as set intersection (AND).  And, just as for
;;  progressive completion, there is a shortcut, `S-backspace', for
;;  `S-TAB' followed by `M-+'.
;;
;;  For example, if you want to see all of the candidates that contain
;;  either `for' or `back', you could type `\(for\|back\)' in the
;;  minibuffer, or you could just type `for', then `S-backspace' (or
;;  `S-TAB' followed by `M-+'), then `back'.  Icicles replaces your
;;  input by `\(for\|back\)'.  You can continue with additional
;;  alternative match patterns.  And you can combine narrowing with
;;  widening, that is, progressive with regressive completion.
;;
;;  You can of course cycle among all matching candidates, regardless
;;  of which alternative they match.  One use of regressive completion
;;  is with Icicles search - it corresponds to the OR searching of
;;  common search engines.
;;
;;  Gotcha: When completing file names that are not absolute
;;  (e.g. using `C-x C-f', not `C-u C-x C-f'), be sure that the
;;  default directory is not included in your minibuffer input when
;;  you hit `M-+'.  You do not want the overall regexp that `M-+'
;;  constructs to be something like \(/my/default/dir/foo\|bar\) - you
;;  want it to be just \(foo\|bar\).  For absolute file name
;;  completion there is no such problem, because the completion
;;  candidates themselves have a directory component.  So either use a
;;  `nil' value of `insert-default-directory' or use `M-k' to remove
;;  the directory component before hitting `M-+'.
;;
;;  See Also: (@> "Recompleting Using the Original Domain") for how to
;;  widen back to the original set of candidates.
 
;;(@* "Completion On Demand")
;;
;;  Completion On Demand
;;  --------------------
;;
;;  When the minibuffer is active for your input, completion is not
;;  always available.  Functions such as `completing-read' and
;;  `read-file-name' provide completion, but other functions that read
;;  input in the minibuffer, such as `read-from-minibuffer' and
;;  `read-string', do not provide completion.
;;  (See (@> "Completion Status Indicators"), for how to tell when
;;  completion is available in Icicles.)
;;
;;  But in Icicles you can always invoke (lax) completion to insert
;;  some completed text in the minibuffer - this is completion on
;;  demand.
;;
;;  On-demand completion is always available to insert a file name.
;;  You invoke this using `C-M-F', that is, `C-M-S-f'
;;  (`icicle-read+insert-file-name').  A recursive minibuffer is used
;;  to perform the completion.  The result of completing is inserted
;;  at point in the parent minibuffer, without replacing any other
;;  text that might already be there.
;;
;;  You can use this feature to add multiple file or directory names
;;  to the same minibuffer input.  In this way, for instance, you can
;;  use it to add particular file or directory names as arguments to a
;;  shell command that you input in the minibuffer.  By default, a
;;  relative name is inserted, but if you use a prefix argument then
;;  the directory component is included.
;;
;;  Similarly, you can use `C-M-C', that is, `C-M-S-c'
;;  (`icicle-completing-read+insert'), to invoke non file-name
;;  completion.  This, however, is available only if the command
;;  reading from the minibuffer allows it, by defining a set of
;;  possible completions.
;;
;;  The actual keys used for on-demand completion are customizable,
;;  using options `icicle-read+insert-file-name-keys' and
;;  `icicle-completing-read+insert-keys'.  The default values are
;;  `C-M-S-f' and `C-M-S-c'.
;;
;;  Another kind of on-demand completion is provided by minibuffer
;;  multi-command `icicle-insert-history-element' (`M-o').  This is
;;  always available in the minibuffer, regardless of whether input is
;;  being read with completion.  It lets you use completion to insert
;;  any number of entries from the current minibuffer history into the
;;  minibuffer at point.  See (@> "History Enhancements").
 
;;(@* "Moving Between the Minibuffer and Other Buffers")
;;
;;  Moving Between the Minibuffer and Other Buffers
;;  -----------------------------------------------
;;
;;  Sometimes, when the minibuffer is active, you might want to move
;;  the cursor and focus from the minibuffer back to the original
;;  buffer from which you activated the minibuffer.  When you are in
;;  Icicle mode, the `pause' key is bound (by default) to command
;;  `icicle-switch-to/from-minibuffer', which does that.  This lets
;;  you start minibuffer input (with or without completion), and then
;;  interrupt it to search, edit, and so on, in the original buffer.
;;  This same command (bound to `pause') then lets you switch back to
;;  the minibuffer - it acts as a toggle for the input focus; go back
;;  and forth as much as you like.
;;
;;  This can be especially useful when you use multi-commands (see
;;  (@> "Multi-Commands")).  In that case, you often keep the
;;  minibuffer active for completion while performing multiple
;;  completion actions.  It can be handy to interrupt this to perform
;;  some normal editing or search, and then resume multi-command
;;  actions.
;;
;;  For example, when using `g' (`icicle-Info-goto-node') in Info, if
;;  you use `C-RET' to show a matching candidate node, you can `pause'
;;  to visit it, search it using `C-s', and then `pause' back to the
;;  minibuffer to continue browsing other node candidates.
;;
;;  Another use for this feature is to select text in the original
;;  buffer and then insert it in the minibuffer.  See also
;;  (@> "Inserting Text Found Near the Cursor") for another way to do
;;  that.
;;
;;  A somewhat related toggle is available using `C-insert'.  This
;;  lets you switch the focus between the minibuffer and buffer
;;  `*Completions*'.  See (@> "*Completions* Display") for more
;;  information.
 
;;(@* "Inserting a Regexp from a Variable or Register")
;;
;;  Inserting a Regexp from a Variable or Register
;;  ----------------------------------------------
;;
;;  Regexps are powerful, but they can sometimes be complex to compose
;;  and hard to remember once composed.  A shortcut is to compose a
;;  regexp that you want to use and assign it to an Emacs variable or
;;  register.
;;
;;  If you assign it to a register (using `C-x r s'), then you can use
;;  `C-x r i' (`insert-register') in the minibuffer to insert it.  If
;;  you assign it to a string, then you can use `C-='
;;  (`icicle-insert-string-from-variable') to insert it.
;;
;;  If you use `C-u C-=' (provide a prefix argument) then you are
;;  prompted for the variable to use.  Completion candidates for this
;;  include all string-valued variables.
;;
;;  Without `C-u', the default variable is used (no prompting),
;;  `icicle-input-string'.  So for example, if `icicle-input-string'
;;  had value "[a-zA-Z]+" then it would match any completion candidate
;;  composed only of letters.  You can customize
;;  `icicle-input-string'.
;;
;;  For convenience, instead of using Lisp evaluation of a sexp such
;;  as (setq icicle-input-string "[a-zA-Z]+") or (setq my-var ".*"),
;;  you can use Icicles command `icicle-save-string-to-variable' to
;;  save a regexp to a variable.  You are prompted for the regexp to
;;  save.  Just as for `icicle-insert-string-from-variable', with a
;;  prefix argument you are prompted for the variable to use (all
;;  variables are completion candidates).  With no prefix argument the
;;  regexp is saved to variable `icicle-input-string'.
;;
;;  Another way of inserting a string into the minibuffer is to use a
;;  negative prefix argument with `M-:' (e.g. `M-- M-:') during
;;  minibuffer input.  With this method, you can type not only a
;;  string-valued variable name but any Emacs-Lisp expression.  The
;;  expression need not evaluate to a string - whatever the result of
;;  evaluation is, it is pretty-printed in the minibuffer, to be used
;;  as part of your input text.
;;
;;  These shortcut features are especially convenient for use with
;;  command `icicle-search' - you can use it to search text for
;;  sentences, paragraphs, file names, URLs, dates, times, function
;;  definitions, and any other text entities that you can specify by
;;  regexp.  Create a library of regexp-valued variables that are
;;  useful to you, and use `C-=' to quickly access them in
;;  `icicle-search'.
;;  See (@file :file-name "icicles-doc2.el" :to "Icicles Search Commands, Overview")
;;  for more information.
;;
;;  These shortcuts are also handy for Isearch, in particular, regexp
;;  search.  Use `M-e' after `C-M-s', to edit the search string (which
;;  puts you in a minibuffer), then use `C-u C-=' or `C-x r i' to
;;  insert a saved regexp.
;;
;;  See Also:
;;
;;  * (@> "Inserting Text Found Near the Cursor")
;;  * (@> "Multi-Inputs") for ways to act on multiple minibuffer
;;    insertions all at once.
 
;;(@* "Special Characters in Input Patterns")
;;
;;  Special Characters in Input Patterns
;;  ------------------------------------
;;
;;  Regular-expression syntax treats some characters specially, but
;;  some of these special characters have another special meaning in
;;  Emacs when used with file-name inputs.  What about the conflict
;;  between interpreting characters such as `$', `\', `.', `?', and
;;  `*' as 1) regexp special characters and 2) special characters for
;;  file-name input?  For example, when inputting a file name, should
;;  `*' be treated as a regexp multiple-occurrences operator or as a
;;  file-name wildcard?
;;
;;  In Emacs file-name input:
;;
;;  - `$' can be used to prefix environment variables.
;;
;;  - `*' and `?' can be used as wildcards, effectively inputting
;;    multiple file names at once.
;;
;;  - `.' and `..' can be used to navigate a directory hierarchy.
;;
;;  - `\' is a directory separator, like `/', on MS Windows, at least.
;;
;;  Icicles handles the conflict by interpreting such characters as
;;  regexp special characters only during input completion and cycling
;;  - and then only if you do not escape them (with `\').  If present
;;  in the input when you finally accept it (using `RET'), they take
;;  on their normal Emacs meanings for file-name input:
;;  environment-variable prefix, wildcard, directory abbreviation, or
;;  directory separator.
;;
;;  That is, whenever there is a potential conflict of interpretation,
;;  the regexp meaning is used for completion and cycling, and the
;;  standard interpretation for file-name input is used for accepting
;;  the input.  So, for example, to get the wildcard interpretation of
;;  `*', just forego regexp completion and cycling.  And vice versa:
;;  forego the wildcard interpretation to use regexp completion and
;;  cycling.  File-name globbing and completion are independent.
;;
;;  Note: Because `?' is useful in regexp syntax, the standard Emacs
;;        minibuffer binding of `?', which just displays the
;;        completion-candidates list, is not used in Icicles.  In
;;        Icicles, `?' self-inserts in the minibuffer, like any other
;;        printable character.  (Use `TAB' or `S-TAB' to display the
;;        list.)  In standard Emacs, you must quote `?' or
;;        copy-and-paste it, to insert it in the minibuffer for use as
;;        a file-name wildcard.
;;
;;  The interpretation conflict for `\' (on MS Windows) is not really
;;  a problem, anyway.  Although you cannot use a backslash (`\') as a
;;  directory separator during apropos completion and cycling, you can
;;  always use a slash (`/') instead - even on MS Windows.  The best
;;  practice is to just break with MS-Windows syntax, and get in the
;;  habit of always using `/' as the directory-separator character.
;;
;;  But what if you copy an absolute filename from some MS Windows
;;  application, so it has backslashes, and you want to use it in
;;  Emacs?  You can go ahead and paste it in the minibuffer for
;;  filename completion, as long as you are not doing regexp
;;  completion.  You can (a) use prefix completion with it, (b) use
;;  `C-`' to turn on regexp quoting for apropos completion (so you can
;;  complete a substring), or (c) change the backslashes to slashes.
;;
;;  Even if you always use only slash, not backslash, as a directory
;;  separator when inputting, however, it is possible that you could
;;  run into some trouble on MS Windows.  You might (knowingly or not)
;;  use `\' as a directory separator in the values of environment
;;  variables that you use as part of file-name input.  If you are
;;  regexp completing then those backslashes will be treated as regexp
;;  escapes.
;;
;;  So you should use only non-regexp completion with input that
;;  includes environment variables whose expansions might include
;;  backslashes.
;;
;;  The interpretation conflict for `$' is also not a real problem.
;;  You can get the effect of both interpretations of `$' at the same
;;  time, because Icicles recognizes that `$' at the end of input
;;  cannot be an environment-variable prefix.  This means, for
;;  example, that you can use a pattern such as `$HOME.*t$' to match
;;  the files in your home directory whose names end in `t'.
;;
;;  The first `$' here is not treated specially during regexp matching
;;  and cycling; the environment variable `$HOME' is expanded by the
;;  shell to a directory name.  The second `$' is treated as the
;;  regexp special character that matches at the end of a line.  When
;;  using environment variables, you can also enclose them in braces:
;;  `${HOME}', for example.
;;
;;  Note: Starting with Emacs 23, if option
;;  `icicle-TAB-completion-methods' includes `vanilla', and if you
;;  choose `vanilla' completion for `TAB' (by cycling using `C-(' or
;;  by customizing `icicle-TAB-completion-methods' to use `vanilla' as
;;  the default), then Icicles `TAB' completion will complete an
;;  environment variable during file-name completion.  This is in
;;  addition to the traditional shell expansion of a variable when you
;;  hit `RET'.
;;
;;  Tip: Because slash (`/') is about the only non-word syntax
;;       character that is likely to appear in file-name completions,
;;       you can usually use `\W$' to match only directories (by
;;       matching the `/' at the end of their names).  `\W' is the
;;       regexp pattern that matches any character that does not
;;       appear in words.  For example, you can use `${HOME}\W$' to
;;       match all direct subdirectories in your home directory.
;;
;;(@* "Quoting (Escaping) Special Characters")
;;  ** Quoting (Escaping) Special Characters **
;;
;;  You can toggle interpretation vs escaping of regexp special
;;  characters at any time, using `C-`' in the minibuffer (command
;;  `icicle-toggle-regexp-quote').  Escaping special characters this
;;  way means they are no longer special; they simply match
;;  themselves.  This has the effect of reducing apropos completion to
;;  simple substring completion.  If you never want to use regexp
;;  matching (*not* recommended!), you can customize user option
;;  `icicle-regexp-quote-flag', setting it to non-`nil'.
;;
;;  Apropos (regexp) completion contains literal substring completion
;;  as a (common) special case.  Sometimes you want to use regexp
;;  completion, but you also want to match a literal substring that
;;  contains special characters.  You can of course quote (escape)
;;  each of these characters by hand.  Alternatively, you can use
;;  `M-%' (`icicle-regexp-quote-input') to quote the text that you
;;  want to match literally.  If the region is active, then it is
;;  quoted; otherwise, your entire minibuffer input is quoted.
;;
;;  Note that if a substring that you want to match literally can
;;  occur anywhere in the target completion candidate, then it is
;;  simpler to just use progressive completion.  Quoting a literal
;;  substring is useful when the overall regexp requires it to be at a
;;  certain location in the target.
;;
;;  See Also:
;;
;;  * (@file :file-name "icicles-doc2.el" :to "Icicles Dired Enhancements")
;;    for how to use Icicles regexp-matching to open Dired on sets of
;;    files that you might not be able to specify using file-name
;;    wildcards.
;;
;;  * (@> "Multi-Commands") for a way to open multiple files whose
;;    names match a regular expression.
;;
;;  * (@file :file-name "icicles-doc2.el" :to "File-Name and Directory-Name Completion Tips")
;;    for:
;;    - Information about abbreviating your home directory as `~' or
;;      expanding it.
;;    - A way to locate and open files by regexp anywhere in your file
;;      system - that is, match against directory-name as well as
;;      file-name components.
;;
;;  * (@> "Progressive Completion")
 
;;(@* "Exiting the Minibuffer Without Confirmation")
;;
;;  Exiting the Minibuffer Without Confirmation
;;  -------------------------------------------
;;
;;  Normally, if you exit the minibuffer with input that only
;;  partially matches a completion candidate, the value you input is
;;  exactly what you typed.  That is, exiting does not automatically
;;  complete your input - what you type is what you get.  This is
;;  "lax" (or "permissive") completion, and it is desirable most of
;;  the time, because it lets you input a value that does not
;;  correspond to any of the completion candidates.  This is how, for
;;  instance, you can use `C-x C-f' to open a new file or `C-x b' to
;;  create a new buffer.
;;
;;  However, some people prefer "strict" completion: limiting input to
;;  the available completion candidates.  This can be handy in the
;;  case of switching to a buffer, for instance.  If you have a buffer
;;  named `new-ideas.txt', you might like to be able to type only
;;  `new' followed by `RET', and not have to first complete the input
;;  text.  This is the behavior of libraries `ido.el' and
;;  `iswitchb.el'.
;;
;;  It is the command you use that decides whether `RET' first
;;  completes your input before exiting the minibuffer (strict
;;  completion) or not (lax completion).  This is done in the command
;;  definition by providing a non-`nil' or `nil' REQUIRE-MATCH
;;  argument to function `completing-read', which prompts you and
;;  reads your input, possibly completing it.
;;
;;  If you use standard Emacs command `switch-to-buffer' then
;;  completion is lax: `RET' does not complete your input `new' to
;;  `new-ideas.txt'; it simply accepts your input as is, and creates a
;;  new buffer with that name, `new'.
;;
;;(@* "Using `S-RET' to Accept a Partial Match")
;;  ** Using `S-RET' to Accept a Partial Match **
;;
;;  By default, Icicles command `icicle-buffer', not vanilla command
;;  `switch-to-buffer', is bound to `C-x b' in Icicle mode.  (You can
;;  customize option `icicle-top-level-key-bindings' to prevent this
;;  rebinding of `C-x b' in Icicle mode.)
;;
;;  The default behavior of `icicle-buffer' is the same as the
;;  behavior of `switch-to-buffer' with respect to `RET'.  However,
;;  you can obtain the complete-and-exit `RET' behavior with
;;  `icicle-buffer' by setting option
;;  `icicle-buffer-require-match-flag' to `partial-match-ok'.  This
;;  value overrides the REQUIRE-MATCH argument to `completing-read',
;;  in effect forcing it to `t'.
;;
;;  Whenever completion is strict, requiring a match against one of
;;  the completion candidates (typically, an existing file or buffer
;;  name), you can complete and exit the minibuffer all at once, with
;;  only partial input in the minibuffer, by using `RET'.
;;
;;  But what about apropos completion?  And what about non-strict
;;  (lax) completion, whether prefix or apropos?  You can use `S-RET'
;;  (`S-return'), bound to command `icicle-apropos-complete-and-exit',
;;  to force completion and acceptance of the completed input.  It
;;  acts similarly to what `RET' does for strict prefix completion.
;;  It works for both strict and lax completion.
;;
;;  `S-RET' first completes your input according to the current
;;  completion mode (`icicle-default-cycling-mode' or the last
;;  completion command used - prefix or apropos).  If there is only
;;  one completion then it completes your input to that match and then
;;  exits.
;;
;;  For example, if you use `C-x C-f' with input `abc' and you hit
;;  `S-RET', then it is either prefix-completed or apropos-completed,
;;  depending on the current completion mode.  If the only match is,
;;  say, `abcdefg.el', for prefix completion or, say,
;;  `123abcde456.txt' for apropos completion, then that complete file
;;  name is entered.
;;
;;(@* "Accepting Partial Matches by Default")
;;  ** Accepting Partial Matches by Default **
;;
;;  For those people who prefer that a partial match always be
;;  accepted immediately, regardless of the context (whether a match
;;  is required or not) and without having to use `RET' or `S-RET',
;;  there is Icicles user option
;;  `icicle-top-level-when-sole-completion-flag'.  If you set this to
;;  non-`nil', then, whenever your input matches only one candidate
;;  completion, that candidate is used immediately.  I do not
;;  recommend this practice generally, but some people might prefer
;;  it.
;;
;;  Option `icicle-top-level-when-sole-completion-delay' is the number
;;  of seconds Icicles waits, before returning to top level with the
;;  sole completion.  It has no effect if
;;  `icicle-top-level-when-sole-completion-flag' is `nil'.  The delay
;;  gives you a chance to forestall acceptance of the sole completion:
;;  editing the completion (typing or deleting a character) before the
;;  delay expires prevents its automatic acceptance.
;;
;;  See Also: (@* "Ido and IswitchB")
 
;;(@* "Ido and IswitchB")
;;
;;  Ido and IswitchB
;;  ----------------
;;
;;  Libraries Ido and IswitchB are alternatives to Icicles that also
;;  enhance minibuffer completion in various ways.  Their UIs are
;;  similar to each other - Ido essentially extends IswitchB's
;;  buffer-name completion to file names as well.  Neither completes
;;  other kinds of candidates.  They work only for buffer names or
;;  file names, but you can advise the standard completion functions
;;  to get them to use Ido completion more generally.
;;
;;  The behavior of Ido and IswitchB is different from the default
;;  Icicles behavior.  If you prefer their behavior for buffers then
;;  you can just use IswitchB and Icicles together.  You cannot use
;;  Icicles and Ido together, however - they use the minibuffer in
;;  incompatible ways.
;;
;;  The default behavior of Icicles is different, but you can make
;;  Icicles behave more like Ido if you like.  It would be a mistake
;;  to look for a complete reproduction of the Ido behavior in
;;  Icicles, however.  If you want exactly the same behavior as Ido,
;;  then use Ido. ;-)
;;
;;  The Icicles UI is different by design.  Some of this difference in
;;  approach has to do with the fact that Ido is specialized to
;;  buffer- and file-name completion.  The generality of Icicles makes
;;  a different approach appropriate.  Icicles has many additional
;;  features that are not available in other libraries, but its main
;;  advantage is its generality: you use the same user interface for
;;  input of any kind.  As you learn more about Icicles you may begin
;;  to appreciate its approach, even if you are a diehard Ido addict.
;;
;;  This section summarizes some differences between Icicles and Ido
;;  and tells you how you can get more Ido-like behavior in Icicles if
;;  that's what you prefer.  It does not cover Icicles features that
;;  have no counterpart in Ido or features that they have in common
;;  (except to emphasize some differences).
;;
;;  If you have the Ido habit but want to give Icicles a try, then
;;  this section is for you.  I recommend, however, that you give the
;;  default Icicles behavior a good try before convincing yourself
;;  that you still prefer a more Ido-like approach.
;;
;;  See also the references at the section end for other sections that
;;  go into more detail about some of the things mentioned here.
;;  
;;  1. Incremental completion.  By default, Icicles does not turn on
;;     incremental completion until you have hit `TAB' or `S-TAB' to
;;     display the matching candidates.  Ido turns it on immediately.
;;     You can get that behavior by setting option
;;     `icicle-show-Completions-initially-flag' to `t'.
;;
;;     You can get an intermediate behavior in this regard by instead
;;     setting option `icicle-incremental-completion' to a value that
;;     is neither `nil' nor `t'.  That makes Icicles show the matching
;;     candidates as soon as you start typing input, but not before
;;     that.  See also (@> "Icompletion") and option
;;     `icicle-incremental-completion-delay'.
;;
;;  2. Non-expansion of minibuffer input.  By default, Icicles expands
;;     your minibuffer input when you complete.  This is like vanilla
;;     Emacs (for prefix completion), but it is unlike Ido, which only
;;     shows you the available candidates but does not change your
;;     input text.  You can get this non-expansion behavior in Icicles
;;     by setting option `icicle-expand-input-to-common-match' to 0.
;;     See (@> "Incremental Completion (Input Expansion) in the Minibuffer").
;;
;;  3. Matching.  By default, Ido uses substring matching for
;;     completion.  You can hit a key to switch to prefix matching,
;;     "flex" matching, or regexp matching.  Icicles gives you these
;;     same matching possibilities, and more.  (What Ido calls "flex"
;;     matching Icicles calls "scatter" matching.)  The main
;;     difference here is that Icicles regexp support is general and
;;     complete.  Regexp-matching in Ido does not work with Ido-style
;;     completion.
;;
;;  4. Current candidate, cycling, sorting.  Both Ido and Icicles have
;;     a notion of "current candidate".  In Ido, completion candidates
;;     are presented in a predefined sort order, most recently used
;;     first.  The current candidate is the first one.  You cycle
;;     candidates by moving the first to last or the last to first.
;;
;;     In Icicles, you can switch among any number of sort orders at
;;     any time by hitting a key.  (And you can easily define your own
;;     sort orders.)  When you cycle candidates, the candidates stay
;;     in order.  If the candidates are displayed in `*Completions*'
;;     then the current one is highlighted there, in place.  The
;;     highlight moves, not the candidate.
;;
;;  5. Input editing.  In Ido, cycling does not replace your input by
;;     the current candidate.  To edit the current candidate you hit a
;;     key to enter an edit mode (recursive minibuffer).  In Icicles,
;;     cycling replaces your input in the minibuffer by the current
;;     candidate, so you can just edit it there normally.  You can use
;;     `C-l' to retrieve your original input.
;;
;;  6. Completions shown.  In Ido, a limited number of matching
;;     completion candidates are shown in the minibuffer.  You can hit
;;     a key to see all matches in a separate buffer.
;;
;;     In Icicles, completion candidates are always shown in buffer
;;     `*Completions*', not in the minibuffer.  You can limit the
;;     number of matches shown by customizing option
;;     `icicle-max-candidates'.  Only the first
;;     `icicle-max-candidates' (in the current sort order) are shown.
;;
;;     You can also increment and decrement this truncation value on
;;     the fly during completion, by hitting `C-x #' and then using
;;     the vertical arrow keys or the mouse wheel.  (For that feature
;;     you also need library `doremi.el'.)
;;
;;  6. Auto-choice of sole candidate.  In Ido, if there is only one
;;     match for your input then `TAB', which normally completes, also
;;     chooses that candidate - you do not need to hit `RET'.  By
;;     default, Icicles always requires you to explicitly choose, with
;;     `RET' (or `C-RET').  If you set option
;;     `icicle-top-level-when-sole-completion-flag' to non-`nil', then
;;     Icicles provides similar behavior to Ido.  See also option
;;     `icicle-top-level-when-sole-completion-delay'.
;;
;;(@* "Ido-Like Behavior Everywhere: `icicle-ido-like-mode'")
;;  ** Ido-Like Behavior Everywhere: `icicle-ido-like-mode' **
;;
;;  If you want Icicles to be Ido-like in general, then turn on global
;;  minor mode `icicle-ido-like-mode' (not available in Emacs 20).
;;  Doing that sets options `icicle-show-Completions-initially-flag'
;;  and `icicle-top-level-when-sole-completion-flag' to `t'.  Turning
;;  the mode off sets them to `nil'.
;;
;;  You can simultaneously set option `icicle-max-candidates' when you
;;  turn on `icicle-ido-like-mode', by using a positive prefix
;;  argument.  If you want the option to keep that value when you turn
;;  the mode off, then use a zero or negative prefix argument.
;;  Otherwise, it is reset to `nil' (no limit on the number of
;;  candidates displayed).
;;
;;  When you use this mode, you might also want to use `nil' or `t' as
;;  the value of option `icicle-default-value', in order to not insert
;;  the default value in the minibuffer.  If you want to change that
;;  option dynamically for the mode, use `icicle-ido-like-mode-hook'.
;;  E.g.:
;;
;;  (add-hook 'icicle-ido-like-mode-hook
;;            (lambda () (setq icicle-default-value
;;                        (if icicle-ido-like-mode t 'insert-end))))
;;  
;;(@* "Ido-Like Behavior for Buffers and Files")
;;  ** Ido-Like Behavior for Buffers and Files **
;;
;;  If you want Ido-like behavior in Icicles for buffers or files, but
;;  not in general, then customize either or both options
;;  `icicle-buffers-ido-like-flag' and `icicle-files-ido-like-flag' to
;;  non-`nil'.
;;
;;(@* "IswitchB-Like Behavior for `icicle-buffer'")
;;  ** IswitchB-Like Behavior for `icicle-buffer' **
;;
;;  As mentioned, you can use IswitchB with Icicles.  GNU Emacs
;;  deprecated IswitchB starting with Emacs 24.4, but it is still
;;  available.
;;
;;  If you want to get IswitchB-like behavior with Icicles without
;;  using IswitchB then you can advise `icicle-buffer'.
;;
;;  There are at least two ways to do this, depending on what behavior
;;  you want.  Let's assume that in any case (a) you want incremental
;;  completion from the outset (no need to hit `TAB' or `S-TAB', and
;;  (b) you want to Emacs to accept as your choice the sole candidate
;;  as soon as you narrow matching to a single candidate.  For (a),
;;  you bind `icicle-incremental-completion' to `always'.  For (b),
;;  you bind `icicle-top-level-when-sole-completion-flag' to `t'.
;;
;;  1. In the first case, prefix completion is the default (as usual),
;;     but `icicle-buffer' uses vanilla Emacs completion as the
;;     Icicles `TAB' completion method.  This reflects IswitchB's
;;     substring matching.  To do this, bind
;;     `icicle-current-TAB-method' to `vanilla'.
;;
;;  2. In the second case, `icicle-buffer' starts out with apropos
;;     completion, not prefix completion.  This too reflects
;;     IswitchB's substring matching, but it extends it to regexp
;;     completion.  To do this, you bind `icicle-default-cycling-mode'
;;     to `apropos'.
;;
;;  ;; 1. Use vanilla Emacs matching for prefix completion by default.
;;  (defadvice icicle-buffer (around iswitchb-like-1 activate)
;;    (interactive)
;;    (let* ((icicle-current-TAB-method                   'vanilla)
;;           (icicle-incremental-completion               'always)
;;           (icicle-top-level-when-sole-completion-flag  't))
;;      ad-do-it))
;;  (ad-activate 'icicle-buffer)
;;
;;  ;; 2. Start with apropos completion by default.
;;  (defadvice icicle-buffer (around iswitchb-like-2 activate)
;;    (interactive)
;;    (let* ((icicle-default-cycling-mode  'apropos)
;; 	     (icicle-incremental-completion  'always)
;; 	     (icicle-top-level-when-sole-completion-flag 't))
;;      ad-do-it))
;;  (ad-activate 'icicle-buffer)
;;
;;
;;  See Also:
;;
;;  * (@> "Exiting the Minibuffer Without Confirmation")
;;  * (@file :file-name "icicles-doc2.el" :to "Customization and General Tips"):
;;     `icicle-buffer-require-match-flag',
;;     `icicle-deletion-action-flag',
;;     `icicle-file-require-match-flag',
;;     `icicle-show-Completions-initially-flag',
;;     `icicle-incremental-completion',
;;     `icicle-incremental-completion-delay',
;;     `icicle-max-candidates',
;;     `icicle-regexp-quote-flag',
;;     `icicle-top-level-when-sole-completion-flag',
;;     `icicle-top-level-when-sole-completion-delay',
;;  * (@file :file-name "icicles-doc2.el" :to "Completion Methods and Styles")
;;  * (@> "Special Characters in Input Patterns")
;;  * (@> "Prefix Completion and Apropos Completion")
;;  * https://www.emacswiki.org/emacs/IciclesDiscussion#IdoAndIcicles
;;    (out-of-date discussion, but it might be helpful)
 
;;(@* "*Completions* Display")
;;
;;  *Completions* Display
;;  ---------------------
;;
;;  Icicles enhances the `*Completions*' display in several ways.  The
;;  following features are available whenever buffer `*Completions*'
;;  is displayed.
;;
;;  * In buffer `*Completions*', you can use the arrow keys (`down',
;;    `up', `right', `left') to navigate among the candidate
;;    completions.  The current candidate (under the cursor) is
;;    highlighted.
;;
;;  * When you cycle completions in the minibuffer:
;;
;;    - The current candidate is highlighted in `*Completions*'.
;;
;;    - Help on the current candidate (typically the first line of a
;;      doc string) is displayed in the mode line, provided user
;;      option `icicle-help-in-mode-line-delay' is greater than zero.
;;
;;  * The mode line of buffer `*Completions*' shows information about
;;    the current completion state, in order from the left:
;;
;;    . The minor-mode lighter for Icicles, `Icy'.  This includes one
;;      or more of these suffixes when appropriate:
;;
;;      `+'   for a multi-command
;;      `||'  when candidates are multi-completions
;;      `...' if the set of candidates in `*Completions' is truncated
;;
;;    . The total number of completion candidates - e.g. `567
;;      candidates'.  If the set of candidates in `*Completions*' is
;;      currently truncated (because of option `icicle-max-candidates'
;;      - see (@file :file-name "icicles-doc2.el" :to "Customization and General Tips"),
;;      then the total number of candidates before truncation is also
;;      shown - e.g. `149/567 candidates shown'.
;;
;;    . The current completion method: vanilla, basic, fuzzy, swank
;;      (fuzzy symbol), apropos, scatter, Levenshtein, Levenshtein
;;      strict, or Jaro-Winkler.  See
;;      (@file :file-name "icicles-doc2.el" :to "Completion Methods and Styles")
;;      and user options `icicle-TAB-completion-methods' and
;;      `icicle-S-TAB-completion-methods-alist'.
;;
;;    . The current sort order, and whether it is reversed.  See
;;      (@> "Sorting Candidates and Removing Duplicates")
;;
;;  * You can use `C-insert' to move back and forth between the
;;    minibuffer and `*Completions*'.  In each direction, the current
;;    candidate is tracked in the destination buffer.  For example, if
;;    the candidate in the minibuffer is `foobar', after you hit
;;    `C-insert' the cursor is on `foobar' in `*Completions*'.  In the
;;    other direction, if the cursor is on `foobar' in
;;    `*Completions*', after you hit `C-insert' the current input in
;;    the minibuffer is `foobar'.
;;
;;  * `*Completions*' can also serve as a new kind of icompletion help
;;    - see (@> "Icompletion").
;;
;;  * You can choose multiple candidates during completion, by
;;    clicking them with `mouse-2' while holding the Control key
;;    pressed.  See (@> "Multi-Commands").  You can choose a set of
;;    candidates in additional ways, and then act on all of them - see
;;    (@> "Sets of Completion Candidates").
;;
;;  * Icicles dynamically resizes the `*Completions*' window
;;    vertically, to fit the current set of completion candidates.
;;    The window is not resized, however, if buffer `*Completions*'
;;    appears in its own frame.  (It is also not resized in Emacs
;;    releases prior to 21.)
;;
;;    You can control this automatic resizing generally or on a
;;    per-command basis:
;;
;;     * User option `icicle-Completions-window-max-height' is the
;;       maximum number of lines to show in the `*Completions*'
;;       window.
;;
;;     * You can override the behavior of option
;;       `icicle-Completions-window-max-height' for any given command,
;;       by setting property `icicle-Completions-window-max-height' on
;;       the command symbol to a different maximum window height
;;       value.  This property value is predefined for commands, such
;;       as `icicle-buffer' and `icicle-file', for which you do not
;;       need to see much of the current buffer contents during
;;       completion.  A large value is used for these commands, so
;;       that nearly all of the frame real estate is given to the
;;       `*Completions*' window.
;;
;;    For example, you can use the following code to set the maximum
;;    `*Completions*' height for command `foo' to 100 and turn off
;;    per-command control of the height for command `bar'.  If you use
;;    such code, evaluate it after you load Icicles.
;;
;;       (put 'foo 'icicle-Completions-window-max-height 100)
;;       (put 'bar 'icicle-Completions-window-max-height nil)
;;
;;  * Starting with Emacs 23, if you also use Do Re Mi library
;;    `doremi-frm.el', then you can use `C-x -' in the minibuffer to
;;    zoom the `*Completions*' buffer text, shrinking or enlarging it
;;    incrementally using `-' or `=', respectively (`=' is typically
;;    on the same keyboard key as `+', but it needs no Shift).
;;
;;    Also starting with Emacs 23 (whether or not you use
;;    `doremi-frm.el'), you can specify an initial text-scale amount
;;    for the `*Completions*' text, by customizing option
;;    `icicle-Completions-text-scale-decrease'.  This controls the
;;    default appearance.
;;
;;    You typically use these features to make the `*Completions*'
;;    text a bit smaller and thus save screen real estate - show more
;;    candidates in less space.  However, Emacs 23 text-scaling does
;;    not by itself let you recuperate the saved window space - it
;;    shrinks the text, but it does not shrink the window accordingly.
;;    For that, you also need library `face-remap+.el' and its option
;;    `text-scale-resize-window', which lets you resize the window or
;;    not, horizontally, vertically, or in both directions.
;;
;;    (For example, you might set `split-width-threshold' to a small
;;    number, so `*Completions*' appears on the side rather than above
;;    or below other windows, and in that case you might want to
;;    resize it only horizontally.)
;;
;;    If you use library `oneonone.el' with a standalone
;;    `*Completions*' frame, then see option
;;    `1on1-completions-frame-zoom-font-difference'.
;;
;;  * If you use a dedicated frame for buffer `*Completions*' then
;;    Icicles gives it the same font as the window that was selected
;;    when `*Completions*' is displayed.  This means, for instance,
;;    that if that original window has a font tailored to a specific
;;    locale or Unicode character set then `*Completions*' will use
;;    that same font.
;;
;;  * Icicles varies the number of columns used to display completion
;;    candidates, for a better fit.  You can tweak this with options
;;    `icicle-candidate-width-factor' and
;;    `icicle-inter-candidates-min-spaces'.
;;
;;    If you use Do Re Mi (library `doremi.el'), then you can modify
;;    these options incrementally during completion, seeing the effect
;;    as they change.  Use `C-x w' or `C-x |' from the minibuffer,
;;    then use the arrow keys or the mouse wheel to increment and
;;    decrement the current value.  WYSIWYG.
;;
;;    Why is this important enough to merit changing it dynamically,
;;    instead of just customizing the options once and for all?
;;    Because different sets of candidates have different candidate
;;    lengths and distributions of those lengths.  Play with it a bit
;;    and you will see.  One size does not fit all in an ideal way.
;;
;;    (You can use option `icicle-Completions-max-columns' to prevent
;;    Icicles from automatically calculating the number of columns to
;;    display, but I do not recommend this.)
;;
;;  * You can use `C-x .' (`icicle-toggle-hiding-common-match') in the
;;    minibuffer at any time during completion to toggle hiding of the
;;    matched portions of the candidates in `*Completions*'.  This
;;    portion is replaced by ellipsis, `...'.  (In Emacs 20, it is
;;    replaced by nothing.)
;;
;;    This can be useful when you do not care about the text that
;;    matches or when that text is particularly long.  For example, if
;;    you use `icicle-find-file-absolute' (`C-u C-x C-f') and the
;;    completion candidates are absolute file names that share a
;;    common directory, it can be convenient to hide the directory
;;    portion that is common to all candidates.
;;
;;  * Starting with Emacs 22, if option
;;    `icicle-image-files-in-Completions' is non-`nil' (it is `t' by
;;    default) then thumbnail images are shown in `*Completions*' for
;;    candidates that are either (relative or absolute) names of image
;;    files or names of image-file bookmarks (see Bookmark+). If the
;;    option value is `image-only', then only the thumbnail images are
;;    shown.  If it is otherwise non-`nil' then the file names are
;;    also shown.  You can cycle the option value using `C-x t' in the
;;    minibuffer at any time during completion.
;;
;;  * Starting with Emacs 22, if option
;;    `icicle-image-preview-in-tooltip' is non-`nil' and
;;    `tooltip-mode' is on, then passing the mouse over an image-file
;;    candidate in `*Completions*' can pop up a tooltip showing a
;;    preview of the image.  The possible values are `nil' (do not
;;    show a tooltip preview), `full' (show a full-size tooltip
;;    preview), and a positive integer (show a thumbnail tooltip
;;    preview of that size).
;;
;;    However, thumbnail tooltip previews are not shown if thumbnails
;;    are already shown in `*Completions*', that is, if option
;;    `icicle-image-files-in-Completions' is non-`nil' and
;;    `icicle-image-preview-in-tooltip' is not `full'.
;;
;;  * You can scroll `*Completions*' down using `C-v', and up using
;;    `M-v'.  You can use `C-u' at any time to reverse the scroll
;;    directions.  In Emacs 22 or later, you can also use the mouse
;;    wheel to scroll `*Completions*'.
;;
;;  * You can lay completion candidates out vertically, if you like,
;;    instead of horizontally (the default).  To do that, customize
;;    option `icicle-completions-format' to have the value `vertical'.
;;
;;    You can toggle the completions layout between horizontal and
;;    vertical at any time during completion, using `C-M-^'.
;;
;;    Starting with Emacs 23.2, this is also possible in vanilla
;;    Emacs, and the vanilla option for this is `completions-format'.
;;    The default value of `icicle-completions-format' is the value of
;;    `completions-format', so if you prefer you can simply use the
;;    vanilla Emacs option.  Vertical layout works in Icicles for all
;;    Emacs versions, starting with Emacs 20.
;;
;;    Unlike the case for vanilla Emacs, in Icicles the arrow keys in
;;    buffer `*Completions*' correctly reflect the candidate order
;;    (e.g. as currently sorted).  This also means that candidate
;;    cycling acts properly for a vertical layout.
;;
;;    Note: For visual clarity, when multi-line candidates are used
;;    (and this includes multi-line multi-completions) the candidates
;;    are displayed in a single column.  When this is the case, the
;;    completions format (horizontal or vertical) makes no difference
;;    - the effect is the same. (Icicles also inserts an empty line
;;    after each multi-line candidate, for readability.)
;;    See also (@file :file-name "icicles-doc2.el" :to "Icicles User Options and Faces")
;;
;;  * In some cases, Icicles adds one or more additional, proxy
;;    completion candidates.  These are placeholders for real
;;    candidates.  If you choose a proxy candidate, then the real
;;    candidate that it refers to is used.  Typical proxy candidates
;;    include a reference to a name under the cursor, a reference to
;;    whatever you then click `mouse-2' on, and a reference to a name
;;    that is the value of a variable.
;;
;;    The inclusion of proxy candidates is controlled by user option
;;    `icicle-add-proxy-candidates-flag'.  You can toggle this
;;    inclusion at any time during completion, using `C-M-_'.  For
;;    performance reasons, you must re-invoke some commands after
;;    toggling the flag on, to make the proxy candidates available.
;;
;;    Examples:
;;
;;     . When you read a file name with completion, the proxy
;;       candidates include the following (reading a face name is
;;       similar):
;;
;;       - `*mouse-2 file name*' - proxy for a file name that you
;;         click with `mouse-2'.
;;
;;       - `*point file name*' - proxy for the file name at point (if
;;         available).
;;
;;       - Single-quoted names of file-name variables - proxy for the
;;         variable value.
;;
;;     . When a command reads input using `icicle-read-number' or
;;       `icicle-read-string-completing', the proxy candidates are all
;;       variables whose values are numbers or strings, respectively.
;;       You can choose such a proxy candidate to use its value.  (All
;;       candidates are proxy candidates for these functions.)
;;
;;     . When you use command `icicle-read-color-WYSIWYG', the proxy
;;       candidates include the following:
;;
;;       - `*point foreground*' - proxy for the foreground color at
;;         the cursor position (point).
;;
;;       - `*mouse-2 foreground*' - proxy for the foreground color
;;         where you then click `mouse-2'.
;;
;;       - `*copied foreground*' - proxy for a previously copied
;;         foreground color, the value of variable
;;         `eyedrop-picked-foreground'.
;;
;;       - Background versions of the first three: `*copied
;;         background*' etc.
;;
;;       - Single-quoted names of color-valued variables - proxy for
;;         the variable value.
;;
;;    See Also:
;;    (@> "Different Places for Saving and Retrieving Candidates")
;;    for information about using `C-M-{' in the minibuffer to
;;    retrieve the value of any variable as minibuffer input.
;;
;;    Proxy Gotcha #1: Your minibuffer input is matched against proxy
;;    candidates, as usual.  If `insert-default-directory' is
;;    non-`nil' when you use `C-x d' (`icicle-dired'), then the
;;    default directory is inserted as part of your input.  If you
;;    then use `TAB' for completion then you will first need to use
;;    `M-k' to remove the inserted directory, as it will not match any
;;    of the proxy candidates.
;;
;;    Proxy Gotcha #2: If you enter text that matches a proxy
;;    candidate then the value of that proxy is taken as the real
;;    input - that's the whole point of proxy candidates.  But
;;    suppose, in Info, you want to (non-incrementally) search for the
;;    text `ftp-program', which is the name of a variable?  If you
;;    type `s ftp-program RET' then the *value* of that variable
;;    (typically `ftp') is taken as the text to search for, instead of
;;    the literal text `ftp-program'.  To take care of this, use
;;    `C-M-_' in the minibuffer to turn off including proxy
;;    candidates.
;;
;;  * Clicking `C-mouse-3' on a candidate in `*Completions*' pops up a
;;    contextual menu for acting on completion candidates.  You can
;;    customize the menu using option
;;    `icicle-Completions-mouse-3-menu-entries'.  By default, the menu
;;    has the following submenus:
;;
;;     . `This Candidate' - Act on the candidate that you clicked to
;;       pop up the menu.  Or act on all current candidates,
;;       individually or collectively.
;;
;;     . `Sorting' - Change the current candidate sort order.
;;
;;     . `Save/Retrieve' - Save (mark) candidates or retrieve them,
;;       including to/from a variable or a cache file.
;;
;;     . `Sets' - Perform operations on sets of candidates, in
;;       particular, the set of current candidates and the set of
;;       saved candidates.
;;
;;     . `Toggle/Cycle/Change' - Toggle, cycle, or otherwise change an
;;       Icicles setting, altering the behavior on the fly.
;;
;;     . `Miscellaneous' - Other candidate operations and general
;;       help.
;;
;;    The popup menu is contextual.  In particular, the available
;;    items can change depending on whether you use a prefix argument
;;    (`C-u C-mouse-3') and whether you have selected candidates using
;;    the region (e.g. mouse drag).  The menu also provides a good
;;    reminder of key bindings available during completion.
;;
;;  There are lots more Icicles features that enhance the display and
;;  behavior of `*Completions*' in some way.  Read on...
;;
;;  See Also: 
;;
;;  * (@> "Moving Between the Minibuffer and Other Buffers"), for
;;    information on the `pause' key, which is somewhat related to
;;    using `C-insert'.
;;
;;  * (@file :file-name "icicles-doc2.el" :to "Candidates with Text Properties")
;;    and (@file :file-name "icicles-doc2.el" :to "Programming Multi-Completions")
;;    for information about using text properties in `*Completions*'.
;;    These sections are for Emacs-Lisp programmers.
 
;;(@* "Icompletion")
;;
;;  Icompletion
;;  -----------
;;
;;  Emacs incremental completion, or icompletion, provided by
;;  Icomplete mode from standard library `icomplete.el', displays
;;  matching completions in the minibuffer.  The completions are,
;;  however, what vanilla Emacs thinks they are.
;;
;;  In general, this means prefix completions, but whatever it might
;;  mean in any context, the point is that these completions might not
;;  be those that you see when you use Icicles completion.  For
;;  example, if you use Icicles apropos completion (`S-TAB') then the
;;  candidates are typically not reflected in the candidates that
;;  Icomplete mode shows in the minibuffer.
;;
;;  Icomplete mode provides completion help, letting you know which
;;  (prefix) completions are available.  The display of candidates
;;  shown by Icomplete mode is updated incrementally as you type
;;  characters.
;;
;;  Icicles enhances Emacs incremental completion in these ways:
;;
;;  1. It plays well with standard Icomplete mode.
;;
;;  2. It highlights the part of your input that does not match any
;;     completion candidate.
;;
;;  3. It provides a new kind of incremental completion, using buffer
;;     `*Completions*'.
;;
;;(@* "Using Icicles with Icomplete Mode")
;;  ** Using Icicles with Icomplete Mode **
;;
;;  Icicles interacts with Icomplete mode in a number of ways.
;;
;;  * In Icicle mode you can toggle Icomplete mode on and off using
;;    `C-M-#' (command `icicle-toggle-icomplete-mode') anytime in the
;;    minibuffer, assuming you have already loaded library
;;    `icomplete(+).el'.  For Emacs versions prior to Emacs 23, the
;;    change takes effect only at the next minibuffer invocation.
;;
;;  * Starting with Emacs 23, Icicles can automatically turn Icomplete
;;    mode off and on, depending on the number of current candidates.
;;    This is a performance optimization, to eliminate the
;;    (independent) calculation by Icomplete mode of a large set of
;;    candidates.
;;
;;    You can control this behavior by customizing user option
;;    `icicle-icomplete-mode-max-candidates'.  Set it to `nil' to turn
;;    off this automatic Icomplete mode toggling.  Set it to a larger
;;    or smaller maximum number of candidates to reduce or increase
;;    its effect.
;;
;;    It is the act of displaying completion candidates that causes
;;    Icicles to check the number of candidates against the option
;;    value and thus perhaps toggle the mode.  This is the case
;;    regardless of how completion is initiated: whether manually
;;    (`TAB' or `S-TAB') or automatically.  But it is not the case for
;;    Icicles completion that does not display candidates (`C-M-TAB',
;;    `C-M-S-TAB').  (See also
;;    (@* "Icompletion in *Completions*: Apropos and Prefix Completion"),
;;    below).
;;
;;  * In addition to this automatic toggling, Icicles multi-commands
;;    that typically display a large number of completion candidates
;;    automatically turn off Icomplete mode.  When this happens, a
;;    message informs you.  Just use `C-M-#' (command
;;    `icicle-toggle-icomplete-mode') to turn it back on.
;;
;;    You can control which commands do this automatic toggling, by
;;    adding/removing property `icicle-turn-off-icomplete-mode'
;;    to/from the command symbol.  For example, command
;;    `icicle-locate-file' has the non-`nil' property by default.  If
;;    you do not want this command to turn off Icomplete mode then add
;;    this to your init file (`~/.emacs'):
;;
;;      (put 'icicle-locate-file 'icicle-turn-off-icomplete-mode nil)
;;
;;  * Icicles works with my library `icomplete+.el' to provide
;;    minibuffer feedback on the number of Icomplete-completion
;;    candidates when you cycle.  This number is displayed whenever
;;    you change direction when cycling.  For example:
;;
;;      M-x forward-line   [Matched]  (13 more)
;;
;;(@* "Icompletion in *Completions*: Apropos and Prefix Completion")
;;  ** Icompletion in *Completions*: Apropos and Prefix Completion **
;;
;;  Buffer `*Completions*' shows you the current set of candidates for
;;  either prefix or apropos completion.  Together, user options
;;  `icicle-incremental-completion',
;;  `icicle-incremental-completion-delay', and
;;  `icicle-incremental-completion-threshold' control incremental
;;  updating of `*Completions*'.
;;
;;  And together with option `icicle-expand-input-to-common-match'
;;  they control incremental expansion (completion) of your input in
;;  the minibuffer.
;;
;;  If option `icicle-incremental-completion' is non-`nil', then
;;  `*Completions*' is automatically updated whenever you change your
;;  input in the minibuffer - that is, with each character that you
;;  type or delete.  This is another form of icompletion.  Unlike
;;  vanilla icompletion, it uses buffer `*Completions*', not the
;;  minibuffer, to show the completion help.
;;
;;  The particular non-`nil' value of `icicle-incremental-completion'
;;  determines when `*Completions*' is displayed and updated.  The
;;  default value, `t', means that `*Completions*' is updated only if
;;  it is already displayed.  Use `t' if you do not want
;;  `*Completions*' to be too intrusive, but you want it to provide
;;  the most help when you ask for help (via `TAB' or `S-TAB').
;;
;;  Any other non-`nil' value is more eager.  It displays and updates
;;  `*Completions*' whenever there is more than one completion
;;  candidate.  That can be more helpful, but it can also be more
;;  distracting.  A value of `nil' turns off automatic updating
;;  altogether - `*Completions*' is then displayed only upon demand.
;;  I find that `t' represents a good compromise, providing help when
;;  I ask for it, and then continuing to help until I've finished
;;  choosing a candidate.
;;
;;  A completion behavior even more eager than that produced by
;;  setting `icicle-incremental-completion' to non`nil' and non-`t'
;;  can be had by setting option
;;  `icicle-show-Completions-initially-flag' to non-`nil'.  In that
;;  case, completions are shown even before you type any input.  You
;;  see all of the possible candidates initially, unfiltered by any
;;  typed input.  In this regard, see also (@> "Ido and IswitchB").
;;
;;  When you are editing, Icicles normally does not interrupt you to
;;  recalculate and redisplay the matches after each character you
;;  type or delete.  Option `icicle-incremental-completion-delay' is
;;  the number of seconds to wait before updating `*Completions*'
;;  incrementally.  It has an effect only when the number of
;;  completion candidates is greater than
;;  `icicle-incremental-completion-threshold'.  This delay can improve
;;  performance when there are many candidates.  It lets you type
;;  ahead before candidate redisplay occurs.
;;
;;  You can cycle Icicles incremental completion at any time (changing
;;  `icicle-incremental-completion' among `nil', `t', and `always')
;;  using command `icicle-cycle-incremental-completion', which is
;;  bound to `C-#' in the minibuffer.  If the number of completion
;;  candidates is very large, then use `C-#' to turn off incremental
;;  completion - that will save time by not updating `*Completions*'.
;;  See also (@> "Dealing With Large Candidate Sets") for other ways
;;  to deal with a large number of candidates.
;;
;;  Note: Incremental completion is effectively turned off whenever a
;;  remote file name is read, that is, whenever your file-name input
;;  matches a remote-file syntax.
;;
;;  There are several advantages of using `*Completions*' for
;;  icompletion, as opposed to the minibuffer as in vanilla Emacs
;;  (Icomplete mode):
;;
;;  1. Many more candidates can be displayed in `*Completions*' than
;;     can be displayed by Icomplete mode, which uses the minibuffer
;;     for feedback.
;;
;;  2. Icomplete mode provides feedback wrt vanilla Emacs completions,
;;     which do not necessarily correspond to the current Icicles
;;     completions (e.g., for `S-TAB').  If you use both Icomplete
;;     mode and Icicles incremental completion then you can have
;;     incremental help for both prefix completion and apropos
;;     completion at the same time, one in the minibuffer and the
;;     other in `*Completions*'.
;;
;;  3. The other Icicles `*Completions*' features are available for
;;     the current set of matching candidates: cycling, highlighting
;;     of match root, highlighting of previously used candidates, and
;;     so on.  See (@> "*Completions* Display").
;;
;;  Although you can cycle Icicles incremental completion at any time
;;  using `C-#', Icicles multi-commands that typically display a large
;;  number of completion candidates automatically turn off incremental
;;  completion.  When this happens, a message informs you.  Just use
;;  `C-#' to turn it back on.
;;
;;  You can control which commands do this, by adding/removing
;;  property `icicle-turn-off-incremental-completion' to/from the
;;  command symbol.  For example, command `icicle-locate-file' has the
;;  non-`nil' property by default.  If you do not want this command to
;;  turn off Icicles incremental completion then add this to your init
;;  file (`~/.emacs'):
;;
;;    (put 'icicle-locate-file
;;         'icicle-turn-off-incremental-completion
;;         nil)
;;
;;(@* "Incremental Completion (Input Expansion) in the Minibuffer")
;;  ** Incremental Completion (Input Expansion) in the Minibuffer **
;;
;;  As mentioned in (@> "Icompletion in *Completions*: Apropos and Prefix Completion"),
;;  another Icicles option, `icicle-expand-input-to-common-match',
;;  controls whether Icicles completes your input in the minibuffer.
;;  There are five levels of this expansion behavior to choose from:
;;  option values 0 to 4.  When expansion occurs it is always the
;;  same.  See (@> "Expanded-Common-Match Completion") for the
;;  description.  What this option controls is whether and when
;;  expansion takes place.
;;
;;  At one extreme (value 0), your input is never expanded (except
;;  when you use `C-M-TAB' or `C-M-S-TAB', which do not display
;;  `*Completions*').  This is unlike vanilla Emacs but similar to Ido
;;  and IswitchB: you look to `*Completions*' for the matches, and you
;;  keep typing (or you cycle) to narrow things down and choose.  If
;;  you use a non-`nil', non-`t' value for
;;  `icicle-incremental-completion' then you might also want to try a
;;  value of 0 for `icicle-expand-input-to-common-match'.
;;
;;  At the other extreme (value 4), your input is expanded in the
;;  minibuffer whenever it is completed in `*Completions*'.  This
;;  includes automatic, incremental expansion, analogous to
;;  incremental completion.
;;
;;  A value of 1 means expand your input only when you explicitly
;;  request completion, using `TAB' or `S-TAB'.
;;
;;  A value of 2 means do that, but also expand your input whenever it
;;  matches only one completion candidate.  This lets you hit `RET' to
;;  choose it (instead of having to hit `S-RET').
;;
;;  A value of 3 means do what 2 does, but also expand your input each
;;  time prefix completion occurs.  It is the same as 4, except that
;;  it does not incrementally expand your input for apropos
;;  completion.
;;
;;  For a long time Icicles had only the equivalent of values 3 and 4,
;;  and I consider them the most useful.  Sometimes you will want to
;;  temporarily turn off expansion for apropos completion.  You can do
;;  that using `C-"' in the minibuffer: it toggles between any two of
;;  the behaviors (3 and 4, by default - see option
;;  `icicle-expand-input-to-common-match-alt').  You can use `C-M-"'
;;  in the minibuffer to cycle among all behaviors.
;;
;;  See Also:
;;
;;  * (@> "Expanded-Common-Match Completion")
;;
;;  * (@> "What Input, What History?")
;;
;;  * (@file :file-name "icicles-doc2.el" :to "Customization and General Tips")
;;
;;(@* "Icicles Highlights the Input that Won't Complete")
;;  ** Icicles Highlights the Input that Won't Complete **
;;
;;  When you are typing or correcting your input during completion,
;;  Icicles highlights the part of your minibuffer input that prevents
;;  it from matching any completion candidates, by default.  This
;;  works for both prefix completion and apropos completion.  For
;;  both, it highlights your input from the leftmost mismatch through
;;  the input end.
;;
;;  You can use `C-M-l' to move the cursor to the start of the
;;  highlighted, mismatched part of your input.  Repeat `C-M-l' to
;;  kill the highlighted portion.  (Because it is killed, you can use
;;  `C-y' to yank it back.)
;;
;;  User options `icicle-incremental-completion',
;;  `icicle-test-for-remote-files-flag',
;;  `icicle-highlight-input-completion-failure',
;;  `icicle-highlight-input-completion-failure-delay', and
;;  `icicle-highlight-input-completion-failure-threshold' control this
;;  highlighting, which is done using face
;;  `icicle-input-completion-fail' (for strict completion) or
;;  `icicle-input-completion-fail-lax' (for lax completion).
;;
;;  If either `icicle-incremental-completion' or
;;  `icicle-highlight-input-completion-failure' is `nil', then no such
;;  highlighting is done.  Remember that you can cycle incremental
;;  completion on and off using `C-#' in the minibuffer.
;;
;;  Because this highlighting can have a negative impact on
;;  performance, you can fine-tune when you want it to occur.  The
;;  value of `icicle-highlight-input-completion-failure' determines
;;  when this highlighting can take place.
;;
;;  In particular, highlighting the non-matching part of remote file
;;  names can be slow.  Two values of the option allow remote file
;;  name highlighting: `always' and `explicit-remote'.  The other
;;  values do not highlight remote file names.  You probably do not
;;  want to use a value of `always'.
;;
;;  If the value is `nil', then highlighting never occurs.  If the
;;  value is `explicit-strict', `explicit', or `explicit-remote', then
;;  highlighting occurs only upon demand: when you hit `TAB' or
;;  `S-TAB' to request completion.  If the value is `implicit-strict',
;;  `implicit', or `always', then highlighting occurs also when you
;;  update input during incremental completion (if incremental
;;  completion is turned on).
;;
;;  I use a value of `implicit' myself, but the default value is
;;  `implicit-strict' because, depending on your setup and use cases,
;;  `implicit' can impact performance for file-name completion (which
;;  is lax, not strict).  I suggest you try `implicit' to see - this
;;  feature is especially useful for file names.
;;
;;    TIP: An alternative way to be informed about a file name
;;         mismatch (when you use `TAB' or `S-TAB') is to use
;;         `icicle-no-match-hook' to signal you using a visual or
;;         audible cue.  For example:
;;
;;         (add-hook 'icicle-no-match-hook
;;                   (lambda ()
;;                     (when (icicle-file-name-input-p) (ding))))
;;
;;  Summary of `icicle-highlight-input-completion-failure' choices:
;;
;;  `nil'             Never
;;  `explicit-strict' Only on demand and only during strict completion
;;  `explicit'        Only on demand (lax and strict completion)
;;  `explicit-remote' Only on demand, even for remote file names
;;  `implicit-strict' Incremental or explicit completion - strict only
;;  `implicit'        Incremental/explicit, lax/strict completion
;;  `always'          Always (including for remote file names)
;;
;;  These values are listed here in terms of increasing
;;  permissiveness, which also can mean increasing performance impact.
;;  That is, a value of `implicit' is more likely to affect
;;  performance than a value of `explicit'.  The greatest performance
;;  hit comes from file-name matching, in particular if remote files
;;  are involved.
;;
;;  If you know that you will not be using remote file names for a
;;  while, you can let Icicles and Tramp know this by using the toggle
;;  `C-^' in the minibuffer to turn off option
;;  `icicle-test-for-remote-files-flag'.  When this is off, you cannot
;;  use remote files.
;;
;;  Turning off `icicle-test-for-remote-files-flag' using `C-^' turns
;;  off Tramp's remote file-name completion and remote file handling.
;;  If you turn off the option using `C-^', then turn it back on using
;;  `C-^' also (instead of just setting the option to non-`nil'), in
;;  order to re-enable Tramp's file-name handling and completion.
;;
;;  Turning off `icicle-test-for-remote-files-flag' can slightly speed
;;  up file-name completion for local files, by avoiding any check for
;;  remote file names.  If you seldom use remote files, then you might
;;  want to customize `icicle-test-for-remote-files-flag' to `nil' and
;;  use `C-^' to toggle it back on whenever you do use remote files.
;;
;;  A `nil' value of `icicle-test-for-remote-files-flag' also
;;  overrides the `icicle-highlight-input-completion-failure' values
;;  `implicit-strict', and `explicit-strict' for file-name completion,
;;  treating them the same as `implicit'.  It is assumed that you use
;;  those values only to avoid the cost of remote file-name matching.
;;
;;  Finally, whether to treat files on MS Windows mapped network
;;  drives as remote or local for things like incremental completion
;;  is controlled by option `icicle-network-drive-means-remote-flag'.
;;  Customize it to `nil' if you would like such files to be treated
;;  as local files.  You can toggle this option anytime during
;;  completion, using `C-x :'.
 
;;(@* "Sorting Candidates and Removing Duplicates")
;;
;;  Sorting Candidates and Removing Duplicates
;;  ------------------------------------------
;;
;;  The current set of completion candidates matching your minibuffer
;;  input can be sorted in various ways.  The sort order affects not
;;  only the display in buffer `*Completions*' but also cycling among
;;  candidates.  In general, candidate sorting is alphabetical by
;;  default, but some commands use different sort orders by default.
;;
;;  Also, duplicate candidates are typically removed as completion
;;  choices, by default.  But for some commands duplicates are
;;  appropriate, so they are not removed.  For example, command
;;  `icicle-search' (`C-c `') uses completion to navigate among search
;;  hits.  Duplicate search hits are retained.  Although some
;;  search-hit candidates might have the same text, they are located
;;  at different buffer positions.
;;
;;  You can interactively control the order of candidates and whether
;;  duplicates are removed.  Use `C-,' during completion to choose a
;;  different sort order or to turn off sorting altogether (one of the
;;  available sort orders is in fact called "turned OFF").  Use `C-$'
;;  to toggle the removal of duplicate candidates.
;;
;;  A few commands, for which sorting is inappropriate, prevent you
;;  from sorting.  For all other commands, Icicles can automatically
;;  turn candidate sorting off and on, depending on the number of
;;  completion candidates.  This is a performance optimization, for
;;  use when the current set of candidates is large.
;;
;;  You can control this behavior by customizing user option
;;  `icicle-sorting-max-candidates'.  Set it to `nil' to turn off this
;;  automatic toggling of sorting.  Set it to a larger or smaller
;;  maximum number of candidates to reduce or increase its effect.
;;
;;  It is the act of completion that causes Icicles to check the
;;  number of candidates against the option value and thus perhaps
;;  toggle sorting.  This is the case regardless of how completion is
;;  initiated: whether manually (`TAB' or `S-TAB') or automatically
;;  (see (@* "Icompletion in *Completions*: Apropos and Prefix Completion")).
;;
;;  The available sort orders for `C-,' are those defined by user
;;  option `icicle-sort-orders-alist' - see
;;  (@> "Defining New Sort Orders"), below.  However, some commands
;;  adjust this list of possibilities by adding command-relevant sort
;;  orders or removing some that might be inappropriate.
;;
;;  The sort order generally remains as you last set it, for
;;  subsequent commands.  However, if the last order you set is
;;  inappropriate for the current command then sorting is turned off.
;;  You can then use `C-,' to choose a sort order appropriate for the
;;  current command.
;;
;;(@* "Changing the Sort Order")
;;  ** Changing the Sort Order **
;;
;;  There are a couple of ways to use `C-,' (bound to command
;;  `icicle-change-sort-order'), to change the current sort order.
;;  Its behavior depends on the value of user option
;;  `icicle-change-sort-order-completion', which is 7 by default.  The
;;  option value determines whether `C-,' lets you use completion to
;;  choose a sort order or it just cycles to the next order.  The
;;  default value of 7 means cycle if there are no more than 7 sort
;;  orders for the current command, and complete if there are more
;;  than 7.
;;
;;  The option value can be an integer, as described, or it can be
;;  `nil', meaning always cycle, or otherwise non-`nil' (e.g. `t'),
;;  meaning always complete.
;;
;;  Whatever the value of `icicle-change-sort-order-completion', a
;;  non-numeric prefix argument (e.g., plain `C-u') flips the behavior
;;  defined by the option: if completion would normally be used then
;;  `C-u C-,' cycles to the next sort order.  If cycling would
;;  normally be used then `C-u C-,' initiates completion to choose a
;;  sort order.
;;
;;  For example, suppose you use the default option value of 7 and
;;  there are fewer than 7 sort orders for the completion operation
;;  provided by the current command.  In this case, `C-,' cycles to
;;  the next order.  But if you would like to see what the available
;;  orders are, just use `C-u C-,' and pick one using completion.
;;
;;  A numeric prefix argument (e.g. `C-9') reverses the direction of
;;  the current sort order.  It invokes command
;;  `icicle-reverse-sort-order'.
;;
;;  For example, if candidates are currently sorted alphabetically
;;  from A to Z, then `C-9 C-,' flips the sort order, so that from
;;  then on sorting is from Z to A.  If buffer names are sorted from
;;  small to large buffer size, then `C-9 C-,' sorts large buffers
;;  first.  This works for all sort orders.
;;
;;  Whenever you change the sort order, the new sort order is echoed,
;;  so you can use `C-9 C-,' twice to just remind yourself of the
;;  current sort order.
;;
;;  Note that if sorting has been turned off automatically for the
;;  current minibuffer invocation because there are more candidates
;;  than `icicle-sorting-max-candidates', using `C-,' to choose a new
;;  sort order does not turn sorting back on.  The newly chosen sort
;;  order takes effect only when sorting is turned on again
;;  (e.g. because there are fewer candidates).
;;
;;  In addition to the current sort order, which is defined by the
;;  value of user option `icicle-sort-comparer', an alternative sort
;;  order is available at all times.  It is the value of option
;;  `icicle-alternative-sort-comparer'.  By default, this sorts
;;  candidates into two alphabetical groups: those previously used as
;;  accepted input, followed by those not yet used.
;;
;;  Just as you can choose a different current sort order using `C-,',
;;  so you can choose a different alternative sort order using `M-,'.
;;  It behaves the same way, including regarding option
;;  `icicle-change-sort-order-completion' and a prefix argument.
;;
;;  How do you actually use the alternative sort order?  Use `C-M-,'
;;  (command `icicle-toggle-alternative-sorting') to swap the
;;  alternative sort for the current sort.  This is the quickest way
;;  to flip between two sort orders.  If, for example, you set your
;;  alternative sort order to "turned OFF", then this is a quick way
;;  to toggle sorting on and off.
;;
;;  The first time during a session that you use a command that
;;  completes a buffer name or a file name, sorting changes to (that
;;  is, `icicle-sort-comparer' is set to) whatever is specified by
;;  user option `icicle-buffer-sort' or `icicle-file-sort',
;;  respectively, provided the option is non-`nil'.  This gives you a
;;  way to privilege a particular sorting method for each of these
;;  kinds of completion.
;;
;;(@* "Defining New Sort Orders")
;;  ** Defining New Sort Orders **
;;
;;  When you use `C-,' or `M-,', the sort orders that you can choose
;;  from are those in user option `icicle-sort-orders-alist'.  You can
;;  customize this option to add or remove available sort orders.  A
;;  better way to define a new sort order is to use macro
;;  `icicle-define-sort-command' in your Emacs init file (`~/.emacs').
;;  This defines a new Icicles command, named `icicle-sort-ORDER',
;;  where `ORDER' is the name of the new sort order.  The definition
;;  of the "alphabetical" sort order provides an example:
;;
;;    (icicle-define-sort-command "alphabetical"
;;                                icicle-case-string-less-p
;;      "Sort completion candidates alphabetically.")
;;
;;  The first argument, "alphabetical", is a string naming the new
;;  sort order.  When you change to this sort order, a message says
;;  "Sorting is now alphabetical".  Whatever sort-order name you
;;  provide is used in the message.
;;
;;  The second argument is the actual function used for sorting.  It
;;  can be any function, including a lambda expression.  The function
;;  takes two string arguments and returns non-`nil' if and only if
;;  the first string sorts before (is "less than") the second.  In
;;  this case, function `icicle-case-string-less-p' is used, which
;;  compares its two arguments alphabetically (lexicographically).
;;  The third argument is the doc string for the new sorting command.
;;
;;  The result of this definition is:
;;
;;  1. The creation of command `icicle-sort-alphabetical'.
;;  2. The addition of an entry for the new sort order in option
;;     `icicle-sort-orders-alist'.  The entry associates sort order
;;     "alphabetical" with comparison function
;;     `icicle-case-string-less-p'.
;;
;;  You can invoke the new sorting command any time using `M-x', but
;;  you can also change to the new sort order using `C-,' (or `M-,')
;;  during minibuffer completion.
;;
;;(@* "Different Sorts for Different Sorts of Uses")
;;  ** Different Sorts for Different Sorts of Uses **
;;
;;  There are many different uses of completion in Emacs, and this
;;  means that sorting candidates needs to be flexible - there cannot
;;  be a single sort order, or even a single set of sort orders, that
;;  is useful for all purposes.  Completion, and therefore also
;;  sorting of completion candidates, needs to deal with different
;;  types of candidates and different numbers of them, in different
;;  contexts.
;;
;;  Icicles predefines many sort functions, and you can easily define
;;  more of your own.  You can choose a different sort at any time, as
;;  mentioned above.  A good sort order can be a big help, depending
;;  on the context.  However, sorting is not free, and it can be
;;  helpful to think for a moment about some of the consequences of
;;  sorting, in terms of performance.
;;
;;  What does a sort function do?  It determines which of two strings
;;  should come first, that is, which is "less than" the other.
;;  During sorting, pairs of candidates are compared using the sort
;;  function.  And each time you change your input by typing or
;;  deleting a character, the new set of matching candidates is sorted
;;  (if `icicle-incremental-completion' is non-`nil').
;;
;;  The number of candidates to be sorted depends on the kind of
;;  completion and how you use Icicles.  Some Icicles users like to
;;  use cycling more and completion less, which means sorting more
;;  candidates.  Other users favor using completion to narrow down the
;;  number of matches (which I recommend).  Some commands typically
;;  have few possible completion candidates; others have many.
;;  Buffer-name completion, for example, typically involves relatively
;;  few candidates, whereas file-name completion typically involves
;;  many.
;;
;;  If there are many candidates matching your input, then many
;;  comparisons will be made each time the candidate set is sorted.
;;  This means that if your sort function is complex, response can be
;;  slow.  A complex sort function might be OK for sorting a small or
;;  medium set of candidates, but it might not be appropriate for
;;  sorting a very large set.
;;
;;  Only you, as a user, can control which sort makes the best sense
;;  for you in any given situation.  If you are likely to have
;;  zillions of candidates in some context, then you probably will
;;  want to change to a sort that computes quickly.  You can, of
;;  course, even choose not to sort at all, but simple sort
;;  comparisons do not noticeably impact performance, even for a very
;;  large number of candidates.
;;
;;  Icicles could offer a threshold option similar to
;;  `icicle-incremental-completion-threshold' (or it could reuse that
;;  option), and not bother to sort if the number of candidates passed
;;  the threshold, but there can be many sort orders of differing
;;  complexity, so a set of thresholds would really be needed, perhaps
;;  one per sort order.
;;
;;  Rather than having you try to manage such complexity ahead of time
;;  using options, it's better to just let you manage it at completion
;;  time: Choose the sort order with knowledge of the possible
;;  candidate set.  For example, if the set of candidates to sort will
;;  include every file on your file system, then you probably will
;;  want to use a simple sort.  On the other hand, there are
;;  situations where you might nevertheless prefer to wait a few
;;  seconds, in order to perform a complex sort that is of particular
;;  use.
;;
;;  In sum, Icicles keeps it simple, and leaves it up to you to choose
;;  the appropriate sort order for any given context.  This design
;;  choice is one reason why Icicles makes it easy to choose a sort
;;  even while you are completing input - each act of completion is
;;  different.
;;
;;  It can help you choose, however, to know which of the predefined
;;  Icicles sort orders are more complex, and therefore tend to be
;;  slower.  Here they are:
;;
;;    Sort Order                      Sort Function Used
;;    ----------                      ------------------
;;    by previous use alphabetically  `icicle-historical-alphabetic-p'
;;    by last use as input            `icicle-latest-input-first-p'
;;    by last use                     `icicle-latest-use-first-p'
;;
;;  The reason these sorts are slower is that they check the current
;;  minibuffer history, to see whether, and where, each candidate is
;;  located in the history list.  If you, like I, have very long
;;  history lists, then this can take a while.  I use histories of
;;  virtually unlimited length - I let library `savehist-20+.el' save
;;  all of my histories from one Emacs session to the next.
;;
;;  Here are some of the Icicles sort orders that exist by default:
;;
;;    - alphabetical - see
;;      (@> "Putting Previous Candidates First: `C-M-,'")
;;    - case-insensitive - (@> "Completion Status Indicators")
;;    - by last use as input
;;    - by previous use alphabetically - see
;;      (@> "Putting Previous Candidates First: `C-M-,'")
;;      by color name (colors) - see
;;      (@file :file-name "icicles-doc2.el" :to "Candidates with Text Properties")
;;      by hue (colors)
;;      by purity/saturation (colors)
;;      by brightness/value/luminance (colors)
;;      by all HSV components, in order (colors)
;;      by HSV distance from a base color (colors)
;;      by amount of red (colors)
;;      by amount of green (colors)
;;      by amount of blue (colors)
;;      by all RGB components, in order (colors)
;;      by RGB distance from a base color (colors)
;;   22 by key name, prefix keys first (keys)- see (@> "Completing Prefix Keys")
;;   22 by key name, local bindings first (keys)- see
;;      (@> "Local Bindings and Menu Items Are Highlighted")
;;   22 by command name (commands)
;;    - by abbrev frequency (commands) - see
;;      (@> "Multi `M-x' Turns Every Command into a Multi-Command")
;;      by buffer size (buffer names)
;;      *...* buffers last (buffer names)
;;      by major mode name (buffer names)
;;   22 by mode-line mode name (buffer names)
;;      by file/process name (buffer names)
;;    - by last file modification time (file names) - see
;;      (@> "Icicles Commands that Read File Names")
;;    - by file type (extension) (file names)
;;    - by directories first or last (file names)
;;    - in book order (Info) - see
;;      (@file :file-name "icicles-doc2.el" :to "Icicles Completion for Info")
;;    - special candidates first - see
;;      (@> "Local Bindings and Menu Items Are Highlighted"),
;;      (@file :file-name "icicles-doc2.el" :to "Candidates with Text Properties"),
;;      (@file :file-name "icicles-doc2.el" :to "Icicles OO: Object-Action Interaction")
;;    - proxy candidates first - see (> "*Completions* Display")
;;    - extra candidates first - see
;;      (@file :file-name "icicles-doc2.el" :to "Global Filters")
;;    - by second multi-completion part (multi-completions) - see
;;      (@> "Sorting Candidates by Their Second Part")
;;    - turned OFF  (does not sort at all)
;;
;;  As you can see, some are appropriate for color-name completion,
;;  some for buffer-name completion, and some for file-name
;;  completion.  Some are general, appropriate for most kinds of
;;  completion.
;;
;;  Those marked above with the label `22' can be used only with Emacs
;;  22 or later.  Those marked with a hyphen (-) are defined using
;;  `icicle-define-sort-command', so they correspond to explicit
;;  commands whose doc you can examine.  The command names in this
;;  case are `icicle-sort-' followed by the sort-order names (with
;;  hyphens substituted for spaces) - for example,
;;  `icicle-sort-by-directories-last' and `icicle-sort-turned-OFF'.
;;
;;(@* "Adding a Saved Sort Order")
;;  ** Adding a Saved Sort Order **
;;
;;  There are many predefined sort orders (see
;;  (@> "Different Sorts for Different Sorts of Uses")), and you can
;;  define your own new sort orders (see
;;  (@> "Defining New Sort Orders")).  This section is about a unique
;;  Icicles feature that lets you combine any number of sort orders
;;  interactively, melding them together.
;;
;;  You do this as follows:
;;
;;  1. Start with a given sort order (use `C-,' to choose one,
;;     possibly with a prefix argument).
;;
;;  2. Save the set of candidates you are interested in, using `C-M->'
;;     (see (@> "Saving and Retrieving Completion Candidates")).  This
;;     saves the candidates in their current order at the time of the
;;     save: the saved order.
;;
;;  3. Choose a different sort order (again, `C-,', possibly with a
;;     prefix argument).
;;
;;  4. Use `C-M-+' (`icicle-plus-saved-sort') to combine the two sort
;;     orders, that is, the (new) current order and the saved order.
;;
;;  What `icicle-plus-saved-sort' does is sum, for each completion
;;  candidate, its ranks (indexes) in the two sort orders, and then
;;  reorder candidates based on the summed ranks.
;;
;;  For example, if a given candidate is the 4th candidate in the
;;  current list of candidates, and it is the 7th candidate in the
;;  saved list of candidates, then its combined sort rank is 4 + 7 =
;;  11.  With a score of 11 it sorts after a candidate whose score is,
;;  for example, 6, and before a candidate whose score is, for
;;  example, 13.
;;
;;  The candidates are reordered according to the combined sort
;;  orders, forming a new current order.
;;
;;  When you use `C-M-+' it does not matter what order the saved
;;  candidates are in or what order you used to sort the current
;;  candidates.  (But you will generally want to use the same set of
;;  candidates.)  In particular, after using `C-M-+' the candidates
;;  are typically in an order that corresponds to no predefined sort -
;;  that's OK.
;;
;;  You can use `C-M-+' again if you like, to add in the saved sort
;;  order again with the new current order.  This gives the saved
;;  order more weight than the original current sort order.  Continued
;;  repetition of `C-M-+' gives the saved sort order more and more
;;  weight.  Eventually a fixed point is reached: `C-M-+' produces no
;;  further change in the order.
;;
;;  For example, consider `icicle-read-color-WYSIWYG'.  With user
;;  option `icicle-WYSIWYG-Completions-flag' non-`nil' (e.g. a string)
;;  it lets you see the effect of `C-M-+' in a striking, graphical
;;  way.  However, to see the effect you will first want to use
;;  `S-pause' (`icicle-toggle-highlight-saved-candidates') in the
;;  minibuffer to turn off highlighting of the saved candidates, since
;;  that highlighting obscures the color highlighting.  (You can use
;;  `C-S-pause' to toggle option `icicle-WYSIWYG-Completions-flag'.)
;;
;;  Sorting by color hue shows essentially a single rainbow of
;;  candidates in `*Completions*': pinks, followed by magentas,
;;  purples, blues, cyans, greens, yellows, browns, reds, and grays.
;;  Sorting by color brightness shows a single value gamut, the
;;  brightest colors followed by dimmer and dimmer colors, down to the
;;  dimmest (black).
;;
;;  Try `M-x icicle-read-color-WYSIWYG', sorting (`C-,', possibly with
;;  a prefix argument) first by hue.  Save the completion candidates
;;  (`C-M->').  Now sort by brightness (`C-,' again, possibly with a
;;  prefix argument).  Now use `C-M-+' to add/merge the two sort
;;  orders.  You now see essentially a series of rainbows, from
;;  brighter to dimmer and dimmer.
;;
;;  Use `C-M-+' again, to give hue more prominence in the merged sort
;;  order.  And again.  Keep hitting `C-M-+' until there is no more
;;  apparent change in the sort order - at this point you are back to
;;  a pure hue sort.
;;
;;  You can also at any time save the candidates again, saving the
;;  latest order as the new sort order.  Then you can reorder the
;;  current candidates using a different sort order (`C-,').  And then
;;  use `C-M-+' again to merge in the newly saved order.  You can play
;;  this way ad infinitem.
;;
;;
;;  See Also:
;;
;;  * (@file :file-name "icicles-doc2.el" :to "Customization and General Tips")
;;    for more about `icicle-buffer-sort' and other buffer-name
;;    completion parameters.
;;
;;  * (@file :file-name "icicles-doc2.el" :to "Global Filters") for a
;;    way to filter and sort the domain of discourse, that is, all
;;    possible candidates, prior to any use of completion.
 
;;(@* "A Propos d'Apropos")
;;
;;  A Propos d'Apropos
;;  ------------------
;;
;;  The Icicles doc speaks often of apropos this and apropos that.
;;  "Apropos" could almost be thought of as another name for Icicles.
;;
;;  Somewhat different things are meant by this term.  But they all
;;  stem from the traditional use in Emacs of the term to mean
;;  "about", and the traditional `apropos' commands that accept a
;;  regexp and give you information about symbols whose names match
;;  that regexp.
;;
;;  The most common mention of "apropos" in Icicles is in connection
;;  with "apropos completion" - see (@> "Apropos Completions"), which
;;  uses regexp matching.  It is the regexp matching that inspires the
;;  name.
;;
;;  This doc section describes ways in which Icicles gives you help
;;  apropos (about) something in Emacs.  All of these ways let you use
;;  regexp matching, but they also work with any other kind of
;;  matching.  That is, they provide information about things, and you
;;  can use them with apropos completion, but you can also use them
;;  with other kinds of completion.
;;
;;(@* "Get Help on Completion Candidates")
;;  ** Get Help on Completion Candidates **
;;
;;  General Icicles help is available at any time during minibuffer
;;  input, by hitting `M-?' (`icicle-minibuffer-help').  This section
;;  is instead about specific help for individual completion
;;  candidates.
;;
;;  When you cycle among completion candidates, help for the current
;;  candidate is shown in the mode-line, provided user option
;;  `icicle-help-in-mode-line-delay' is greater than zero.  This makes
;;  it easy to see what each candidate means or does.  Similarly, this
;;  help is shown whenever your input is completed entirely to one of
;;  the candidates.  If you use library `lacarte.el', then mode-line
;;  candidate help is even available for menu-item candidates.
;;
;;  Typically, this candidate mode-line help is the first line of the
;;  candidate's doc string, but alternative help sources can be used
;;  (and a doc string is not available for some kinds of candidates).
;;
;;  For some common object types the mode-line candidate help is a
;;  summary of useful information.  For a buffer-name candidate, for
;;  example, it includes the mode name, buffer size, and associated
;;  file or directory, if any.  The full directory name can be
;;  especially helpful for choosing among Dired buffers.
;;
;;  Note that you can sometimes improve performance by not displaying
;;  help in the mode line, that is, by setting
;;  `icicle-help-in-mode-line-delay' to zero.  This is because it
;;  takes time to compute and display some kinds of help in the
;;  mode-line.  In any case, a user event (e.g. a key press) always
;;  interrupts the mode-line display of help.  But it might not be
;;  obvious to you that you can interrupt help display this way.
;;
;;  To see more help than what is shown in the mode-line, for each
;;  candidate or any given candidate as you cycle, press and hold the
;;  Control and Meta keys while using the vertical arrow keys, for
;;  prefix completion, or the `prior' and `next' keys (often labeled
;;  Page Up and Page Down), for apropos completion.  To show help on
;;  any individual candidate, navigate to it (by cycling or using
;;  completion), and hit `C-M-RET' (`C-M-return') - or press Control
;;  and Meta and click it with `mouse-2' (`C-M-mouse-2') in buffer
;;  `*Completions*'.
;;
;;  For example, if you use standard command `switch-to-buffer' and
;;  you cycle among candidate buffer names, the buffer is described in
;;  buffer `*Help*' as the buffer name appears in the minibuffer.
;;  This includes descriptions of the major and minor modes.  If you
;;  use library `help-fns+.el' then it also includes the full name of
;;  the file or directory associated with the buffer (if any), the
;;  buffer size, the time it was last displayed, whether it has been
;;  modified, and whether it is read-only.
;;
;;  By default, you need not use the Meta key for candidate help; the
;;  same bindings work with just the Control key.  So, for example,
;;  you can click `C-mouse-2' to get help on a candidate or use
;;  `C-next' to cycle candidate help information.  However, Icicles
;;  multi-commands often have a different use for these bindings that
;;  do not include Meta.  It is only by default, when a multi-command
;;  has not bound a more specific action to the plain Control
;;  bindings, that you can use the sans-Meta bindings for help on
;;  candidates.
;;
;;  For example, Icicles binds `M-x', `C-x b', and `C-x C-f' to
;;  multi-commands that execute a command, switch to a buffer, and
;;  open a file, respectively.  If you use only the Control key,
;;  without the Meta key, when choosing candidates for these commands,
;;  you will not get help on the candidates; instead, you will execute
;;  a candidate command, switch to a candidate buffer, and open a
;;  candidate file, respectively.  For more information, see
;;  (@> "Multi-Commands").
;;
;;(@* "Use Candidate Help Like You Use Emacs Command `apropos'")
;;  *** Use Candidate Help Like You Use Emacs Command `apropos' ***
;;
;;  You can use this candidate-help functionality as a kind of
;;  expanded `apropos' functionality.  As an example, type `C-h v
;;  out', then type `S-TAB' to display all variables that match "out"
;;  (in buffer `*Completions*').  Then use `C-M-next' repeatedly to
;;  cycle among those variables, displaying their documentation in the
;;  `*Help*' buffer as they appear one by one in the minibuffer.  Or
;;  click individual variable names with `C-M-mouse-2', to display
;;  their documentation.
;;
;;  The standard `apropos' commands show only the first doc-string
;;  line.  Icicles shows that automatically in the mode-line, and it
;;  shows the complete doc string on demand when you use `C-M-'.  This
;;  can be handy, for instance, when you are unsure which of several
;;  similarly named candidates to choose.  Seeing a candidate's
;;  documentation along with its name can help you decide.
;;
;;  You can click links in buffer `*Help*' to look up more info, and
;;  then resume `C-M-next' where you left off, all without leaving
;;  completion.
;;
;;  This also works with menu items, if you load library `lacarte.el'
;;  as well as Icicles.  As you cycle among matching menu items, the
;;  corresponding command documentation is displayed in `*Help*'.
;;
;;  If you also use library `help-fns+.el' (Emacs 22 or later) or
;;  library `help+.el' (or `help+20.el' for Emacs 20), then you can
;;  use these Icicles features with additional help commands such as
;;  `describe-file' (`C-h M-f'), `describe-keymap' (`C-h M-k'),
;;  `describe-command' (`C-h C-c'), `describe-option' (`C-h o'), and
;;  `describe-option-of-type'.
;;
;;  `C-h C-o', which is bound by those libraries to command
;;  `describe-option-of-type', is bound in Icicle mode to
;;  `icicle-describe-option-of-type' instead, which is a multi-command
;;  that uses multi-completion and is therefore more powerful.  See
;;  (@> "Multi-Completions").  `C-h C-o' describes a user option that
;;  is of a particular custom type: you match the type and the option
;;  name at the same time (in parallel).
;;
;;  As an example, try `C-h C-o ici C-M-j string S-TAB' (`C-M-j' just
;;  separates the option name and type parts).  In buffer
;;  `*Completions*', you see all options whose name contains `ici' and
;;  whose type (or an inherited type) definition contains `string'.
;;  That means not only options that are strings, but options that are
;;  lists that contain string elements, options that can be a string
;;  or something else, and so on.
;;
;;  Browse the doc for these options, one after the other, using
;;  `C-M-next'.  This is a way to see, at the same time, the
;;  documentation for individual options (in buffer `*Help*') and the
;;  types their values must satisfy (in `*Completions*').
;;
;;  And remember that you can leave the option-name part or the type
;;  part empty, to see all options of a certain type or options of all
;;  types with a certain name pattern.  For example, `C-h C-o .* C-M-j
;;  string S-TAB' or `C-h C-o ici S-TAB'.
;;
;;  And you can of course use progressive completion as well, to
;;  match, say, a type that has a `string' component and an `integer'
;;  component - or whatever.  The type part of a completion candidate
;;  is an entire `defcustom' type, so its `:tag' values are also
;;  included.  This means that you can also match against the
;;  descriptive text (tag) that appears next to a value component in
;;  Customize.
;;
;;  `C-h C-o' is a powerful way to browse options and their
;;  documentation.  See the doc string of
;;  `icicle-describe-option-of-type' for more possibilities.  See
;;  also `icicle-customize-apropos-options-of-type'
;;  ((@> "Icicles Apropos Commands")), which is similar to
;;  `icicle-describe-option-of-type' but which opens a Customize
;;  buffer for all of the matching options when you hit `RET'.
;;
;;  Candidate help (prefix `C-M-') is available for these types of
;;  completion candidates, by default:
;;
;;  * menu items
;;  * commands and other functions
;;  * keymap variables (if `describe-keymap' is defined - see
;;    library `help-fns+.el')
;;  * user options and other variables
;;  * faces
;;  * fonts (mode-line help only)
;;  * command abbreviations (using `apropos-command' for matches)
;;  * property lists
;;  * buffers
;;  * files
;;  * bookmarks
;;
;;  Starting with Emacs 22, if you have command-line tool `exiftool'
;;  installed and in your `$PATH' or `exec-path', and if the candidate
;;  names either an image file or an image-file bookmark (see
;;  Bookmark+), then EXIF information (metadata) about the image is
;;  included.  See standard Emacs library `image-dired.el' for more
;;  information about `exiftool'.
;;
;;  If the same candidate names a function, a variable, and a face, or
;;  any two of these, then all such documentation is shown (Emacs 22
;;  and later).
;;
;;  In addition to the candidate types listed above, any command that
;;  uses completion can define its own candidate help action function
;;  and bind it to `icicle-candidate-help-fn'.
;;
;;  A command can also provide its own mode-line and tooltip help for
;;  any individual candidate.  See
;;  (@file :file-name "icicles-doc2.el" :to "Applying Text Properties to a Candidate String").
;;
;;  For more information about the types of candidates and their
;;  associated documentation, see the documentation for command
;;  `icicle-help-on-candidate'.  This command is bound to `C-M-RET'
;;  (`C-M-return'), `C-M-mouse-2', `C-help', `C-M-help', `C-f1', and
;;  `C-M-f1'.  When no specific action is defined for candidates, it
;;  is also bound to `C-RET' (`C-return') and `C-mouse-2'.  You can
;;  use this to get help on any completion candidate during
;;  completion.  See also the related help-cycling commands,
;;  `icicle-next-candidate-per-mode-help',
;;  `icicle-help-on-next-apropos-candidate', and so on, bound to
;;  `C-M-down', `C-M-up', `C-M-next', `C-M-prior', `C-M-end', and
;;  `C-M-home'.
;;
;;  (The documentation refers to the keys that provide candidate help
;;  as `C-M-RET' etc.  Actually, these are only the keys by default.
;;  You can customize them, using option
;;  `icicle-candidate-help-keys'.)
;;
;;  If you use one-buffer-per-frame (`pop-up-frames' non-`nil'), then
;;  displaying `*Help*' in one frame might interfere with viewing
;;  `*Completions*' in another.  For that reason, the `*Completions*'
;;  frame is raised to the front.  Also, if user option
;;  `icicle-move-Completions-frame' is non-`nil' then the
;;  `*Completions*' frame is moved to the edge of the display, out of
;;  the way, whenever you access help on a candidate.  (Default value:
;;  `right', meaning move it to the right).
;; 
;;(@* "Icicles Apropos Commands")
;;  ** Icicles Apropos Commands **
;;
;;  The commands described in this section can generally be thought of
;;  as extensions of the traditional Emacs apropos commands.  In
;;  addition to what each of them does with your chosen completion
;;  candidate or the current set of completion candidates, each of
;;  them provides the complete documentation for the current
;;  completion candidate as its candidate help (`C-M-RET',
;;  `C-M-mouse-2', and so on).
;;
;;(@* "Replacements for Standard Apropos Commands")
;;  *** Replacements for Standard Apropos Commands ***
;;
;;  By default (see option `icicle-top-level-key-bindings'), in Icicle
;;  mode the standard Emacs `apropos' commands are remapped to Icicles
;;  versions.  This section describes most of these commands.
;;  See (@> "Value-Aware Variable-Apropos Multi-Commands") for command
;;  `icicle-apropos-value', which replaces standard command
;;  `apropos-value'.
;;
;;  The other Icicles commands that replace standard apropos commands
;;  act similarly to the standard commands, but they also let you see
;;  the list of regexp matches incrementally (as with all Icicles
;;  commands), using `S-TAB'.  If you also use my library
;;  `apropos-fn+var.el', then these Icicles commands take advantage of
;;  the apropos enhancements provided by that library.
;;
;;  These are the Icicles apropos commands that standard commands are
;;  remapped to: `icicle-apropos', `icicle-apropos-command',
;;  `icicle-apropos-function', `icicle-apropos-option',
;;  `icicle-apropos-variable', and `icicle-apropos-zippy'.
;;
;;  Another feature of these Icicles commands is that when more than
;;  one type of object can be a candidate, candidates of a certain
;;  type are shown in buffer `*Completions*' using face
;;  `icicle-special-candidate'.  For example, command `icicle-apropos'
;;  shows function names as special candidates, to help you
;;  distinguish them from variable names.
;;
;;  Starting with Emacs 22, both the standard Emacs apropos commands
;;  and the Icicles versions let you input a set of words (really, any
;;  strings with no regexp special chars) separated by space chars, as
;;  an alternative to inputting a regexp.
;;
;;  However, Icicles progressive completion provides a more powerful
;;  way to search with multiple words (in fact, multiple regexps) -
;;  you can of course use it with the Icicles apropos commands.  Also,
;;  there are a few problems with the standard Emacs apropos commands,
;;  with respect to interpreting your input as either a set of
;;  keywords or a regexp.  Because they allow two very different
;;  syntaxes as input, the standard apropos commands are forced to
;;  make some limiting compromises for keyword searching.
;;
;;  See Also: (@> "Progressive Completion").
;;
;;(@* "Documentation-Apropos Multi-Commands")
;;  *** Documentation-Apropos Multi-Commands ***
;;
;;  Icicles multi-commands `icicle-doc', `icicle-fundoc', and
;;  `icicle-vardoc' provide the functionality of standard Emacs
;;  command `apropos-documentation', but with additional features -
;;  see (@> "Multi-Completions").  In addition, you can use
;;  multi-command `icicle-plist' to find symbols with certain
;;  property-list keys and values.
;;
;;  Each of those multi-commands uses multi-completion candidates.
;;  The candidates in each case are multi-part, with the first part
;;  being the name of the symbol and the last part being its
;;  documentation.  For `icicle-doc' there is a middle part, which is
;;  the symbol type (`FUNCTION', `VARIABLE', or `FACE').
;;
;;(@* "Type-Aware Variable-Apropos Multi-Commands")
;;  *** Type-Aware Variable-Apropos Multi-Commands ***
;;
;;  The following apropos multi-commands for user options are
;;  type-aware.  Like the documentation-apropos multi-commands, they
;;  use multi-completion candidates.  In this case the first part is
;;  the option name and the second part is its Customize type
;;  (`defcustom' type).
;;
;;  * `icicle-apropos-options-of-type'
;;  * `icicle-customize-apropos-options-of-type'
;;  * `icicle-describe-option-of-type' (bound to `C-h C-o')
;;
;;  They all work the same way.  They do something with all loaded
;;  user options of a the type that you specify.  The first one
;;  describes an individual option, showing its doc string.  The
;;  second opens an Apropos buffer for all matching options.  The
;;  third opens a Customize buffer for them.
;;
;;  These are very powerful commands, especially if you know a bit
;;  about `defcustom' `:type' specifications, but even if you do not.
;;  See the doc string of `icicle-describe-option-of-type' for more
;;  information, especially about using a prefix argument to determine
;;  how the second, `TYPE', part of the completion candidates is
;;  handled.
;;
;;(@* "Value-Aware Variable-Apropos Multi-Commands")
;;  *** Value-Aware Variable-Apropos Multi-Commands ***
;;
;;  The following apropos multi-commands for variables are
;;  value-aware.
;;
;;  For the commands that follow, you are prompted first for a
;;  predicate that restricts completion candidates to variables whose
;;  current value satisfies it.  For example, if you enter `integerp'
;;  as the predicate then candidates are variables whose value is an
;;  integer.
;;
;;  * `icicle-apropos-vars-w-val-satisfying'
;;  * `icicle-customize-apropos-opts-w-val-satisfying'
;;  * `icicle-describe-var-w-val-satisfying'
;;
;;  The predicate you enter is read as a Lisp sexp, but it is not
;;  evaluated.  It must be either a function symbol, such as
;;  `integerp', or a lambda form, such as `(lambda (val) (and
;;  (integerp val) (> val 500)))'.  Other than the fact that it must
;;  accept at least one argument (the value of a variable), it can be
;;  anything you like.  Individual candidate help (e.g., `C-M-RET')
;;  shows you the doc string and the current value.
;;
;;  Multi-command `icicle-apropos-value' is also value-aware.  It lets
;;  you match variables by name and value at the same time (in
;;  parallel): the completion candidates are multi-completions: the
;;  variable name plus its current value.
;;
;;  With a prefix argument you can use `icicle-apropos-value' to
;;  examine other Emacs objects and their associated information:
;;  symbols plus their plists, functions plus their definitions.
 
;;(@* "Multi-Commands")
;;
;;  Multi-Commands
;;  --------------
;;
;;(@* "What Is a Multi-Command?")
;;  ** What Is a Multi-Command? **
;;
;;  A multi-command is a command that lets you make multiple input
;;  choices in a single command execution: a multiple-choice command.
;;  You can choose multiple items from a set of choices, using buffer
;;  `*Completions*' as a multiple-choice "menu".  (It's not necessary
;;  to display `*Completions*', however.)  Instead of asking you
;;  "Which file do you want to delete?", a multi-command asks you, in
;;  effect, "Which file(S) do you want to delete?".
;;
;;  Nothing especially new here.  Any Emacs command could be defined
;;  to use an input loop, asking for file names until you do something
;;  to signal that you're done inputting.  It could provide for
;;  file-name completion by calling `read-file-name' to read your
;;  input.
;;
;;  * But what if you could also filter the domain of discourse on the
;;    fly, so that the candidate files were only those matching a
;;    regular expression (regexp) that you typed? Then, the command
;;    definition would need to provide for that behavior too.
;;
;;  * And what if you could then take the complement of that set of
;;    candidate file names, with respect to the complete set of files
;;    in the directory? Or subtract (exclude) some set of file names
;;    from the set of matching names, to get the set of possible
;;    choices?
;;
;;  * And what if the set of potential candidates at each step (regexp
;;    match, complement, set difference) could also be displayed in a
;;    multiple-choice menu?
;;
;;  For such multi-command functionality you need Icicles.
;;
;;  You can tell whether a command is a multi-command when you execute
;;  it: if it is a multi-command, then the prompt is prefixed by `+'.
;;  For example, multi-command `icicle-file' uses this prompt:
;;
;;    + File or directory:
;;
;;  Normal, non multi-command `find-file' uses this prompt, which has
;;  no `+':
;;
;;    Find file:
;;
;;  Just remember that `+' means that you can choose any number of
;;  inputs.
;;
;;(@* "How Does a Multi-Command Work?")
;;  ** How Does a Multi-Command Work? **
;;
;;  When an Icicles multi-command prompts you for input, you can make
;;  a single choice and press `RET' to confirm it, as usual, or you
;;  can choose any number of completion candidates, using `C-RET'
;;  (`C-return') or `C-mouse-2' for each.
;;
;;  (The documentation refers to the key that acts on a completion
;;  candidate as `C-RET'.  Actually, it is this only by default.  You
;;  can customize it, using option `icicle-candidate-action-keys'.)
;;
;;  You can thus act on multiple candidates, or even multiple times on
;;  the same candidate, during the same execution of the command.
;;
;;  But you do not have to - you can use any multi-command just as if
;;  it were a normal, single-choice command.
;;
;;  For example, command `icicle-delete-file' lets you delete a single
;;  file or a set of files that match your minibuffer input - all in
;;  the same command execution.  If you type no input, then all files
;;  in the current directory match, and you can delete any number of
;;  them individually.  If you type `~$' and hit `S-TAB'
;;  (`icicle-apropos-complete'), then all files that end in `~' match,
;;  and you can delete any number of them.  Similarly, command
;;  `icicle-buffer-other-window' lets you display any number of
;;  buffers, and so on.
;;
;;  You make multiple choices this way by cycling through the
;;  candidate completions, as usual, and hitting `C-RET' whenever you
;;  want to choose (act on) the current cycle candidate.  Or just
;;  press and hold Control while clicking each chosen candidate with
;;  `mouse-2'.
;;
;;  Similarly, you can use `C-down', `C-up', `C-next', `C-prior',
;;  `C-end', or `C-home' to act on successive candidates, forward or
;;  backward.  You can thus just hold down the Control key while
;;  cycling, to act on each candidate in turn, if you want.
;;
;;  Instead of, or in addition to, cycling, you can use completion to
;;  get to a particular candidate you want.  No matter how a candidate
;;  is made current, you can choose the current candidate (perform the
;;  action on it) using `C-RET' or `C-mouse-2'.
;;
;;  For lax (permissive) completion, you can use `C-RET' to act on
;;  *any* input text - you need not choose one of the available
;;  candidates.  This means, for example, that you can create any
;;  number of new file buffers with a single `C-x C-f' invocation, as
;;  well as open any number of existing files.
;;
;;  As always, hitting `RET' (or `S-RET') ends the command.  For most
;;  multi-commands, hitting `RET' performs the same action as `C-RET',
;;  but it is possible to have a command that acts differently for
;;  `RET' and `C-RET'.  That is the case, for instance, when help is
;;  displayed via `C-RET'.
;;
;;  You can use `C-RET' or `C-mouse-2' repeatedly to act multiple
;;  times on the same candidate.  A shortcut is to use `C-u' with
;;  `C-RET' or `C-mouse-2'.  That will work if the candidate action
;;  function is designed to be `C-u' sensitive.  This is the case for
;;  the Icicles multi-commands that read the name of a command or
;;  keyboard macro and execute the command or macro:
;;  `icicle-execute-extended-command' (`M-x'), `icicle-kmacro'
;;  (`S-f4'), and `icicle-execute-named-keyboard-macro' (`C-x M-e').
;;
;;  So for example, if you use `C-u 10 C-RET' on command
;;  `forward-char' during `M-x' command completion, the cursor
;;  advances 10 characters.  Another example: `C-x M-e C-u 200 C-RET'
;;  on a keyboard-macro candidate `foo' executes `foo' 200 times.  You
;;  can use all of the numeric prefix argument shortcuts, such as
;;  `M--', `M-7', and `C-6', with the exception of `C--', which has a
;;  different meaning (`icicle-candidate-set-difference') in the
;;  Icicles minibuffer.
;;
;;  Note that you can supply a prefix argument for both the
;;  multi-command and any of its individual actions.  The command is
;;  free to interpret these differently.  For example, a prefix
;;  argument for `icicle-kmacro' provides a default repeat factor,
;;  which can then be overridden for any individual action by
;;  providing a different prefix argument.
;;
;;  As another example, a prefix argument used with any completion
;;  candidate for command `icicle-find-file-no-search' visits the file
;;  in read-only mode.  But a prefix argument for the command itself
;;  reverses this effect: read-only becomes the default so that a
;;  prefix argument for a given candidate means that the visited file
;;  buffer is writable.
;;
;;  If user option `icicle-use-candidates-only-once-flag' is
;;  non-`nil', then, when you act on a candidate, it is removed from
;;  the list of available candidates, for clarity.  Commands where
;;  this behavior is appropriate bind this option to a non-`nil'
;;  value.  This is a user option, but you normally will not customize
;;  it.
;;
;;  You can use `C-g' to exit a multi-command at any time, without
;;  making a final choice using `RET'.  If the actions performed by a
;;  multi-command are easily reversible, `C-g' will often restore
;;  things to the way they were before performing the actions.
;;
;;  Does this `C-RET' stuff sound familiar?  Using a multi-command is
;;  similar to accessing help on a candidate
;;  (see (@> "Get Help on Completion Candidates")).  A multi-command
;;  is any command that has a special action defined for use with
;;  `C-RET' (command `icicle-candidate-action') on the current cycle
;;  candidate.  If no such special action is defined, then help on the
;;  candidate is displayed - displaying help is just the default
;;  action for `C-RET', used when no other action is defined.  You can
;;  always access candidate help using the `C-M-' prefix: `C-M-help',
;;  `C-M-f1', `C-M-RET', `C-M-mouse-2', `C-M-down', `C-M-up',
;;  `C-M-next', `C-M-prior', `C-M-end', and `C-M-home'.
;;
;;  You can also cycle among elements of a set, performing actions, if
;;  you use my libraries `doremi.el', `doremi-cmd.el', and
;;  `doremi-frm.el'.  Like Icicles, Do Re Mi lets you see the effect
;;  of a choice immediately, whenever you make changes.  Each library
;;  has its own advantages and special uses.  Advantages of Icicles
;;  include:
;;
;;    - completion to candidate values
;;    - restoration after making changes, letting you preview changes
;;      without actually applying them
;;
;;  See Also:
;;
;;  * (@> "More about Multi-Commands") for more about using
;;    multi-commands.
;;
;;  * (@file :file-name "icicles-doc2.el" :to "Defining Icicles Commands (Including Multi-Commands)")
;;    for how to define your own multi-commands.
;;
;;  * (@> "Moving Between the Minibuffer and Other Buffers").
 
;;(@* "Multi-Completions")
;;
;;  Multi-Completions
;;  -----------------
;;
;;  This section is about using completion candidates that are
;;  composed of more than one part: strings that you can complete
;;  against separately and simultaneously.
;;
;;(@* "Icicles Multi-Completion Commands")
;;  ** Icicles Multi-Completion Commands **
;;
;;  Have you ever used standard Emacs command `apropos-documentation'?
;;  It searches the doc strings of all Emacs-Lisp symbols for matches
;;  to an input regexp, and displays the hits.  It can be useful when
;;  you do not remember the name of a function or variable but you can
;;  guess at terms that might occur in its doc string.  Typically,
;;  people resort to it only after first trying apropos commands that
;;  match against the function or variable name.
;;
;;  The idea behind `apropos-documentation' also motivates Icicles
;;  command `icicle-doc'.  This is a multi-command (see
;;  (@> "Multi-Commands")), so you can use `C-RET' and `C-next' to
;;  browse the regexp matches, displaying the documentation of each
;;  match in turn, and you can change the regexp to get different
;;  matches during the same command invocation.
;;
;;  Like `apropos-documentation', `icicle-doc' lets you match a regexp
;;  against the doc strings of symbols such as functions, variables,
;;  and faces.  You can of course use progressive completion to match
;;  multiple regexps.  Here, for example, is a query that shows all
;;  doc strings (functions, variables, faces - a lot! of doc strings)
;;  that match both `mail' and `mode line', in either order, as
;;  completion candidates:
;;
;;    M-x icicle-doc RET mail S-SPC mode line
;;
;;  You cannot do that with vanilla Emacs `apropos-documentation' or
;;  with any other vanilla Emacs `apropos*' command.
;;
;;  Commands `icicle-vardoc', `icicle-fundoc', and `icicle-plist' are
;;  similar to `icicle-doc' in the kind of functionality they provide.
;;
;;  Each of these commands gathers a tremendous amount of information
;;  to construct an initial set of completion candidates based on all
;;  existing Emacs symbols (in the standard `obarray').  This is
;;  time-consuming.  Since the set of symbols that have property lists
;;  or that name functions, variables, or faces changes little, you
;;  can optionally bypass this gathering and reuse the last initial
;;  set of candidates for that command.  You do this by invoking the
;;  command using a prefix argument (non-negative prefix argument, for
;;  `icicle-vardoc' and `icicle-plist').
;;
;;  Each of these particular commands also uses Icicles
;;  multi-completion.  A "multi-completion" is a completion candidate
;;  that has multiple parts.  A multi-completion command lets your
;;  input match any or all parts individually, at the same time (in
;;  parallel).
;;
;;  With commands that provide multi-completion candidates, you use
;;  apropos completion (`S-TAB'), not prefix completion (`TAB'), at
;;  least whenever your input pattern tries to match other than just
;;  the first multi-completion part.
;;
;;  * Commands `icicle-vardoc' and `icicle-fundoc' let you match both
;;    the function or variable name and the doc string.
;;
;;  * Command `icicle-doc' lets you match any combination of the
;;    following:
;;
;;    - the function, variable, or face name
;;    - the type: FUNCTION, VARIABLE, or FACE (uppercase)
;;    - the doc string
;;
;;  * Command `icicle-plist' lets you match both a symbol name and its
;;    property list.  You can use it to find symbols with certain
;;    property-list keys or values.  By default (for Emacs 22 or
;;    later), plists are pretty-printed (in `*Help*' and
;;    `*Completions*'), but if you use a negative prefix argument then
;;    pretty-printing is skipped, gaining a little time.
;;
;;  * Commands `icicle-describe-option-of-type' (bound to `C-h C-o' in
;;    Icicle mode), `icicle-apropos-options-of-type', and
;;    `icicle-customize-apropos-options-of-type' each let you match
;;    both an option name and the option's `defcustom' type
;;    definition.  There are several ways to match type definitions,
;;    controlled by the prefix argument.  You can match a type
;;    directly or taking type inheritance into account.  You can match
;;    a type as a sexp or using a regexp.  You can match a type
;;    expression or match the option's current value against a type.
;;    See (@> "Type-Aware Variable-Apropos Multi-Commands").
;;
;;  Icicles file-name commands that use multi-completion include
;;  `icicle-locate-file', `icicle-locate-file-other-window',
;;  `icicle-recent-file', and `icicle-recent-file-other-window'.
;;  These commands let you match against multi-completion candidates
;;  that have an absolute file name part and a part that is the file's
;;  last modification date.  This means that you can easily find those
;;  notes you took sometime last week...
;;
;;  The way multi-completion commands work is a bit inelegant perhaps,
;;  and it can take a little getting used to, but it is quite powerful
;;  and lets you do things with completion that are otherwise
;;  impossible.
;;
;;  In the case of commands that use two-part multi-completions, you
;;  provide two patterns as input, one to match the name of a symbol
;;  (e.g. a function or variable) and one to match some associated
;;  information (doc string, property list, or type definition).
;;
;;  However, since completion candidates are not actually multipart,
;;  you in fact type a single pattern that is the concatenation of the
;;  two.  You join these two regexps using `icicle-list-join-string'
;;  (a user option), which, by default, is `^G^J', that is, a
;;  control-G character followed by a control-J (newline) character.
;;  As always, you can input control characters using `C-q', so to
;;  input `^G^J' you can use `C-q C-g C-q C-j'.
;;
;;  However, in Icicles, `C-j' (newline) is self-inserting during
;;  completion, so you do not need to quote it with `C-q' - you can
;;  use just `C-q C-g C-j'.  Better yet, you can use `C-M-j'
;;  (`icicle-insert-list-join-string') to insert `^G^J'.
;;
;;  This has the added benefit (in Emacs 22 or later) of hiding the
;;  `^G' - it's there, but you do not see it.  This hiding is only
;;  cosmetic; you still match the characters `^G' and `^J'.  In the
;;  same way, Icicles hides the `^G' part of `^G^J' in
;;  `*Completions*', so the join string appears as a newline
;;  character.
;;
;;  This hiding of `^G' happens only when option
;;  `icicle-list-join-string' has its (ugly but useful) default value.
;;  If not seeing the join string confuses you and you would prefer to
;;  distinguish multi-completion part separators from ordinary newline
;;  characters, then customize `icicle-list-join-string' - just remove
;;  the following from the Lisp sexp that defines the default value:
;;
;;    (set-text-properties 0 1 '(display "") strg)
;;
;;  As an example of using a multi-completion command, you can use the
;;  following to match a function name that contains `dired' and its
;;  doc string that contains `file':
;;
;;    M-x icicle-fundoc dired^G^Jfile S-TAB
;;
;;  That is, you type this:
;;
;;    M-x icicle-fundoc dired C-q C-g C-j file S-TAB
;;
;;  or this:
;;
;;    M-x icicle-fundoc dired C-M-j file S-TAB
;;
;;  Well, almost.  The way it actually works is that the completion
;;  candidates are themselves formed by concatenating symbol names
;;  with their doc strings, using `icicle-list-join-string'.  Your
;;  input regexp is matched against those candidates.  This means that
;;  the input regexp `dired^G^Jfile' would actually match only
;;  function names that *end* with `dired' and doc strings that
;;  *begin* with `file'.
;;
;;  To match `file' against any part of the doc string, you must
;;  explicitly link the two component regexps with a regexp that
;;  matches anything.  If you want to search only the first lines of
;;  doc strings, you can use `.*' to do that: `dired.*^G^J.*file' will
;;  match all functions whose names contain `dired' and whose doc
;;  strings' first lines contain `file'.
;;
;;  Why only the first lines?  Because `.' matches any character
;;  except a newline - it does not look past the first line.  If you
;;  want to search the entire doc strings (or property lists, for
;;  `icicle-plist'), then you need to use a connecting regexp that
;;  matches any character, including a newline.  That means a regexp
;;  such as `\(.\|\n\)'.  Or you can just use the Icicles multi-line
;;  dot feature - see (@> "Dot, Dot, Dot").
;;
;;  Without a multi-line dot, you would use something like this to
;;  search whole, multi-line doc strings for `file':
;;
;;    M-x icicle-fundoc dired.*^G^J\(.\|\n\)*file S-TAB
;;
;;  That is, you would type (without the spaces):
;;
;;    M-x icicle-fundoc dired.* C-M-j \ ( . \ | C-j \ ) * file S-TAB
;;
;;  With a multi-line dot, you would type just this:
;;
;;    M-x icicle-fundoc dired.* C-M-j . * file S-TAB
;;
;;  What if you want to match, say, `file' in either the function name
;;  or the doc string, not necessarily both?  Remember that a
;;  multi-completion is in fact a single string, with a separator such
;;  as `^G^J' in the middle somewhere.  Because it is a single string,
;;  the simple minibuffer input `file' matches the substring `file'
;;  anywhere in the multi-completion.  So the answer is just this:
;;
;;    M-x icicle-fundoc file S-TAB
;;
;;  Even this simple command expression combines the effect of Emacs
;;  commands `apropos-function' with that of `apropos-documentation'.
;;
;;(@* "Mode-Line Lighter Indication of Multi-Completion")
;;  ** Mode-Line Lighter Indication of Multi-Completion **
;;
;;  Whenever multi-completion candidates are available, the `Icy'
;;  minor-mode lighter has the suffix `||' (think "parallel") appended
;;  to it.  You can see this lighter at the left of the mode line of
;;  buffer `*Completions*' or in the minor-mode part of other mode
;;  lines.
;;
;;(@* "How Multi-Completions Work")
;;  ** How Multi-Completions Work **
;;
;;  These commands that accept a multipart regexp are examples of
;;  Icicles multi-completion.  Icicles extends standard function
;;  `completing-read' so that it will accept, as the set of completion
;;  candidates, an alist argument whose candidates are not only
;;  individual strings but can also be lists of strings.  Each string
;;  in the list is one part of a multipart completion candidate, that
;;  is, a multi-completion.  The strings are joined together pairwise
;;  using `icicle-list-join-string' by `completing-read'.  Commands
;;  `icicle-fundoc' and`icicle-vardoc' each use lists of two strings
;;  (name and doc), but a multi-completion can have any number of
;;  strings.
;;
;;  Why is the default value of `icicle-list-join-string' so odd:
;;  `^G^J'?  You can use any string you like, but here is the
;;  rationale behind the default choice:
;;
;;  - ^G does not normally occur in simple strings such as doc strings
;;  - a newline (^J) visually separates the multiple component strings
;;  - ^G^J is not too difficult to enter: `C-M-j' or `C-q C-g C-j'
;;
;;  It is important that the value of `icicle-list-join-string' not be
;;  something that is, itself, likely to match any of the candidates.
;;  Otherwise, it would not serve its role as separator.
;;
;;  I find that it helps a bit (in Emacs 22 or later) to customize
;;  face `escape-glyph', which is used for control characters such as
;;  `^G', in such a way that it stands out a bit, especially because
;;  control characters can be used in regexps that also use `^' as a
;;  special character.  I use an orange background with a blue
;;  foreground for this face.
;;
;;(@* "Multi-Completions Let You Match Multiple Things in Parallel")
;;  ** Multi-Completions Let You Match Multiple Things in Parallel **
;;
;;  Consider the command `describe-option-of-type', defined in my
;;  library `help-fns+.el' (or `help+.el', for Emacs 20).  This lets
;;  you first pick a `defcustom' type using completion and then pick
;;  an option of that type to describe.  There are two separate,
;;  sequential acts of completion.  For each completion act, your
;;  current input defines a set of matches.  You can see all option
;;  types that match, say, the regexp `.*string', which means all
;;  types that contain `string'.  After you choose one of those types,
;;  you can see all options of that type whose names start with
;;  `icicle' and then pick one.
;;
;;  You can thus tweak the type regexp to filter types, and you can
;;  tweak the name regexp to filter option names.  And you can of
;;  course use progressive completion to whittle down either set of
;;  matches, piecemeal.
;;
;;  What you cannot do, however, using `describe-option-of-type' is
;;  filter both sets at the same time: narrow down the set of type
;;  matches and name matches simultaneously.  For that, you need
;;  Icicles multi-completion.  Without it, you must commit 100% to a
;;  type before you can choose among the options of that type.  With
;;  it, you can change the type (or the name) part of your input
;;  regexp on the fly, and see immediately the set of matching names
;;  (or types) as well.
;;
;;(@* "Multi-Completions vs `completing-read-multiple'")
;;  ** Multi-Completions vs `completing-read-multiple' **
;;
;;  Note that there is (only) a superficial similarity between Icicles
;;  multi-completion and the functionality provided by function
;;  `completing-read-multiple' of standard Emacs library `crm.el'.
;;  The latter lets you complete multiple strings in the minibuffer,
;;  one at a time.  It involves only vanilla Emacs completion, and it
;;  uses the same set of completion candidates for each of the strings
;;  in the input.
;;
;;  By contrast, Icicles multi-completion completes each part of your
;;  input against a different set of completion candidates.  For
;;  example, when you use `icicle-vardoc', it completes the
;;  variable-name part of your input against the names of defined
;;  variables, and the variable-description part against the doc
;;  strings of defined variables.  Standard Emacs command
;;  `completing-read-multiple' lets you complete several different
;;  variable names at the same minibuffer prompt, but they each
;;  complete against the same set of variable names.
;;
;;  Multi-completion matches a list of regexps in parallel.  See also
;;  the descriptions of `M-*' and `S-SPC', which match a list of
;;  regexps in series: (@> "Progressive Completion").  You can combine
;;  these features, of course.
;;
;;(@* "Sorting Candidates by Their Second Part")
;;  ** Sorting Candidates by Their Second Part **
;;
;;  Most multi-completions have two parts.  Typically, the first part
;;  is the main part, that is, the part that you will most frequently
;;  complete against.  Many candidate sort orders involve some flavor
;;  of alphabetic order, and this means alphabetizing first with
;;  respect to the first multi-completion part.
;;
;;  However, it can sometimes be convenient to sort instead by the
;;  second part first.  That is what the Icicles sort order "by 2nd
;;  parts alphabetically" is for.  You can use it, for example, with
;;  command `icicle-locate-file' to sort file-name candidates first by
;;  date, and then by file-name for the same date.  This gives you an
;;  easy way to look up files that you modified during a given time
;;  period.  For example, your input regexp can limit candidates to
;;  those files last modified sometime in July, 2008, and you can then
;;  access these chronologically (by cycling or in buffer
;;  `*Completions*').  And do not forget that you can always reverse
;;  the current sort order, using `C-N C-,' where N is an integer.
;;
;;(@* "Multi-Completions with a Part You Never See")
;;  ** Multi-Completions with a Part You Never See
;;
;;  Some multi-commands, such as `icicle-buffer' (`C-x b'), and
;;  `icicle-file' (`C-x C-f'), `icicle-visit-marked-file-of-content'
;;  (`C-F' in Dired), and `icicle-Info-goto-node' (`g' in Info), do
;;  not show the second part of multi-completion candidates in
;;  `*Completions*', because it would just be distracting.  The point
;;  of such commands is to access a named container (buffer, file, or
;;  node), and the optional second multi-completion part matches the
;;  entire container content (the buffer, file, or node text).
;;
;;  See (@> "Chapter & Verse: Searching Named Containers") for more
;;  about this.
;;
;;  See Also:
;;
;;  * (@> "Multi-Commands")
;;  * (@> "Chapter & Verse: Searching Named Containers") and 
;;    (@> "Match File Names and File Content Too") for information
;;    about commands that find containers such as files, buffers, and
;;    Info nodes by name and by content
;;  * (@> "Programming Multi-Completions") for information about
;;    changing the appearance and behavior of Icicles
;;    multi-completions using Emacs-Lisp code.
;;  * (@> "Sorting Candidates and Removing Duplicates")
;;  * (@> "Progressive Completion")
 
;;(@* "Chapter & Verse: Searching Named Containers")
;;
;;  Chapter & Verse: Searching Named Containers
;;  -------------------------------------------
;;
;;  Some Icicles multi-commands, such as `icicle-buffer' (`C-x b'),
;;  `icicle-file' (`C-x C-f'), `icicle-visit-marked-file-of-content'
;;  (`C-F' in Dired), and `icicle-Info-goto-node' (`g' in Info),
;;  access containers of text (buffer, file, Info node) by name.
;;
;;  When this is the case, Icicles provides multi-completion
;;  candidates, the first part being the container name and the second
;;  part being the entire content of the container with that name (the
;;  buffer, file, or node text).
;;
;;  You can think of these as "chapter-&-verse" commands: you name a
;;  chapter or you search for a verse among chapters.  Or you do both
;;  at the same time, searching particular chapters for particular
;;  verses.
;;
;;  The second multi-completion part (the verse, or content) is never
;;  shown in `*Completions*', and you can ignore it altogether if you
;;  want.  If you do not use `C-M-j' followed by a content-matching
;;  pattern, then no content-searching occurs, which is of course
;;  quicker than searching lots of text.
;;
;;  But if the container (chapter) names are not enough to help you
;;  locate some information, then you can search the content instead.
;;  Or you can match both at the same time: one pattern for the name
;;  and another for the content.  Each pattern is optional.
;;
;;  When you search using a chapter-&-verse command, all you care
;;  about is whether the content matches your input.  Icicles looks
;;  for only one content match in a given container.  In other words,
;;  the second part of your input just filters candidates, retaining
;;  only those containers whose contents match.
;;
;;  This is a powerful and unique way to search.  Of course there are
;;  commands, such as `grep', that act similarly, but they are
;;  generally not incremental: letting you change name and/or content
;;  pattern on the fly as you see the corresponding matches.
;;
;;  Icicles search (e.g., `C-c `') has some similarities.  It lets you
;;  first choose a set of files or buffers to search, then it shows
;;  you the search hits as you change your content-matching pattern.
;;  But with Icicles search you cannot change the set of containers to
;;  search incrementally: you choose them at the outset once and for
;;  all, before you start to search.

;;  For Icicles search commands the emphasis is thus on the content
;;  search.  For Icicles chapter-&-verse commands the emphasis is not
;;  on searching; the aim is just to identify and go to a given
;;  container.  Searching is just one way to find the right container.
;;
;;  Chapter-&-verse commands access a container in the same way,
;;  whether or not they search it contents.  They do not move to any
;;  search-hit location.  For instance, `icicle-file' simply visits
;;  the chosen file, just as `find-file' would.
;;
;;  But you can search the container after you visit it, using `C-M-s'
;;  or `C-M-r' (regexp Isearch).  Whenever a content search is
;;  successful the content search pattern is added to
;;  `regexp-search-ring' when you hit `S-TAB'.  So when the
;;  chapter-&-verse command is finished you can immediately search for
;;  content matches incrementally (`C-M-s').
;;
;;  (You can also work in the other direction, reusing an Isearch
;;  regexp as a content-matching regexp.  See
;;  (@> "Using Completion to Insert Previous Inputs: `M-o'").)
;;
;;  This on-the-fly, on-demand content-matching can be quite handy.
;;  But as a general rule it is a good idea to first narrow down the
;;  set of candidates by matching container names before you set off
;;  to also search content.  Otherwise, if you have many or large
;;  containers with no content matches then you will waste time
;;  searching unnecessarily.  It is also generally a good idea to
;;  temporarily turn off Icicles incremental completion and Icomplete
;;  mode when you search container content.
;;
;;  See Also:
;;
;;  * (@> "Multi-Completions")
;;  * (@> "Match File Names and File Content Too")
;;  * (@> "Content-Matching Pattern as Isearch Regexp")
 
;;(@* "Dot, Dot, Dot")
;;
;;  Dot, Dot, Dot
;;  -------------
;;
;;  This section is about dot, that is, `.', and its role as a regexp
;;  special character in apropos completion.
;;
;;  Since the inception of regular-expression matching, `.' has
;;  matched any character *except* a newline character (aka `^J', aka
;;  `C-j').  Recent languages typically have an additional mode in
;;  which `.' can match any character, including a newline.  See, for
;;  example, http://www.regular-expressions.info/dot.html and this
;;  language comparison for regexp features:
;;  http://www.regular-expressions.info/refflavors.html.
;;
;;  It is not unusual to manipulate multi-line completion candidates
;;  in Icicles, in which case it can be handy to let `.' match any
;;  character, including a newline.  For this and more general
;;  reasons, I long ago requested such a mode for Emacs, but there was
;;  little interest in implementing it.  In Emacs, dot never matches a
;;  newline.  Too bad.
;;
;;  The regexp `\(.\|[\n]\)' is good enough, of course: it matches any
;;  character: any character any except newline, plus newline.  But it
;;  is a bit unwieldly, especially when used within a larger regexp,
;;  and especially if used more than once in the same regexp.
;;  Interactively, you input the `\n' using `C-j', and it appears in
;;  the minibuffer as a newline character; that is, it creates another
;;  line of input.
;;
;;  For convenience in multi-line matching, I added a *multi-line
;;  dot*, or dot-matches-newline-too, hack to Icicles.  This feature
;;  is turned off, by default.  You can toggle it on/off, using
;;  command `icicle-toggle-dot' (aka `icicle-toggle-.'), which is
;;  bound to `C-M-.' in the minibuffer during completion.
;;
;;  When this is turned on, `.' is highlighted in the minibuffer
;;  (using face `highlight'), and it matches newlines also.  In fact,
;;  although it appears as just a highlighted dot, the ugly regexp
;;  `\(.\|[\n]\)' (the value of constant `icicle-anychar-regexp') is
;;  really used, under the covers.  Icicles takes care of things so
;;  that you can edit normally (delete and transpose characters,
;;  etc.): A multi-line `.' acts just like a normal, single character,
;;  even though it is really a string of ten characters.
;;
;;  If you prefer to see the full regexp, `\(.\|[\n]\)', but
;;  highlighted, then set option `icicle-dot-show-regexp-flag' to
;;  non-`nil'.  (In Emacs 20, the newline-matching dot is always shown
;;  as that full regexp.)  And remember that you can use multi-command
;;  `icicle-toggle-option' anytime (bound to `M-i M-i' during
;;  completion) to toggle the option.  If you prefer to turn on
;;  newline matching by default, then just customize option
;;  `icicle-dot-string'.
;;
;;  This match-anything dot is handy, but sometimes you might want to
;;  match anything except a newline, perhaps in the same input pattern
;;  where you also want to match any character (possibly a newline) at
;;  other positions.  How can you get the plain dot behavior, when
;;  multi-line dot is turned on?
;;
;;  One way is just to use a regexp that matches anything except
;;  newline: `[^\n]' (which you input using `[ ^ C-j ]').  Another way
;;  is to use a plain prefix argument: `C-u .'.  (A numeric prefix
;;  argument N inserts N multi-line dots, each of which matches any
;;  single character.)
;;
;;  `C-u' flips the behavior of `.' when you hit it: If by default `.'
;;  enters a multi-line dot, then `C-u .' enters a plain dot.  If by
;;  default `.' enters a plain dot, then `C-u .' enters a multi-line
;;  dot.  So `C-u' also gives you a way to enter a one-off multi-line
;;  dot, if you prefer to generally have `.' not match a newline.
;;  Either way, what you see in the minibuffer is the single character
;;  `.', highlighted if it is a multi-line dot, unhighlighted if it is
;;  a plain dot.
;;
;;  Multi-line dots are converted to plain dots automatically when you
;;  use prefix completion.  And if you then move back to apropos
;;  completion during the same completion operation, you get back any
;;  multi-line dots you had before, and any plain dots that you
;;  entered before remain plain.
;;
;;  So when is a multi-line dot useful?  Whenever you want to match
;;  against multi-line candidates.  Typical use cases include
;;  `icicle-search' and the Icicles doc commands, `icicle-vardoc',
;;  `icicle-fundoc', and `icicle-doc'.
;;
;;  Note that the dot-matching behavior described here applies only to
;;  matching minibuffer input against completion candidates.  It does
;;  not mean that whenever you type `.' in the minibuffer it is
;;  interpreted specially.  For example, when you input (using `RET')
;;  a regexp as the context pattern for Icicles search, a `.'  has its
;;  usual meaning in Emacs regexps - it does not match newlines.
;;
;;  If you want a regexp that you input (using `RET') to match any
;;  character including a newline, then you can use `C-u C-=
;;  icicle-anychar-regexp' to insert the proper string explicitly.
;;  You can shorten this to just `C-=' if you use command
;;  `icicle-save-string-to-variable':
;;
;;   M-x icicle-save-string-to-variable C-u C-= icicle-anychar-regexp
;;
;;  See Also:
;;
;;  * (@file :file-name "icicles-doc2.el" :to "Using Regexps with Icicles Search")
;;  * (@> "Inserting a Regexp from a Variable or Register")
 
;;(@* "More about Multi-Commands")
;;
;;  More about Multi-Commands
;;  -------------------------
;;
;;  A multi-command is any command that uses input completion and lets
;;  you perform actions on any number of individual completion
;;  candidates without exiting completion.  See (@> "Multi-Commands").
;;
;;  The default action is invoked on the current candidate by `C-RET'
;;  (`icicle-candidate-action').  There are three other kinds of
;;  actions on individual candidates:
;;
;;  * alternative actions, invoked by `C-S-RET' (`C-S-return')
;;  * deletion actions, invoked by `S-delete'
;;  * help actions, invoked by `C-M-RET' (`C-M-return')
;;
;;  A given command can define any combination of these four kinds of
;;  actions: none of them, any one of them, any two of them, any three
;;  of them, or all four kinds.
;;
;;  This section provides information about alternative actions and
;;  deletion actions.
;;
;;  See Also:
;;
;;  * (@> "Get Help on Completion Candidates") for information about
;;    using candidate help.
;;  * (@file :file-name "icicles-doc2.el" :to "Defining Multi-Commands the Hard Way")
;;    for information about defining a custom candidate-help action
;;    for a command.
;;
;;(@* "Alternative Actions")
;;  ** Alternative Actions **
;;
;;  Just as you can use `C-RET', `C-mouse-2', `C-next', and so on to
;;  invoke a command's default action on multiple completion
;;  candidates individually, so you can use `C-S-RET'
;;  (`icicle-candidate-alt-action'), `C-S-mouse-2', `C-S-next', and so
;;  on to invoke an alternative action that is associated with the
;;  command.  If the main action of a command `my-find-file' is to
;;  visit a file, and the alternative action is to print a file, then
;;  you can use `C-S-RET' to print one or more files on the fly, even
;;  as you are completing the name of a file to be visited.
;;
;;  Keys `C-|' and `M-|' apply the alternative action defined for a
;;  given multi-command to *all* matching candidates at once.  (In the
;;  same way, `C-!' and `M-!' apply the main action defined for it to
;;  all candidates - see (@> "Choose All Completion Candidates").)
;;
;;  For example, in Icicles search (e.g. `C-c `'), the alternative
;;  action (e.g. `C-S-RET') replaces all or part of the current search
;;  hit, and `M-|' does the same for all search hits.
;;
;;  It is the particular command that defines its alternative action.
;;  Some commands define no such action.  Some commands, as their
;;  alternative action, prompt you to choose (using completion) a
;;  function to be applied to the current completion candidate.  In
;;  this case, a single alternative action effectively provides a set
;;  of possible actions.
;;
;;  To achieve this, such commands use the value of user option
;;  `icicle-type-actions-alist', which associates lists of possible
;;  functions with specific candidate types.  For example, for
;;  file-name candidates, you can choose among functions that act on
;;  file names.
;;
;;  Choosing such a function to apply is itself a multi-command
;;  operation.  You can thus apply any number of functions to any
;;  number of candidates.
;;
;;  For example, while you are using `C-x C-f', you can, say, print
;;  one or more candidate files on the fly, or invoke a shell command
;;  on selected files, or byte-compile them...  This is a particularly
;;  handy feature, especially if you customize
;;  `icicle-type-actions-alist' for your own particular use.
;;
;;  Some such functions you can choose produce no side effect; they
;;  simply return a value.  But if you use `C-u' before `C-S-RET',
;;  then the result of applying the function is pretty-printed (in the
;;  echo area or buffer `*Pp Eval Output*').  For example, if you use
;;  `C-x C-f', you hit `C-u C-S-RET' on the candidate file name
;;  `icicles-doc1.el', and you choose the function candidate
;;  `file-attributes' at the completion prompt `How (action): ', then
;;  the properties of the candidate file (`icicles-doc1.el') are
;;  displayed.  With just `C-S-RET' (no prefix argument), the list of
;;  properties is computed, but not displayed.
;;
;;  Be aware of this gotcha: The alternative action for commands that
;;  use `icicle-type-actions-alist' prompts for a function.  If you
;;  want to apply that function to all current completion candidates,
;;  then you must use `M-|', not `C-|', because `C-|' prompts you for
;;  each candidate.  `M-|' is designed to do the right thing here: it
;;  prompts you once for the function to apply, and then applies it to
;;  each of the current candidates.  And you can filter the set of
;;  current candidates (progressive completion and so on), or retrieve
;;  a saved set of candidates to operate on.
;;
;;  Note that completion while you choose a function to apply is lax.
;;  This means that you can really enter any function, including a
;;  lambda expression that you invent on the fly.  Of course, the
;;  function must accept an object of the appropriate type, (but it
;;  need not actually use that argument).
;;
;;  Using a lambda expression here is a good way to curry a function
;;  that requires multiple arguments, so that it adapts to expect just
;;  a single argument of the appropriate object type.  For example,
;;  (lambda (sym-name) (get (intern sym-name) 'invisible)) transforms
;;  function `get', which takes a symbol and a property as arguments,
;;  to a function that takes a symbol name and looks up the
;;  `invisible' property of the symbol.
;;
;;  Option `icicle-type-actions-alist' is predefined with a number of
;;  candidate types (buffer, color, command, face, file, frame,
;;  function, option, process, variable, and window) and their
;;  associated action functions, but you can add new types or modify
;;  their associated function lists.  Any Emacs-Lisp functions can be
;;  used, including lambda expressions.  But each function must accept
;;  a value of the appropriate type as its sole required argument
;;  (additional, optional arguments are OK).
;;
;;  Sometimes, you might want to define your own alternative action
;;  function for some command.  Do this if you always want the same
;;  alternative action, and it is not the predefined one.  To do this,
;;  just customize option `icicle-alternative-actions-alist'.  The
;;  associations defined by this option always override any predefined
;;  alternative actions for the corresponding commands.
;;
;;  An alternative to using option `icicle-alternative-actions-alist'
;;  is to define a new command, wrapping an existing command with a
;;  `let' binding that defines the action you want.  I recommend using
;;  the option instead, but you might sometimes prefer this approach.
;;  For example:
;;
;;  (defun my-icicle-find-file (f)
;;    "`icicle-find-file', but with `w32-browser' as the alt action."
;;    (interactive
;;      (let ((icicle-candidate-alt-action-fn  'w32-browser))
;;        (list (read-file-name "File: "))))
;;    (icicle-find-file f))
;;
;;  If you are familiar with Icicles object-action commands (see
;;  (@file :file-name "icicles-doc2.el" :to "Icicles OO: Object-Action Interaction")),
;;  then this idea of choosing an object (completion candidate) and
;;  then choosing a function to act on it will ring a bell.  And just
;;  as for the object-action commands, here too Icicles exploits any
;;  object-action associations ("sources" and "types") defined by
;;  library Anything (`anything.el'), if you happen to use that, in
;;  addition to the associations defined by
;;  `icicle-type-actions-alist'.  And when you do use the Icicles
;;  object-action commands, the same behavior is available as for
;;  alternative actions.
;;
;;  You might have noticed, above, that a type/actions association is
;;  predefined for type `function'.  Since the actions you can choose
;;  are themselves functions, you can even use `C-S-RET' on one of
;;  them, to apply a function-for-functions (e.g. `find-function' or
;;  `symbol-function') to it.  This is a curiosity, but it can
;;  sometimes be useful.
;;
;;  Finally, note that the completion candidate to which you apply an
;;  alternative action is in most cases a string.  In some cases, the
;;  alternative action functions expect a non-string object, and they
;;  will raise an error if applied to a string.
;;
;;  Icicles takes care of this in the case of buffer-name candidates.
;;  It assumes that you really want to operate on a buffer, not its
;;  name (a string), so it automatically calls `get-buffer' before
;;  applying the alternative action function.
;;
;;  See Also:
;;
;;  * (@> "Perform Alternative Operations on the Fly")
;;  * (@file :file-name "icicles-doc2.el" :to "Icicles OO: Object-Action Interaction")
;;  * (@file :file-name "icicles-doc2.el" :to "Search and Replace")
;;  * (@> "Choose All Completion Candidates")
;;
;;(@* "Deleting Objects")
;;  ** Deleting Objects **
;;
;;  When it is defined for a particular command, minibuffer command
;;  `icicle-delete-candidate-object', bound to `S-delete' (that's the
;;  `delete' key, Shifted), deletes the object or objects named by the
;;  completion candidate on which it operates.  (At least that is the
;;  default behavior - if you customize `icicle-deletion-action-flag'
;;  to `nil', then `S-delete' has no effect.)
;;
;;  Which objects are thus targeted by a given candidate (name) is
;;  something that must be defined by the particular command.  The doc
;;  string of a command should always indicate the effect of using
;;  `S-delete', if a deletion action is defined.
;;
;;  As an example of a deletion action, Icicles command
;;  `icicle-buffer-other-window', bound to `C-x 4 b', opens buffers
;;  named by the individual candidates you act on, using `C-RET'.  But
;;  it also lets you kill any buffer that you act on, using
;;  `S-delete'.  This is not the alternative action for the command
;;  (which is bound to `C-S-RET'); it is the deletion action.
;;  Similarly, command `icicle-bookmark' jumps to a bookmark, but you
;;  can also use `S-delete' with it to delete individual bookmarks.
;;
;;  When you use `S-delete' with a command that allows duplicate
;;  candidate names that represent different candidate objects, it
;;  deletes only the object associated with the current candidate
;;  (name).
;;
;;  Some multi-commands define a deletion action, so that `S-delete'
;;  works; some do not.  Consult the doc for any given command to see
;;  if it does.  Whenever it is defined, the meaning of "delete"
;;  depends on the particular command you use.
;;
;;  `S-delete' deletes only the objects named by the current
;;  completion candidate.  However, with a prefix argument, it deletes
;;  *ALL* objects named by the current set of completion candidates,
;;  after you confirm that this is really what you want to do.  This
;;  is a quick way to delete things whenever `S-delete' is available:
;;  Use input patterns, with progressive completion, chipping away,
;;  and so on, to define the candidates to delete, then use `C-u
;;  S-delete' and confirm their deletion.  Bye-bye.
;;
;;  Do not confuse the unshifted `delete' key with `S-delete'.
;;  `delete' does not delete any objects; it just removes a completion
;;  candidate so that you cannot complete to it.  `S-delete' deletes
;;  an object and removes its name as a completion candidate.
;;
;;  If you are an Emacs-Lisp programmer, then you can define your own
;;  multi-commands that provide a deletion action via `S-delete'.
;;  There are two ways to do this.  Both involve binding
;;  `icicle-delete-candidate-object':
;;
;;  * Bind it to a deletion function.  The function must accept a
;;    completion candidate string and perform the deletion.
;;
;;  * Bind it to a symbol (variable) whose value is a list of
;;    completion-candidate objects.  The entries in the list must be
;;    completion candidates for the current call to `completing-read',
;;    but the list itself need not be the COLLECTION argument to
;;    `completing-read'.  The list can be an alist, a list of strings,
;;    or a list of symbols.  The object that corresponds to the
;;    current candidate when `S-delete' is invoked is deleted from the
;;    list.  If, in addition, the list variable is a user option, then
;;    the updated list value is saved in the user's `custom-file' (or
;;    init file, `~/.emacs').
;;
;;  For more information about using this feature in Emacs-Lisp code,
;;  see the doc of function `icicle-delete-current-candidate-object'
;;  (`S-delete') and variable `icicle-delete-candidate-object'.
;;
;;(@* "Option `icicle-use-C-for-actions-flag'")
;;  ** Option `icicle-use-C-for-actions-flag' **
;;
;;  In some contexts, you end up using `C-next' more than `next', and
;;  likewise for the other keys that combine candidate action and
;;  cycling.  This is especially true for Icicles multi-commands that
;;  act like a browser, such as `icicle-search', `icicle-imenu',
;;  `icicle-find-tag', `icicle-Info-goto-node', and
;;  `icicle-compilation-search'.  In these cases, you use the action
;;  keys to navigate among the locations indicated by the completion
;;  candidates.
;;
;;  If you set user option `icicle-use-C-for-actions-flag' to `nil',
;;  then the keys that cycle are swapped with the keys that both cycle
;;  and act on a candidate.  You can then use `down', `up', `next',
;;  `prior', `end', and `home' to both cycle and act (e.g. navigate),
;;  and `C-down', `C-up', `C-next', `C-prior', `C-end', and `C-home'
;;  to merely cycle, without acting.  The option has no effect on
;;  other keys.
;;
;;  You can toggle `icicle-use-C-for-actions-flag' at any time using
;;  `M-g' (`icicle-toggle-C-for-actions') in the minibuffer.
;;
;;(@* "Accessing Saved Locations (Bookmarks) on the Fly")
;;  ** Accessing Saved Locations (Bookmarks) on the Fly **
;;
;;  When you complete the names of some kinds of objects, you can use
;;  `C-x m' to follow bookmarks to objects of that type.  This is
;;  available only if you use library `bookmark+.el'.
;;
;;  For example, when you invoke a command that completes file names,
;;  you can use `C-x m' to interrupt that operation and complete
;;  against the names of file bookmarks.  This is a multi-command, so
;;  you can actually visit any number of file bookmarks.  When
;;  finished, you can continue with non-bookmark file-name completion.
;;
;;  The same thing holds for Info bookmarks when you use
;;  `icicle-Info-goto-node' (`g' in Info mode); for buffer (non-file)
;;  bookmarks when you use `icicle-buffer' (`C-x b'); and for Dired
;;  bookmarks when you use `icicle-dired' (`C-x d').
;;
;;  See Also:
;;  (@file :file-name "icicles-doc2.el" :to "Icicles Bookmark Enhancements")
 
;;(@* "Multi-Inputs")
;;
;;  Multi-Inputs
;;  ------------
;;
;;  Multi-commands, multi-completions, and multi-inputs too.
;;
;;  In Icicles, if you hit `RET' in the minibuffer then the entire
;;  minibuffer content is entered: sent to Emacs for processing by the
;;  current command.  In some cases you can also act individually on
;;  multiple inputs that together make up the minibuffer contents.
;;
;;  Such multi-inputs are read using Lisp function `read' and then
;;  acted on individually in order, left to right.  That `read' is
;;  used means that you must separate multi-inputs the same way that
;;  Lisp sexps are separated.  Typically this means either putting
;;  whitespace between them or wrapping them in "...".
;;
;;  It is the value after reading a multi-input that is acted on.  You
;;  can act on all multi-inputs at once, but each individually, using
;;  `M-R' (command `icicle-multi-inputs-act').  (You can also use
;;  `M-S' to save them as a set of retrievable completion candidates -
;;  see (@> "Saving or Retrieving Additional Candidates").)
;;
;;  For example, here are some multi-inputs in a minibuffer:
;;
;;    ici*.el "my file.txt" bookmark+.el
;;
;;  After reading, these are the multi-inputs that can be acted on:
;;
;;    ici*.el
;;    my file.txt
;;    bookmark+.el
;;
;;  With `C-x C-f' (`icicle-file'), `M-R' opens each of those input
;;  patterns, just as if you had used `C-x C-f' separately on each
;;  one: it visits all Icicles files, file `my file.txt', and file
;;  `bookmark+.el'.
;;
;;  With this feature you can, for instance, select an existing list
;;  of file names, yank it into the minibuffer of a file-processing
;;  command such as `C-x C-f', and act on each of the files.
;;
;;  `M-R' can be used with any minibuffer, not just one that is used
;;  for completion.  But the command that reads from the minibuffer
;;  needs to provide an action that can be applied to such
;;  multi-inputs.
;;
;;  This is the case for all multi-commands: the candidate action
;;  function - the same function that you apply to individual
;;  completion candidates using `C-RET' - is also the action function
;;  for individual multi-inputs.  For commands other than
;;  multi-commands, the command definition just needs to bind the
;;  multi-input action function to variable
;;  `icicle-multi-inputs-action-fn'.
;;
;;  For example, if you had your own command `my-string' that calls
;;  `read-string' and shows the string length, then you would do the
;;  following - it is the `let' binding that gives `M-R' its action
;;  function:
;;
;;    (defun my-string ()
;;      "Read a string and show its length."
;;      (interactive)
;;      (let ((icicle-multi-inputs-action-fn  'my-string-action))
;;        (my-string-action (read-string "String: "))))
;;
;;    (defun my-string-action (string)
;;      "Show length of STRING."
;;      (message "`%s' has %d characters" string (length string))
;;      (sleep-for 1))
;;
;;    M-x my-string a bb ccc dddd M-R RET
;;
;;  The `M-R' displays the lengths of the multi-inputs `a', `bb',
;;  `ccc', and `dddd', in turn.  The `RET' then displays the length of
;;  the complete input, `a bb ccc dddd'.  You can of course end using
;;  `C-g' instead of `RET' if you are interested only in the effect of
;;  `M-R'.
;;
;;  How do you populate the minibuffer with multi-inputs?  One way is
;;  simply to type them or yank text, as indicated above.  But there
;;  are also two minibuffer keys that help in this regard.  `M-o'
;;  (`icicle-insert-history-element') lets you choose multiple
;;  previous inputs, inserting them into the minibuffer.  `M-r'
;;  (`icicle-roundup') lets you choose current completion candidates,
;;  inserting them.  These are both multi-commands that you use from
;;  the minibuffer.
;;
;;  Both `M-o' and `M-r' accept a prefix argument that controls
;;  candidate insertion: whether the candidate is automatically
;;  followed by a space char or wrapped with "...".  See
;;  (@> "Multi-Input Insertion with a Prefix Arg") for an explanation.
;;
;;  See Also:
;;
;;  * (@> "History Enhancements") for information about populating the
;;    minibuffer with multiple previous inputs using `M-o'.
;;
;;  * (@> "Multi-Input Insertion with a Prefix Arg") information about
;;    using a prefix argument with `M-o' or `M-r'.
;;
;;  * (@> "Saving or Retrieving Additional Candidates") for
;;    information about using `M-S' to save multi-inputs as a set of
;;    completion candidates for later reuse.
 
;;(@* "Icicles Tripping")
;;
;;  Tripping with Icicles
;;  ---------------------
;;
;;  Among the more useful multi-commands are those whose actions take
;;  you to some location indicated by the completion candidate.  This
;;  is the way commands such as `icicle-bookmark', `icicle-find-tag',
;;  `icicle-Info-goto-node', and `icicle-occur' work - you can use
;;  `next' and so on to move among candidates to choose them to act
;;  on, but when you do act on them, Icicles takes you to the places
;;  they name.
;;
;;  So just holding down both the Control key and `next' takes you
;;  from one place to the next.  And `C-mouse-2' takes you directly to
;;  the location you click.  Typically, `C-g' aborts the trip and puts
;;  you back at your starting point, and `RET' ends the trip at the
;;  chosen destination.
;;
;;  There are many such Icicles tripping (or navigation or browsing)
;;  commands, and they all work similarly.  They give you the normal
;;  cycling behavior provided by vanilla Emacs commands such as
;;  `find-tag' (via `M-.', `C-u M-.', `M-*' etc.) or
;;  `set-mark-command' (via `C-u C-SPC').  But unlike the vanilla
;;  Emacs commands, the keys for this cycling are always the same.
;;
;;  More importantly, you need not cycle through all possibilities.
;;  You can go directly to particular locations with `C-RET',
;;  `C-mouse-2' or using completion.  And your input filters the
;;  available candidates, as always.  And you can, as always, use
;;  progressive completion, chipping away, and so on to define your
;;  `C-next' trip itinerary using a process of refinement.
;;
;;  Whereas vanilla Emacs gives you some commands that let you use
;;  completion to enter a destination and go there, and it gives you
;;  other commands that let you cycle among locations, Icicles rolls
;;  all of that into one.  And you use the same keys, always, to
;;  navigate.
;;
;;  Here are some of the Icicles tripping commands:
;;
;;  * Trips among tagged files (delicious-style tagging - requires
;;    library `bookmark+.el').  (The `C-f' is a reminder that the
;;    command uses `read-file-name', like `find-file'.  The `a'
;;    indicates that the command completes an autofile bookmark name.)

;;    (Use prefix key `C-x 4 j' for other-window commands.)
;;    `icicle-find-file-tagged'                   (`C-x j t C-f C-f')
;;    `icicle-find-file-all-tags'                 (`C-x j t C-f *')
;;    `icicle-find-file-all-tags-regexp'          (`C-x j t C-f % *')
;;    `icicle-find-file-some-tags'                (`C-x j t C-f +')
;;    `icicle-find-file-some-tags-regexp'         (`C-x j t C-f % +')
;;
;;    `icicle-bookmark-autofile-all-tags'         (`C-x j t a *')
;;    `icicle-bookmark-autofile-all-tags-regexp'  (`C-x j t a % *')
;;    `icicle-bookmark-autofile-some-tags'        (`C-x j t a +')
;;    `icicle-bookmark-autofile-some-tags-regexp' (`C-x j t a % +')
;;
;;  * Trips among bookmarks of all types:
;;
;;    `icicle-bookmark'                           (`C-x j j',
;;                                                 `C-- C-x r m')
;;    `icicle-bookmark-tagged'                    (`C-x j t j')
;;
;;  * Type-specific bookmark trips (requires library `bookmark+.el').
;;    (Use prefix key `C-x 4 j' for other-window commands.)
;;    `icicle-bookmark-autofile'                  (`C-x j a')
;;    `icicle-bookmark-non-file'                  (`C-x j b')
;;    `icicle-bookmark-bookmark-list'             (`C-x j B')
;;    `icicle-bookmark-dired'                     (`C-x j d')
;;    `icicle-bookmark-file'                      (`C-x j f')
;;    `icicle-bookmark-file-this-dir'             (`C-x j . f')
;;    `icicle-bookmark-gnus'                      (`C-x j g')
;;    `icicle-bookmark-info'                      (`C-x j i')
;;    `icicle-bookmark-image'                     (`C-x j M-i')
;;    `icicle-bookmark-desktop'                   (`C-x j K')
;;    `icicle-bookmark-local-file'                (`C-x j l')
;;    `icicle-bookmark-man'                       (`C-x j m')
;;    `icicle-bookmark-remote-file'               (`C-x j n')
;;    `icicle-bookmark-region'                    (`C-x j r',
;;                                                 `C-u C-u C-x C-x')
;;    `icicle-bookmark-all-tags'                  (`C-x j t *')
;;    `icicle-bookmark-some-tags'                 (`C-x j t +')
;;    `icicle-bookmark-all-tags-regexp'           (`C-x j t % *')
;;    `icicle-bookmark-some-tags-regexp'          (`C-x j t % +')
;;    `icicle-bookmark-file-all-tags'             (`C-x j t f *')
;;    `icicle-bookmark-file-all-tags-regexp'      (`C-x j t f % *')
;;    `icicle-bookmark-file-some-tags'            (`C-x j t f +')
;;    `icicle-bookmark-file-some-tags-regexp'     (`C-x j t f % +')
;;    `icicle-bookmark-url'                       (`C-x j u')
;;    `icicle-bookmark-w3m'                       (`C-x j w')
;;    `icicle-bookmark-temporary'                 (`C-x j x')
;;    `icicle-bookmark-bookmark-file'             (`C-x j y')
;;    `icicle-bookmark-autonamed'                 (`C-x j #')
;;    `icicle-bookmark-autonamed-this-buffer'     (`C-x j , #')
;;    `icicle-bookmark-this-buffer'               (`C-x j , ,')
;;    `icicle-bookmark-specific-buffers'          (`C-x j = b')
;;    `icicle-bookmark-specific-files'            (`C-x j = f')
;;
;;  * `icicle-buffer' (`C-x b')        - Trip among buffers
;;  * `icicle-buffer-narrowing'        - Trip among narrowings
;;                                       (buffer restrictions)
;;  * `icicle-compilation-search' (`C-c `') - Trip among `grep' hits
;;  * `icicle-dired'                   - Trip among directories
;;  * `icicle-file' (`C-x C-f')        - Trip among files
;;  * `icicle-find-file' (`C-x C-f')   - Trip among files
;;  * `icicle-find-file-absolute' (`C-u C-x C-f') - Trip among files
;;  * `icicle-find-file-in-tags-table' - Trip among files listed in
;;                                       current tags table (project)
;;  * `icicle-find-file-read-only' (`C-x C-r') - Visit read-only
;;  * `icicle-find-first-tag' (`C-x 4 .') - Trip among tag hits
;;  * `icicle-find-tag' (`M-.')        - Trip among tag hits
;;  * `icicle-goto-any-marker' (`C-0 C-SPC') - Trip among all markers
;;  * `icicle-goto-global-marker' (`C-- C-x C-SPC') - Trip among
;;                                       global markers
;;  * `icicle-goto-marker' (`C-- C-SPC') - Trip among local markers
;;  * `icicle-imenu' (`C-c =')           - Trip among definitions
;;    (`icicle-imenu-full' to search full definitions)
;;
;;  * Type-specific Imenu trips (library `imenu+.el' recommended).
;;    (And `*-full' versions of each to search full definitions.)
;;    `icicle-imenu-command'
;;    `icicle-imenu-face'
;;    `icicle-imenu-key-explicit-map'
;;    `icicle-imenu-key-implicit-map'
;;    `icicle-imenu-macro'
;;    `icicle-imenu-non-interactive-function'
;;    `icicle-imenu-user-option'
;;    `icicle-imenu-variable'

;;  * `icicle-Info-goto-node' (`g' in Info)- Trip among Info nodes
;;  * `icicle-Info-index' (`i' in Info) - Trip among Info nodes
;;  * `icicle-Info-menu' (`m' in Info) - Trip among Info nodes
;;  * `icicle-locate', `icicle-locate-file' - Trip among files
;;  * `icicle-occur' (`C-c '')         - Trip among `occur' hits
;;                                       (`icicle-search' among
;;                                       single-line hits)
;;  * `icicle-occur-dired-marked-recursive' - Search marked in Dired
;;  * `icicle-recent-file'             - Trip among recent files
;;
;;  * `icicle-search' (`C-c `')        - Trip among regexp search hits
;;  * `icicle-search-bookmarks-together' (`C-u C-c `'),
;;    `icicle-search-bookmark',        - Search multiple bookmarks
;;
;;  * Type-specific bookmark searches
;;    `icicle-search-bookmark-list-bookmark' - Search bookmark lists
;;    `icicle-search-dired-bookmark'   - Search Dired bookmarks
;;    `icicle-search-file-bookmark'    - Search file bookmarks
;;    `icicle-search-gnus-bookmark'    - Search Gnus bookmarks
;;    `icicle-search-info-bookmark'    - Search Info bookmarks
;;    `icicle-search-local-file-bookmark'- Search local-file bookmarks
;;    `icicle-search-man-bookmark'     - Search `man'-page bookmarks
;;    `icicle-search-non-file-bookmark' - Search non-file bookmarks
;;    `icicle-search-region-bookmark'  - Search bookmarked regions
;;    `icicle-search-remote-file-bookmark' - Search remote bookmarks
;;    `icicle-search-url-bookmark'     - Search URL bookmarks
;;  * `icicle-search-char-property'    - Trip among buffer strings with
;;                                       with a text/overlay property
;;  * `icicle-search-dired-marked-recursive' - Search marked in Dired
;;  * `icicle-search-file'             - Search multiple files
;;  * `icicle-search-ibuffer-marked'   - Search marked bufs in Ibuffer
;;  * `icicle-search-keywords' (`C-c ^') - Trip among keyword search
;;                                       hits.
;;  * `icicle-search-overlay-property' - Trip among buffer strings
;;                                       with some overlay property.
;;  * `icicle-search-pages'            - Search Emacs pages
;;  * `icicle-search-paragraphs'       - Search Emacs paragraphs
;;  * `icicle-search-sentences'        - Search sentences as contexts
;;  * `icicle-search-text-property' (`C-c "') - Trip among buffer
;;                                       strings with a text property
;;  * `icicle-search-thing'            - Search thing-at-point things
;;  * `icicle-search-word' (`C-c $')   - Search whole-word hits
;;  * `icicle-search-xml-element'      - Search XML elements
;;  * `icicle-search-xml-element-text-node' - Search text of XML elts
;;  * `icicle-select-frame' (`C-x 5 o') - Trip among frames, by name
;;  * `icicle-select-window' (`C-0 C-x o') - Trip among windows, by
;;                                       buffer name
;;  * `icicle-select-zone'             - Select buffer zones as region
;;
;;  (You need library library `Bookmark+' for
;;  `icicle-search-bookmark-list-marked'.  You need library `Dired+'
;;  for `icicle-occur-dired-marked-recursive' and
;;  `icicle-search-dired-marked-recursive'.  You need library
;;  `zones.el' for `icicle-buffer-narrowing' and
;;  `icicle-select-zone'.)
;;
;;  Note: Icicles search commands and commands `icicle-find-tag',
;;  `icicle-goto-marker', `icicle-goto-any-marker', and
;;  `icicle-goto-global-marker' effectively bind user option
;;  `icicle-incremental-completion' to `always', because I think you
;;  typically want to start them out with incremental completion
;;  turned on.  Remember that you can use `C-#' (once or twice) to
;;  turn incremental completion off.
;;
;;(@* "Highlighting the Destination")
;;  ** Highlighting the Destination **
;;
;;  `icicle-bookmark-region-other-window' activates the bookmarked
;;  region (highlighting it) when you visit it, if you use Transient
;;  Mark mode (or, e.g., Delete Selection mode).
;;
;;  Starting with Emacs 22, most Icicles commands that have single
;;  positions as their trip visits (e.g. `icicle-bookmark',
;;  `icicle-Info-goto-node', `icicle-goto-marker', `icicle-find-tag')
;;  highlight those positions temporarily as they are visited.  Except
;;  for the Icicles search commands, this highlighting is provided by
;;  library `crosshairs.el'.  If `crosshairs.el' and the libraries it
;;  requires are not in your `load-path', then no such highlighting
;;  occurs.
;;
;;  See Also:
;;
;;  * (@> "Icicles Commands that Read File Names") for information
;;    about `icicle-find-file', `icicle-find-file-absolute',
;;    `icicle-find-file-in-tags-table', `icicle-locate',
;;    `icicle-locate-file', and `icicle-recent-file'.
;;  * (@file :file-name "icicles-doc2.el" :to "Icicles Enhancements for Emacs Tags")
;;    for information about `icicle-find-first-tag' and
;;    `icicle-find-tag'.
;;  * (@file :file-name "icicles-doc2.el" :to "Icicles Bookmark Enhancements")
;;    for information about the bookmark browsing commands.
;;  * (@file :file-name "icicles-doc2.el" :to "Icicles Info Enhancements")
;;    for information about `icicle-Info-goto-node',
;;    `icicle-Info-index', and `icicle-Info-menu'.
;;  * (@file :file-name "icicles-doc2.el" :to "Icicles Search Commands, Overview")
;;    for information about `icicle-occur' and `icicle-search'.
;;  * (@file :file-name "icicles-doc2.el" :to "Other Icicles Search Commands")
;;    for information about `icicle-compilation-search',
;;    `icicle-imenu*' commands, `icicle-search-char-property',
;;    `icicle-search-keywords', `icicle-search-overlay-property', and
;;    `icicle-search-text-property'.
;;
;;  * (@file :file-name "icicles-doc2.el" :to "Defining Icicles Tripping Commands")
;;    for information about defining your own tripping commands.
 
;;(@* "Key Completion")
;;
;;  Key Completion
;;  --------------
;;
;;  Here's another weird Icicles feature: completing key sequences
;;  instead of commands or other minibuffer input text.  (This feature
;;  works only for Emacs 22 and later.)
;;
;;  What on earth for?  Have you ever wanted to use one of those
;;  myriad `C-x' key sequences, but forgot just what it was?  The
;;  standard solution to that is to use `C-x C-h', to display all of
;;  the `C-x' bindings together with their commands.
;;
;;  OK, but then you have to scroll down the list of bindings,
;;  searching for the command you want, and then use its key binding.
;;  You can use `C-M-s' to search for a substring of the command name,
;;  in case you do not recall the exact name, but why not use Icicles
;;  completion for this?  Why not match against possible key sequences
;;  and commands?
;;
;;(@* "Completing Keys")
;;  ** Completing Keys **
;;
;;  To complete keys in Icicles, start the key sequence as usual, and
;;  then hit `S-TAB' (command `icicle-complete-keys').  For example,
;;  use `C-x' or `C-x 4', and then hit `S-TAB' to complete the prefix
;;  `C-x' or `C-x 4' (or whatever).  You're then completing against
;;  candidates that are composed of two parts, separated by the value
;;  of option `icicle-complete-keys-separator' ("  =  " by default):
;;
;;  * a key binding that completes what you've typed so far -
;;    e.g. `C-j' (that is, `C-x C-j')
;;
;;  * the command it is bound to - e.g. `dired-jump-other-window'
;;
;;  So, for example, this is a single completion candidate:
;;
;;    C-j  =  dired-jump-other-window
;;
;;  You can match your minibuffer input against the key name, the
;;  command name, or both.
;;
;;  Suppose, for instance, that you want to use a version-control
;;  command, and you remember that all such commands are bound to key
;;  sequences that begin with `C-x v'.  You enter as much of the key
;;  sequence as you remember (`C-x v'), and then you hit `S-TAB'.  You
;;  can then use completion (either apropos or prefix) against the
;;  matching key sequences and command names to invoke the right
;;  command.  And, as a bonus, you are reminded of its key sequence.
;;  You can thus use Icicles key completion to execute a command and,
;;  at the same time, learn its key binding.
;;
;;  (The documentation always refers to the key that performs key
;;  completion as `S-TAB'.  Actually, it is `S-TAB' only by default.
;;  You can customize it, using option `icicle-key-complete-keys'.)
;;
;;(@* "`S-TAB' Is Everywhere - Start With It")
;;  ** `S-TAB' Is Everywhere - Start With It **
;;
;;  In Icicle mode, whenever you are not in the minibuffer or buffer
;;  `*Completions*', key `S-TAB' (`S-tab') initiates key completion.
;;  That is, you do not need to first type part of a key sequence to
;;  use it - you can start with it.  Hit `S-TAB' at any time, and
;;  you're completing a key sequence, even if you have not yet hit any
;;  keys.
;;
;;  This lets you see all key sequences that are available in a given
;;  context.  For example, in Dired, keys special to that mode are
;;  included (and are highlighted as local bindings - see (@> "Local
;;  Bindings and Menu Items Are Highlighted")).
;;
;;  When completing a key sequence, you can type part of a command
;;  name, then hit `S-TAB' to apropos-complete against the command
;;  name.  In this respect, `S-TAB' acts like `M-x', but the key
;;  binding is also part of the completion candidate, so you can also
;;  match key names.
;;
;;(@* "Completing Keys By Name")
;;  ** Completing Keys By Name **
;;
;;  So, just how do you complete input against a set of
;;  binding-plus-command completion candidates?  You can always cycle
;;  among the candidates, of course, and then choose one.  But what
;;  about completion?  Just type text to match candidates, then use
;;  `S-TAB' or `TAB' as usual to complete the text.  Text?  Yes.
;;  Completion candidates are always, ultimately, strings.
;;
;;  Suppose that you type `C-x S-TAB' to show all key sequences that
;;  begin with `C-x'.  You might see a candidate that looks like this:
;;
;;    C-q  =  toggle-read-only
;;
;;  You can then type "C-q" or "d-onl" or any other substring, and
;;  then use `S-TAB' to complete the candidate.  (This second use of
;;  `S-TAB' invokes the command `icicle-apropos-complete', which has
;;  nothing to do with `icicle-complete-keys', which was invoked by
;;  the first `S-TAB'.  The first was invoked outside the minibuffer;
;;  the second was invoked from the minibuffer, during completion.)
;;
;;(@* "Completing Prefix Keys")
;;  ** Completing Prefix Keys **
;;
;;  What happens if the completion candidate is itself a prefix key?
;;  For example, `C-x S-TAB' shows some candidates whose commands are
;;  shown as "...", like this:
;;
;;    4  =  ...      5  =  ...
;;    6  =  ...      C-k  =  ...
;;    ESC  =  ...    RET  =  ...
;;
;;  These represent prefix keys (`C-x 4', `C-x C-k', and so on).  If
;;  you choose such a candidate, then you just continue completing -
;;  buffer `*Completions*' is updated to show the completions of the
;;  compound prefix: `C-x 4', `C-x RET', or whichever you choose.  The
;;  minibuffer prompt shows the completion so far; if you choose
;;  `RET', for instance, then it shows `C-x RET' while prompting you
;;  for the rest of the key sequence.
;;
;;  By default, completion candidates are sorted in buffer
;;  `*Completions*' with local bindings listed first.  You can use
;;  `C-M-,' at any time during key completion to toggle between this
;;  order and sorting with the prefix-key candidates shown first.  You
;;  can use `C-,' at any time to change the sort order among these two
;;  orders and sorting by command name.
;;
;;(@* "Top-Level Key Completion from a Prefix Key")
;;  ** Top-Level Key Completion from a Prefix Key **
;;
;;  In contrast to the previous section, "Completing Prefix Keys",
;;  this section could alternatively be called "Not Completing
;;  (Ignoring) Prefix Keys".
;;
;;  Command `icicle-complete-keys' is normally (i.e., by default)
;;  invoked from `S-TAB', either at top level or by using a prefix key
;;  followed by `S-TAB'.  In the latter case, it completes that prefix
;;  key.  This is a primary use case: show the completions of a prefix
;;  key.
;;
;;  But what if you wanted to perform top-level key completion, but
;;  invoke that operation from a key (`S-TAB' or another) that is on a
;;  prefix keymap?
;;
;;  For example, what if you want to use, say, `C-x C-^' for top-level
;;  key completion?  If you just bind `icicle-complete-keys' to `C-x
;;  C-^' then when you use that key sequence Icicles will complete
;;  prefix key `C-x'.  You need some way to tell Icicles to ignore the
;;  `C-x' here.  You can do that by customizing option
;;  `icicle-complete-keys-ignored-prefix-keys' to include the prefix
;;  key you want to ignore (`C-x' in this case).
;;
;;  You probably do not want to do this for `C-x', since it is useful
;;  to be able to complete `C-x' itself.  But you might, for example,
;;  want to make key completion available from a menu or some other
;;  prefix key whose completions you do not need to be reminded of.
;;
;;  To add top-level key completion to, say, the menu-bar `Edit' menu,
;;  you would customize `icicle-complete-keys-ignored-prefix-keys' by
;;  adding `<menu-bar> <edit>' as a prefix key to ignore.  Then you
;;  would add `icicle-complete-keys' as a `Complete Keys' menu item on
;;  the `Edit' menu, as follows:
;;
;;    (global-set-key [menu-bar edit icicle-complete-keys]
;;                    '("Complete Keys" . icicle-complete-keys))
;;
;;  This has already been done for you: menu item `Show Available Keys
;;  (Complete Key)' in the menu-bar `Icicles' menu performs top-level
;;  key completion.  Use it to see which keys are possible in the
;;  current context.
;;
;;(@* "Navigate the Key-Binding Hierarchy")
;;  ** Navigate the Key-Binding Hierarchy **
;;
;;  Choosing a completion candidate such as `C-x  =  ...' effectively
;;  navigates down the key-binding hierachy (prefix-key hierarchy), to
;;  complete against all keys with prefix `C-x'.  Choosing `5  =  ...'
;;  to complete the prefix `C-x' then navigates down another level, to
;;  complete keys that have prefix `C-x 5'.
;;
;;  What about navigating back up the hierarchy, say from the `C-x 5'
;;  keys to the `C-x' keys, or from the `C-x' keys to the keys with no
;;  prefix?  The special completion candidate `..' does that.  By
;;  default, it is always the first candidate in the `*Completions*'
;;  list.  It is of course not available unless you are completing a
;;  prefix; that is, it is not available at the top level.
;;
;;  This feature means that you can navigate the key-binding hierachy
;;  just as you would navigate the file-system hierarchy (using, say,
;;  `C-x C-f') or the menu-bar hierarchy (using library `lacarte.el').
;;
;;  In fact, since menu-bar bindings are also key bindings, you can
;;  also use key completion to navigate the menu-bar hierarchy - just
;;  complete the prefix key `menu-bar'!  Start with `S-TAB', choose
;;  `menu-bar  =  ...', then choose a menu, and so on.  But menu-bar
;;  menu completion is handy enough that it has its own Icicle mode
;;  key binding, by default: `S-f10'.
;;
;;  Icicles key completion thus provides a general browser for key
;;  bindings, which you can also use to learn about keys and their
;;  associated comands, without necessarily executing them - see
;;  (@> "Key and Command Help").
;;
;;  Gotcha: `S-TAB' uses apropos completion, by default, so remember
;;  that typing `.' matches any character (except a newline).  To
;;  match only the literal string `..' (to go up a level), do one of
;;  the following:
;;
;;  * Turn on escaping of regexp special characters - use `C-`' in the
;;    minibuffer to toggle this.
;;
;;  * Use prefix completion (`TAB').
;;
;;  * Escape the regexp special character explicitly: `\.\.' (or use
;;    `^\.').
;;
;;  * Cycle to candidate `..'.
;;
;;  TIP: During minibuffer completion for something other than a key,
;;       `S-TAB' does not, of course, initiate key completion - it
;;       performs apropos completion for the current set of completion
;;       candidates.  But you can fake it: Use `C-x S-TAB .. RET'.
;;       That is, use `C-x' (which is OK because Icicles provides some
;;       minibuffer key bindings with prefix `C-x').  Then hit `S-TAB'
;;       to see completions of `C-x'.  Then choose the
;;       pseudo-candidate `..', to go up a level and show all key
;;       bindings, not just those with prefix `C-x'.
;;
;;(@* "Local Bindings and Menu Items Are Highlighted")
;;  ** Local Bindings and Menu Items Are Highlighted **
;;
;;  Sometimes it helps to know which key sequences are local bindings,
;;  that is, bindings that are specific to the current mode.  For
;;  example, Dired mode defines keys specific to Dired buffer, such as
;;  `* %', `% g', and `!'.
;;
;;  To help you distinguish local key bindings from others (global and
;;  minor-mode bindings), local bindings are highlighted in buffer
;;  `*Completions*' using face `icicle-special-candidate'.  Otherwise,
;;  the key portion of a candidate (left side of `KEY  =  COMMAND') is
;;  highlighted with face `icicle-candidate-part'. 
;;
;;  However, to make menu items stand out, they are highlighted with
;;  face `icicle-key-complete-menu-local' for menus specific to the
;;  current mode, and face `icicle-key-complete-menu' for other menu
;;  items.  By default, these are the same as faces
;;  `icicle-special-candidate' and `icicle-candidate-part',
;;  respectively, except that they are also boxed.
;;
;;(@* "Completing Keys By Just Hitting Them")
;;  ** Completing Keys By Just Hitting Them **
;;
;;  It may seem odd that you must complete a key sequence by entering
;;  the names of keys, rather than just hitting the keys themselves:
;;  e.g. typing "C-f" rather than hitting `C-f'.  However, if keys
;;  themselves were used for completing, then they could not be used
;;  normally during key-sequence completion.  You could not move the
;;  cursor around the minibuffer using `C-f' or `right' (right arrow),
;;  because those keys would be treated as input for completion.  You
;;  could not use `up' or `down' to cycle among completion candidates
;;  for the same reason.  Likewise, you could not use printing
;;  (self-inserting) keys, such as `a' and `$', to match command
;;  names.  Having to use key names, instead of keys, for completion
;;  is a small price to pay for being able to complete key sequences.
;;
;;  Nevertheless, Icicles also provides a way for you to type key
;;  sequences directly, even if it is something of a workaround:
;;  precede each key with `M-q' (`icicle-insert-key-description',
;;  during key completion) - think of `q' for "quote".  This inserts
;;  the key description of whatever key you hit next.  This key
;;  description (name) can be used to match key-completion candidates.
;;  So, for example, instead of typing "C-f", you can hit `M-q' and
;;  then hit `C-f'.  The key description "C-f" is inserted in the
;;  minibuffer.  If you use `M-q C-M-right', then "C-M-right" is
;;  inserted.  Try it: `S-TAB M-q C-M-right' -> "C-M-right".  Then hit
;;  `TAB' or `S-TAB' to complete the candidate all the way to this:
;;
;;    C-M-right  =  enlarge-frame-horizontally
;;
;;  Note: Whether or not angle brackets are used is governed by user
;;  option `icicle-key-descriptions-use-<>-flag' (aka
;;  `icicle-key-descriptions-use-angle-brackets-flag').  By default,
;;  this is `nil', so angle brackets are not used, which I think
;;  improves readability.  If you set this to non-`nil', then you will
;;  see "<C-M-right>" instead of "C-M-right", both as a completion
;;  candidate and as what is inserted when you use `M-q'.  You can
;;  also provide a prefix argument to `M-q' to flip the behavior of
;;  `icicle-key-descriptions-use-<>-flag' for that occurrence only.
;;  See also my library `naked.el', which lets you use the
;;  no-angle-brackets style also outside of Icicles.
;;
;;(@* "Key and Command Help")
;;  ** Key and Command Help **
;;
;;  That points out another use of key completion, opposite to
;;  learning the bindings of commands: learning the commands bound to
;;  given keys.  In other words, `S-TAB M-q' does both what `C-h w'
;;  (`where-is') does and what `C-h c' (`describe-key-briefly') does.
;;  It also does what `C-h b' (`describe-bindings') does.
;;
;;  The point here is not that `S-TAB M-q' is quicker than `C-h w' or
;;  `C-h c' or `C-h b' - it's not.  The point is that key completion
;;  can be handy in several ways, and it can teach you various things
;;  about keys and commands as you use it.
;;
;;  In addition to this key-completion help about bindings, you can
;;  display help on the commands that are the right sides of the
;;  `S-TAB' completion-candidate equations, by using the multi-command
;;  help keys (see (@> "Help on Completion Candidates")).  That is,
;;  while completing, you can use `C-M-mouse-2', `C-M-RET',
;;  `C-M-next', and so on to describe the command named in the current
;;  completion candidate.
;;
;;(@* "`S-TAB' Is a Multi-Command")
;;  ** `S-TAB' Is a Multi-Command **
;;
;;  Yes, `S-TAB' as `icicle-complete-keys' is a multi-command - see
;;  (@> "Multi-Commands")).  This means that you can, within the same
;;  execution of `S-TAB', invoke any number of keys by clicking
;;  (`C-mouse-2') their names in buffer `*Completions*' or choosing
;;  them any other way (`C-RET', `C-next', and so on).
;;
;;  Since you can navigate up and down the key-binding hierarchy, you
;;  could even stay within a single `S-TAB' invocation to do nearly
;;  everything you want in Emacs (see (@> "Three-Key Emacs"))!
;;
;;(@* "Possible Source of Confusion")
;;  ** Possible Source of Confusion **
;;
;;  Keep in mind that `S-TAB' has two different uses in Icicles when
;;  you are providing input in the minibuffer:
;;
;;  * If input completion is available, then `S-TAB' performs apropos
;;    completion (it is, in effect, bound to
;;    `icicle-apropos-complete').
;;
;;  * If input completion is not available, then `S-TAB' performs key
;;    completion (it is, in effect, bound to `icicle-complete-keys').
;;
;;  In addition, in buffer `*Completions*' `S-TAB' moves backward
;;  among the candidate completions.
;;
;;  This is by design; it takes advantage of the fact that these
;;  contexts are mutually exclusive.  However, this economy comes at a
;;  risk of potential confusion.  It's important that you know whether
;;  or not completion is available when you are inputting text.  If
;;  input completion is not available, but you think it is, then
;;  hitting `S-TAB' might give you a surprise by key completing.  That
;;  behavior is normal - you can use key-completion to input special
;;  characters, for instance.  But if you think that you are instead
;;  completing the original input requested, then you can become
;;  confused.
;;
;;  Icicles provides completion status indicators so that you can
;;  easily tell when completion is available for minibuffer input.
;;  There are two indicators: (1) at the beginning of the minibuffer
;;  prompt and (2) in the `Icy' minor-mode lighter in the mode line.
;;  See (@> "Completion Status Indicators").  If completion is not
;;  indicated when you are prompted for input, it means that `S-TAB'
;;  is available, not for input completion, but for key completion.
;;  Another clue can be found in the prompt text.  For key completion,
;;  it says "Complete keys: ".
;;
;;  If you nevertheless find the overloaded use of `S-TAB' confusing,
;;  you can change the bindings of the key `S-TAB' in these different
;;  contexts.  To do that, you can customize options
;;  `icicle-apropos-complete-keys', `icicle-key-complete-keys', and
;;  `icicle-completion-list-key-bindings'.
;;
;;(@* "Complete Keys in the Minibuffer Also")
;;  ** Complete Keys in the Minibuffer Also **
;;
;;  In the minibuffer, `S-TAB' performs apropos completion of your
;;  typed input.  So it is not available for key completion.  But you
;;  can still complete keys in the minibuffer.  You just use `M-S-TAB'
;;  instead of `S-TAB'.  (If your window manager steals `M-S-TAB' then
;;  try `ESC S-TAB'.)
;;
;;  And just as the actual keys that perform key completion outside
;;  the minibuffer are customizable using option
;;  `icicle-key-complete-keys', so too for the key-completion keys in
;;  the minibuffer: It is actually the keys defined by option
;;  `icicle-key-complete-keys-for-minibuffer' that are used -
;;  `M-S-TAB' is just the key used by default.
;;
;;(@* "Three-Key Emacs")
;;  ** Three-Key Emacs **
;;
;;  Icicles key completion piles a lot of stuff into `S-TAB'.  Just as
;;  `M-x' lets you execute any Emacs command, so does `S-TAB'.  But
;;  `S-TAB' also lets you insert characters.  You might say that it
;;  gives you all of Emacs in one key binding.
;;
;;  Of course, you need a couple other keys, as well.  How many?
;;  Suppose you had limited accessibility in terms of input devices.
;;  Maybe you use Emacs on a cell phone, without voice recognition -
;;  or whatever.  How many keys, buttons, or whatnot do you need to
;;  use Emacs?
;;
;;  1. You need one for `C-g', to interrupt commands.
;;  2. You need one to start telling Emacs what to do.
;;  3. You might need one to choose from a set of possible things to
;;     do.
;;  4. You need one to tell Emacs that you're done telling it what to
;;     do.
;;
;;  (#2 and #3 might be combined somehow.)
;;
;;  What does vanilla Emacs offer out of the box in this regard?
;;
;;  * You can get by with just `mouse-1' and the menu-bar menus, but
;;    they do not cover all of Emacs.  You cannot use them to enter
;;    text, for instance.  Of course, you could add more menus, to be
;;    able to do more.
;;
;;  * You can use `M-x' plus `RET' to execute any command.  But how
;;    would you insert text?
;;
;;  * Similarly, for `M-:', which lets you evaluate any Emacs-Lisp
;;    sexp.  You still need a way to type characters.
;;
;;  Icicles key completion lets you do almost anything in Emacs with
;;  three or four keys, buttons, or whatever:
;;
;;  * `S-TAB' - Offers every key sequence as a possible choice to
;;              execute.
;;  * `next'  - Cycles among candidates, for choosing.
;;  * `RET'   - Chooses the current candidate.
;;  * And of course `C-g', to cancel the current operation.
;;
;;  `S-TAB' includes key `M-x', which offers all commands (even those
;;  not bound) as possible choices.  It also includes key `M-:', which
;;  lets you execute any Emacs-Lisp expression.  That's almost all of
;;  Emacs!
;;
;;  You could even perhaps get away with only three mouse buttons, and
;;  no keyboard:
;;
;;  * `mouse-1' - Choose candidates, scroll, and so on (direct access,
;;    no cycling).
;;
;;  * `mouse-2' - Do what `S-TAB' does (bind it to
;;    `icicle-complete-keys' and `icicle-apropos-complete').
;;
;;  * `mouse-3' - Do what `C-g' does (bind it to `keyboard-quit' and
;;    `icicle-abort-recursive-edit').
;;
;;  Here, `mouse-2' and `mouse-3' are not even used as mouse (pointer)
;;  functions; any keys or buttons would do.  You could use just
;;  `mouse-1' plus a Shift key and a Control key.
;;
;;  Would you want to use Emacs only this way?  Of course not, if you
;;  had a choice.  Typing the character `a' by cycling through every
;;  possible key binding/command combination and hitting `RET' when
;;  you get to `a  =  self-insert-command' would be the epitome of
;;  tedium.  Likewise, doing everything with a single pointer-device
;;  button.  Using only three or four keys or buttons is definitely
;;  not the ideal way to take advantage of Emacs.
;;
;;  But you are probably not limited to just 3 or 4 keys or buttons.
;;  The real point here is that Icicles `S-TAB' opens the door to
;;  almost everything in Emacs.  And if you do have a normal keyboard,
;;  then you can type letters and such to match command names and key
;;  sequences.  Key `next' matches substrings (regexps, actually),
;;  which makes choice even quicker.
;;
;;(@* "Entering Special and Foreign Characters")
;;  ** Entering Special and Foreign Characters **
;;
;;  Command `self-insert-command' is bound to each key that is
;;  associated with a character that can be inserted in text.  It is
;;  the binding of the key `a' and the key `$', for example.  It is
;;  also the binding of keys that your keyboard might not even have -
;;  keys that correspond to special or odd characters and characters
;;  in other languages.
;;
;;  To Icicles key completion, these keys are like other keys.
;;  However, because there are many, MANY keys bound to
;;  `self-insert-command', it can be distracting and slow to allow
;;  such keys as completion candidates.  If option
;;  `icicle-complete-keys-self-insert-ranges' is `nil' (the default
;;  value), then such keys are excluded as candidates.  This is
;;  probably what you want.
;;
;;  If the option is non-`nil', then you can use key completion to
;;  insert the characters whose codes are in the range(s) defined by
;;  the option value.  This lets you see the candidate characters in
;;  `*Completions*' (WYSIWYG), but it is not a terribly convenient or
;;  quick way to insert characters.
;;
;;  Starting with Emacs 23, vanilla Emacs has Unicode support, and you
;;  can insert any Unicode characters using either an input method or
;;  `C-x 8 RET'.  `C-x 8 RET' is bound by default in Emacs to command
;;  `insert-char' (called `ucs-insert' prior to Emacs 24).  If you use
;;  my library `ucs-cmds.el' then you might want to remap that command
;;  to command `ucsc-insert', which is an enhancement.
;;
;;  If option `icicle-read-char-by-name-multi-completion-flag' is
;;  non-`nil' then Icicles enhances this in a few ways:
;;
;;  * It shows in `*Completions*', for each candidate Unicode
;;    character, its name and code point, as well as the character
;;    itself.
;;
;;  * When you cycle among the matching candidates, the name and code
;;    point of the current candidate are shown in the mode line.  The
;;    code point is shown in hexadecimal, octal, and decimal
;;    notations.
;;
;;  * Completion candidates are in fact multi-completions, meaning
;;    that you can match against the name or the code point, or both.
;;
;;  * You can even match the character itself.  Why might you want to
;;    do that?  To see the corresponding Unicode character name(s),
;;    including any old names.  For example, for the character `
;;    (grave accent) you get these two completion candidates:
;;
;;      GRAVE ACCENT      60      `
;;      SPACING GRAVE     60      `
;;
;;  The main purpose for this is to show you the characters and code
;;  points together with their names (WYSIWYG).  The characters are
;;  shown in `*Completions*' using the same font as the frame from
;;  which you invoked `C-x 8 RET'.  That you can also match against
;;  the characters themselves is just a nice-to-have.
;;
;;  The most important features of the Icicles version of `C-x 8 RET'
;;  are (a) seeing the characters and code points (WYSIWYG) and (b)
;;  being able to use progressive completion, so you can use multiple
;;  simple patterns instead of a single complex pattern.
;;
;;  I recommend that you use `insert-char' (or better, `ucsc-insert'),
;;  not key completion, to insert Unicode characters.
;;
;;  There are thousands of Unicode characters.  So if you do use a
;;  non-`nil' value for `icicle-complete-keys-self-insert-ranges' then
;;  you will likely want to use only small ranges for better
;;  performance, e.g., `((0 . 687))' covers Latin characters.  For
;;  Emacs 22, the option is effectively Boolean: any non-`nil' value
;;  allows all self-inserting keys as candidates (there are far fewer
;;  available characters in Emacs 22).
;;
;;  For each Unicode character, the completion candidate is
;;  `CHAR  =  UNICODE-NAME', where UNICODE-NAME is the name of the
;;  Unicode character.  This is so that you can complete against the
;;  name.  But again, I recommend that you use  `C-x 8 RET', not key
;;  completion, to insert a Unicode character.
;;
;;(@* "Handling Keymaps That Are Inaccessible From the Global Map")
;;  ** Handling Keymaps That Are Inaccessible From the Global Map **
;;
;;  Actually, `S-TAB' is not bound to `icicle-complete-keys' in every
;;  keymap.  That would be inconvenient, in general.  By default, it
;;  is so bound in each keymap that is accessible from the global
;;  keymap, as determined by function `accessible-keymaps'.
;;
;;  You've seen, above, how you can navigate through prefix keys,
;;  starting with the global map.  In Dired, for instance, you can use
;;  `S-TAB' at the top level, then choose the prefix key `*' in
;;  `*Completions*', then choose a key, such as `/' (to mark
;;  directories), in the `*' keymap.
;;
;;  However, the act of binding of `S-TAB' in keymaps that are
;;  accessible from the global map does not bind it in the `*' prefix
;;  keymap itself.  To handle this case, Icicles explicitly does for
;;  `dired-mode-map' what it does for the global map: it binds `S-TAB'
;;  in each keymap that is accessible from `dired-mode-map'.  Because
;;  of this, you can use `* S-TAB' to show all key completions of `*'.
;;
;;  This treatment of `dired-mode-map' is done by default.  Similarly
;;  for a few other keymaps.  But you might have other keymaps that
;;  you would like to treat similarly - keymaps that Icicles might be
;;  unaware of.  You do this by including them in the list value of
;;  user option `icicle-keymaps-for-key-completion', along with
;;  `dired-mode-map' and the others provided in the default value.
;;  The list entries are Emacs-Lisp symbols that are bound to keymaps,
;;  each of which should define at least one prefix key.  If you add a
;;  keymap variable to this list, then `S-TAB' will be bound so that
;;  you can use it to complete the prefix keys defined by that map.
;;
;;  Notice that there is no keymap variable that corresponds to prefix
;;  key `*' in Dired mode.  You need only provide a keymap (variable
;;  `dired-mode-map') from which the prefix key is accessible; it is
;;  not necessary to also provide a variable that corresponds to the
;;  prefix keymap itself.
;;
;;  If a keymap listed in `icicle-keymaps-for-key-completion' is not
;;  defined when Icicle mode is entered, then it is ignored.  If you
;;  later define that keymap, then just exit and reenter Icicle mode
;;  for the `S-TAB' binding to take effect.  For example, use `M-x
;;  icy-mode' twice after entering Calendar mode, to be able to
;;  complete `calendar-mode' prefix keys such as `t' - `t S-TAB'.
;;
;;(@* "Automatic Key Completion")
;;  ** Automatic Key Completion **
;;
;;  There are now a few other libraries, such as `guide-key.el' and
;;  `which-key.el', that provide some of what Icicles key completion
;;  offers.
;;
;;  They offer mostly on-the-fly help while you hit keys, not
;;  completion against the associated command names, browsing of
;;  prefix-key hierarchies, etc.  Instead of on-demand control of key
;;  help (`S-TAB'), they show help automatically, after an idle delay.
;;
;;  Similarly, you can opt for automatic display of Icicles key
;;  completions by turning on minor mode
;;  `icicle-auto-complete-keys-mode'.  User option
;;  `icicle-auto-complete-key-delay' specifies how many seconds to
;;  wait before displaying the key completions.
;;
;;  By default this mode is off, because I think help on demand is
;;  generally more useful.
 
;;(@* "Icicles Multi `M-x'")
;;
;;  Icicles Multi `M-x'
;;  -------------------
;;
;;  How about a multi-command replacement for `M-x'?  Instead of
;;  executing a single command, it would execute any number of
;;  commands.  This section describes two such multi-commands,
;;  `icicle-execute-extended-command' and `icicle-command-abbrev',
;;  which by default are bound in Icicle mode to `M-x' and `M-ESC C-x'
;;  (aka `ESC ESC C-x'), respectively.  See Also:
;;  (@file :file-name "icicles-doc2.el" :to "Defining Icicles Multi `M-x'").
;;
;;(@* "Multi `M-x': `icicle-execute-extended-command'")
;;  ** Multi `M-x': `icicle-execute-extended-command' **
;;
;;  When you use `M-x' in vanilla Emacs, you are actually executing
;;  the standard Emacs command `execute-extended-command'.  That
;;  command prompts you for the name of another command, which you
;;  input.  It uses `completing-read' to do this, which is why you can
;;  take advantage of Icicles features when you use `M-x'.  Nothing
;;  new here.
;;
;;  Command `icicle-execute-extended-command' is simply a
;;  multi-command version of `execute-extended-command'.  It does the
;;  same thing, except that it also lets you execute multiple
;;  commands, one by one, using `C-RET' (or `C-next' and so on),
;;  without ever exiting the minibuffer.
;;
;;  With the default value of option `icicle-top-level-key-bindings',
;;  `M-x' is bound to `icicle-execute-extended-command' whenever you
;;  are in Icicle mode.  If you never use it as a multi-command, you
;;  will not notice much difference from `execute-extended-command'.
;;  As always in Icicles, characters such as `?' self-insert, however.
;;
;;  But because Emacs command names typically do not contain spaces
;;  (though they can!), `SPC' is not self-inserting for
;;  `icicle-execute-extended-command'.  This is the only exception to
;;  the rule in Icicles.  For `M-x' with prefix completion, a space
;;  character performs word-at-a-time completion, just as it does in
;;  vanilla Emacs.  In other words, in this exceptional case, `SPC'
;;  acts the same as `M-SPC'.
;;
;;(@* "Examples of Using Multi `M-x'")
;;  *** Examples of Using Multi `M-x' ***
;;
;;  Example: Repeat a command multiple times.  Yes, `C-x z' does this
;;  already (and better) - this is just an illustration.  `M-x
;;  forward-ch TAB' completes to `forward-char'.  Then, use `C-RET' to
;;  execute that command.  Repeat as many times as you want.  Use a
;;  prefix argument if you like.
;;
;;  To switch to another command in the same `M-x' invocation: Erase
;;  the minibuffer (`M-k'), complete the second command, then use
;;  `C-RET'.  As long as you have not yet used `RET', `S-RET', `C-g'
;;  (or, say, `C-]'), you remain within the same invocation of `M-x'.
;;
;;  What about executing a command that, itself, reads an input
;;  argument?  That's OK.  And if that command reads its input with
;;  completion, then you can use `C-RET' on the completion candidates
;;  for that input.
;;
;;  Example: `M-x describe-fa TAB C-RET' gives you the prompt for
;;  command `describe-face'.
;;
;;  1. Type `ici S-TAB' to see the available Icicles faces.
;;
;;  2. Hit `next' until face `icicle-complete-input' is highlighted.
;;
;;  3. Hit `C-RET' to display its documentation.
;;
;;  4. Type `C-next' a few times to see the doc of other Icicles
;;     faces.
;;
;;  5. Use `M-k' to erase the minibuffer, then type `search S-TAB' to
;;     see faces about searching.
;;
;;  6. Cycle through them with `next', then use `C-RET' on
;;     `icicle-search-main-regexp-current' to show its documentation.
;;
;;  7. Use `C-next' to do the same for face
;;     `icicle-search-main-regexp-others'.
;;
;;  8. Use `RET' to finish with command `describe-face' - but you're
;;     still in the same invocation of `M-x'.
;;
;;  9. Change the input to `describe-coding-system' and play again,
;;     this time with coding-system names...
;;
;;  Remember, if you get confused or lost in the minibuffer: `C-]'
;;  (`abort-recursive-edit') or `C-M-S-t' (aka `C-M-T',
;;  `icicle-top-level') should always straighten you out.
;;
;;(@* "What about describe-variable and describe-function?")
;;  *** What about describe-variable and describe-function? ***
;;
;;  Sadly, if you try the last example with `describe-variable' or
;;  `describe-function', you might be in for a surprise.  In Emacs 20,
;;  they both work fine.  In later Emacs versions, `describe-variable'
;;  gives you the message "You did not specify a variable", and
;;  `describe-function' displays a `*Help*' buffer that says that each
;;  function you choose is really a keyboard macro!
;;
;;  Why?  It's a bit complex, but worth hearing about if you want to
;;  understand multi M-x better.
;;
;;  When you choose a command that reads an argument in the minibuffer
;;  and you then hit a multi-command key such as `C-RET' to choose an
;;  argument, Icicles tries to apply the command you chose to the
;;  argument you chose.  However, completion candidates are always
;;  strings, and the command you chose might expect something other
;;  than a string.  That is the case for `describe-variable', for
;;  instance.  The case of `describe-function' is special: it
;;  interprets a string argument blindly as a keyboard macro sequence.
;;
;;  Icicles is smart enough to pick up a `wrong-type-argument' error,
;;  if the command you choose barfs on a string argument.  In that
;;  case, Icicles converts the string to a symbol (or a number) and
;;  tries again, using the symbol (or the number).
;;
;;  And that's why `describe-variable' works in Emacs 20 but not in
;;  later versions: In Emacs 20, `describe-variable' (sanely) raises a
;;  type error if you pass it a string, and Icicles is able to save
;;  the day by then passing it the corresponding symbol.  In later
;;  versions of Emacs, however, instead of raising an error with the
;;  message "You did not specify a variable", `describe-variable' just
;;  displays the message - no error, so there is no way for Icicles to
;;  recuperate.
;;
;;  I've reported this design misfeature to the Emacs developers, and
;;  I hope it will be fixed in a future Emacs version.  Until then, at
;;  least you know...  The more general lesson is this: Icicles can
;;  turn every command into a multi-command, but multi-command actions
;;  will not work for every command.
;;
;;(@* "Multi `M-x' Turns Every Command into a Multi-Command")
;;  *** Multi `M-x' Turns Every Command into a Multi-Command ***
;;
;;  Most of the time, of course, you do not execute commands
;;  successively by name; instead, you use key bindings.  The point
;;  here is that even if you have a binding for a command, Icicles
;;  `M-x' lets you use any command as a multi-command, which can
;;  sometimes be advantageous.
;;
;;  For example, Icicles defines and binds a real multi-command to
;;  `C-x 0' in Icicle mode, which lets you delete any number of
;;  windows.  But, even without such a multi-command, you can get a
;;  similar effect by using `M-x delete-windows-on'.  In this way, you
;;  can turn ordinary Emacs commands that use completion into
;;  multi-commands.
;;
;;  The other point is that you can move from one command to another
;;  within the same execution of `M-x'.  This is a different feature
;;  from being able to use any command that uses completion as a
;;  multi-command.  Both features have their uses.
;;
;;(@* "Multi `M-x' with Abbreviations: `icicle-command-abbrev'")
;;  ** Multi `M-x' with Abbreviations: `icicle-command-abbrev' **
;;
;;  The second multi-command that you can use in place of
;;  `execute-extended-command' is `icicle-command-abbrev', bound in
;;  Icicle mode to `M-ESC C-x' (aka `ESC ESC C-x').  It is similar to
;;  `M-x' (`icicle-execute-extended-command'), with the added twist
;;  that it lets you input command abbreviations, as well as commands.
;;
;;  If option `icicle-add-proxy-candidates-flag' is non-`nil', then
;;  command abbreviations, as well as commands, are available as
;;  completion candidates.  Otherwise, only commands are available.
;;  You can toggle this user option using `C-M-_' in the minibuffer.
;;
;;  Emacs partial completion and some other libraries provide ways for
;;  you to enter command abbreviations instead of command names at the
;;  command prompt (`M-x').  Library `exec-abbrev-cmd.el' by Tassilo
;;  Horn <tassilo@member.fsf.org> is an example.
;;
;;  So just what is a command abbreviation?  Hyphens (`-') in command
;;  names divide them into parts.  For example, `find-file' has two
;;  parts: `find' and `file'.  Each character of a command
;;  abbreviation corresponds to one part of each of the commands that
;;  match the abbreviation.  For example, abbreviation `ff' matches
;;  commands `find-file' and `focus-frame', and abbreviation `fg'
;;  matches `find-grep'.
;;
;;  If user option `icicle-command-abbrev-match-all-parts-flag' is
;;  `nil', then an abbreviation need not match all parts of a command
;;  name; it need match only a prefix.  For example, `nil' means that
;;  abbreviation `ff' also matches `find-file-other-window' and `fg'
;;  also matches `find-grep-dired'.
;;
;;  In Icicles, you can input both abbreviations and commands at the
;;  same prompt, and you can take advantage of the multi-command
;;  feature to execute multiple commands.  You can thus treat command
;;  abbreviations just like commands.  If an abbreviation matches a
;;  single command name, then that command is invoked immediately.  If
;;  it matches more than one, then you can use completion to choose
;;  one.
;;
;;  One or more, that is - multi-command completion is available for
;;  both abbreviations and commands.  That is, you can invoke any
;;  number of them within the same use of `M-ESC C-x' (`ESC ESC C-x').
;;
;;  What happens if your input matches a command name but it is also
;;  an abbreviation for other command names?  By default, command
;;  names take precedence: if your input matches a command name then
;;  that command is invoked.  So, for example, by default the Emacs
;;  command `cd' takes precedence over `cd' as an abbreviation for
;;  commands such as `compile-defun' and `cancel-debug-on-entry'.  If
;;  you instead want abbreviations to take precedence over command
;;  names, then set option `icicle-command-abbrev-priority-flag' to
;;  `t'.
;;
;;  Abbreviations are completed against the (persistent) list of
;;  abbreviations you have used in the past.  That list is also
;;  directly customizable as option `icicle-command-abbrev-alist'.
;;
;;  Besides completing against past abbreviations, you can enter new
;;  abbreviations (the completion is thus lax).  When you exit Emacs,
;;  your abbreviations list is updated and saved, along with the
;;  number of times you've used each abbreviation.  The latter
;;  information is used to sort your abbreviations for completion, so
;;  that those used most frequently are available first.
;;
;;  See Also:
;;
;;  * (@file :file-name "icicles-doc2.el" :to "Global Filters") for
;;    information about the kind of matching that `M-x'
;;    (`icicle-execute-extended-command') does.
;;
;;  * (@file :file-name "icicles-doc2.el" :to "Completion Methods and Styles")
;;    for information about fuzzy completion, which is somewhat
;;    similar to completing abbreviations but is not limited to
;;    inputting commands.
 
;;(@* "Choose All Completion Candidates")
;;
;;  Choose All Completion Candidates
;;  --------------------------------
;;
;;  The previous section describes how you can use `C-RET'
;;  (`icicle-candidate-action') to choose (act on) multiple completion
;;  candidates, individually.  If you hold down the Control key while
;;  you cycle through the candidates, you can run through each of
;;  them, one by one.
;;
;;  Command `icicle-all-candidates-action', which is bound to `C-!' in
;;  the minibuffer, is a shorthand way of doing that: act on all
;;  candidates that match the current input.  In many contexts, `C-!'
;;  reports on any objects that were not acted upon successfully (in
;;  buffer `*Help*').
;;
;;  All multi-commands let you use `C-!' in this way.  Whenever a
;;  command defines a special action for `C-RET' to perform on the
;;  current completion candidate, you can use `C-!' to perform it on
;;  all candidates at once.
;;
;;  Perhaps you already use `% m' (command `dired-mark-files-regexp')
;;  in Dired to mark all files that match a given regular expression,
;;  and then operate on all of the marked files in some way (search
;;  with `A', query-replace with `Q', open with `F', delete with `D',
;;  and so on).  When you execute a multi-command, `C-!' lets you do
;;  something similar.
;;
;;  How does it work?  It applies `icicle-candidate-action-fn' to each
;;  completion candidate that (apropos- or prefix-) matches the
;;  current input in the minibuffer.
;;
;;  Most top-level Icicles commands are multi-commands.  Command
;;  `icicle-delete-file' is an example.  Instead of entering a file
;;  name at the prompt (e.g. using completion or cycling), you can
;;  type a regular expression, use `S-TAB' to see all matching files,
;;  and then use `C-!' to delete all of them at once.
;;
;;  You get the idea: Use the minibuffer to determine a set of objects
;;  by pattern matching, and then act on all elements of the set.
;;
;;  In addition to `C-!', keys `M-!', `C-|', and `M-|' act similarly:
;;
;;  * `M-!' is like `C-!', but it acts on the list of matching
;;    candidates as a whole, rather than acting individually on each
;;    candidate.  For example, with command `icicle-customize-face',
;;    `M-!' opens a single Customize buffer for all of the matching
;;    faces, while `C-!' opens a separate Customize buffer for each
;;    face.
;;
;;  * `C-|' and `M-|' are like `C-!' and `M-!', respectively, but they
;;    apply an alternative action, not the main action, whenever one
;;    is available.
;;
;;  In the definition of a given multi-command, the appropriate action
;;  functions are bound to variables:
;;
;;  * `icicle-candidate-action-fn' (applied to each candidate by
;;    `C-!') - normal single-candidate action
;;
;;  * `icicle-candidate-alt-action-fn' (applied to each candidate by
;;    `C-|') - alternative single-candidate action
;;
;;  * `icicle-all-candidates-list-action-fn' (applied to all
;;    candidates together by `M-!') - normal list-of-candidates action
;;
;;  * `icicle-all-candidates-list-alt-action-fn' (applied to all
;;    candidates together by `M-|') - alternative list-of-candidates
;;    action
;;
;;  For most multi-commands, however, only the normal single-candidate
;;  action is defined.  In this case, `M-!' duplicates what `C-!'
;;  does.  If the corresponding function is not available, each of the
;;  list-action keys (`M-!', `M-|') behaves the same as the
;;  corresponding single-candidate key (`C-!', `C-|), and vice versa.
;;  So for instance, if `icicle-all-candidates-list-action-fn' is
;;  `nil' when reading some input, then `M-!' acts the same as `C-!'.
;;
;;  As a shortcut, if you have saved completion candidates and they
;;  all belong to the current set of completion candidates, then `C-!'
;;  acts on the saved candidates instead of the complete set of
;;  candidates.  This means that you need not first do `C-M-<' to
;;  retrieve the saved candidates; you can do `C-!' directly to act on
;;  them.  `C-|', `M-!', and `M-|' work the same way.
;;
;;  All of the all-candidates actions inhibit candidate help display
;;  in the mode line and minibuffer messages that the individual
;;  actions might effect.  This is to avoid unnecessary delays.
;;
;;  See Also:
;;
;;  * (@file :file-name "icicles-doc2.el" :to "Icicles Dired Enhancements")
;;    for an Icicles alternative to both `A' and `Q' (search and
;;    replace) in Dired.
 
;;(@* "Sets of Completion Candidates")
;;
;;  Sets of Completion Candidates
;;  -----------------------------
;;
;;  Whereas `C-RET' acts on individual objects, `C-!'  acts on an
;;  entire set of objects at once, via their names: the set of all
;;  current completion candidates.  There are additional Icicles
;;  commands that also act, not on individual completion candidates,
;;  but on one or more sets of completion candidates.
;;
;;  One of these is `M-*' or `S-SPC', which effectively narrows the
;;  set of completion candidates by taking the intersection of the
;;  candidate sets defined by various input regexps.
;;
;;  This section presents some more Icicles commands that act on sets
;;  of completion candidates.  The basic idea is that you can perform
;;  set operations using the current set of completion candidates,
;;  changing it into a different set.  You can, then, for example, use
;;  `C-!' to act on everything in a custom-defined set.  Or you can
;;  define a custom set that you want to use often (for example, a
;;  list of project files), save it persistently, and then retrieve it
;;  later to use for completion.
;;
;;  Another way of acting on a set of candidates is to insert them
;;  into the minibuffer and then use `M-R' - see (@> "Multi-Inputs").
;;
;;(@* "Saving and Retrieving Completion Candidates")
;;  ** Saving and Retrieving Completion Candidates **
;;
;;  Set operations such as union and difference act on two sets.  The
;;  current set of completion candidates is always one of these sets.
;;  If an operation, such as set complement, acts on a single set,
;;  then it acts on the current set.
;;
;;  When two sets are involved, the other set is called the "saved
;;  set".  This just means that at some previous time in your sesssion
;;  you saved some completion candidates as the value of variable
;;  `icicle-saved-completion-candidates'.
;;
;;  In buffer `*Completions*', candidates that have been saved are
;;  highlighted using face `icicle-saved-candidate'.
;;
;;  By default, the saved set is not persistent; it is saved only
;;  until the next save in the same Emacs session overwrites it or
;;  adds to it.  See (@> "Persistent Sets of Completion Candidates")
;;  for ways to save candidates persistently.
;;
;;  In addition to persistently saving a candidate set in a file, if
;;  you also use library `Bookmark+' then you can save a set of
;;  Icicles search candidates as a bookmark.  You "jump" to the
;;  bookmark to restore those search results during another Icicles
;;  search.
;;
;;  One way you can save candidates is to use
;;  `icicle-candidate-set-save', bound to `C-M->'.  This saves all of
;;  the current candidates.
;;
;;  Gotcha: If you use progressive completion and you have not yet
;;          typed anything after `M-*' or `S-SPC', then there is not
;;          yet a set of candidates to save.  If you use `C-M->' at
;;          that point, you will reset the saved candidates to none.
;;          To define the current candidates, either type something or
;;          use `S-TAB'.
;;
;;  Another way to save candidates, besides `C-M->', is to select
;;  candidates in buffer `*Completions*' using the (active) region,
;;  and then use `icicle-candidate-set-save-selected', bound to
;;  `C-M-)'.  This saves any candidates that are at least partially in
;;  the region.
;;
;;  You can also use `C-M-)' to UNsave all candidates: just select no
;;  candidates before you hit `C-M-)', to reset the set of saved
;;  completions to none.  Think of this as replacing the saved set
;;  with the empty set (no candidates).  And you need not use `TAB' or
;;  `S-TAB' first to use this, since the current set of candidates is
;;  not used in any way when you reset the saved set.
;;
;;  Command `icicle-mouse-candidate-set-save', bound to `M-S-mouse-3'
;;  in `*Completions*' combines these two: if the region is active,
;;  then the selected candidates become the saved set; otherwise, all
;;  candidates are saved.  This binding makes it easy to save
;;  candidates using the mouse: select them (e.g. drag or double-click
;;  `mouse-1', or click `mouse-1' then `mouse-3'), then use
;;  `M-S-mouse-3' to save. [*]
;;
;;  You can process the list of saved candidates in any way you like
;;  using Emacs Lisp.  For example, you can save a list of file names
;;  that match a regexp, then print the list or process the individual
;;  files in some way.  Here, for instance, is how to save the set of
;;  file names that contain either `dir' or `ici':
;;
;;    `C-x C-f \(dir\|ici\) S-TAB C-M-> C-g'
;;
;;  You can retrieve a set of saved candidates with command
;;  `icicle-candidate-set-retrieve', bound to `C-M-<'.  This replaces
;;  the current set of candidates with those retrieved.  It also acts
;;  like `M-*' by entering a recursive minibuffer, which removes any
;;  saved-candidates highlighting.  Using `TAB' or `S-TAB' restores
;;  the highlighting.
;;
;;  What if you have defined the saved set and you later decide to
;;  narrow it by excluding some of its members?  First, if the saved
;;  candidates are not current then use `C-M-<' to make them so.  Then
;;  use progressive completion to narrow the current candidates.  Then
;;  use `C-M->' to define (only) them as the saved candidates.  For
;;  example, if you hit `C-M->' after narrowing to the files matching
;;  prefix `ici', you can narrow that saved set to only those files
;;  that also match `m', by using `S-SPC m C-M->'.
;;
;;(@* "Saving or Retrieving Additional Candidates")
;;  ** Saving or Retrieving Additional Candidates **
;;
;;  You can use `C-<' to retrieve a set of saved candidates and add
;;  them to the current candidates, instead of replacing those
;;  candidates.  This way, you can build up the current set of
;;  candidates by retrieving (combining) several saved sets.
;;
;;  In the other direction, you can save additional candidates, adding
;;  them to a set of candidates already saved, in these ways:
;;
;;  * `C->' (`icicle-candidate-set-save-more') adds all of the current
;;    candidates.
;;
;;  * `C-)' (`icicle-candidate-set-save-more-selected') adds any
;;    candidates that you have selected using the region in
;;    `*Completions*'.
;;
;;  * `M-mouse-3' (`icicle-mouse-candidate-set-save-more') acts the
;;    same as `C-)' or `C->', depending on whether or not the region
;;    is active in `*Completions*': it adds selected or all
;;    candidates.
;;
;;  * Extending the region with `mouse-3', and then clicking `mouse-3'
;;    again in the same place, acts the same as `C-)'.  That is, click
;;    `mouse-1', then click `mouse-3' twice in another location, to
;;    save all candidates between the `mouse-1' and `mouse-3'
;;    positions. [*]
;;
;;  * The `insert' key (`icicle-save/unsave-candidate') adds just the
;;    current completion candidate (e.g. during cycling).  Clicking a
;;    candidate in `*Completions*' with `M-S-mouse-2'
;;    (`icicle-mouse-save/unsave-candidate') does the same thing.  If
;;    you do this to a candidate that has already been saved, then it
;;    is UNsaved (no longer saved).
;;
;;  Note that the `insert' key and `M-S-mouse-2' are toggles for a
;;  given candidate, saving or unsaving it.  In this sense each is its
;;  own opposite.  In another sense, the opposite operation of saving
;;  is simply removing a candidate from the current set of candidates.
;;  You do that using the `delete' key or `S-mouse-2'.
;;
;;  Another way to add to the list of saved candidates is to use `M-S'
;;  (`icicle-multi-inputs-save').  This is available for any
;;  minibuffer input, not just during completion.  It adds all of the
;;  multi-inputs currently in in the minibuffer to the set of saved
;;  candidates.  So you can, for instance, use `M-S' on a minibuffer
;;  containing the following input, to add its three file names to the
;;  current saved set of candidates in variable
;;  `icicle-saved-completion-candidates'.
;;
;;    icicles-mac.el "my file.txt" bookmark+.el
;;
;;  Matching, saving, and retrieving candidates is a powerful way to
;;  interact with completion.  One important use is to prepare a list
;;  of candidates on which to act, and then act on them all at once
;;  using `C-!'.  This is a good way to proceed when you want to
;;  double-check what to act on, before you actually act.  This is the
;;  same idea behind marking files in Dired and then operating on the
;;  marked files, using `x'.  It corresponds to what is represented in
;;  some user interfaces by filling out a checklist followed by
;;  clicking `OK'.
;;
;;  [* If you click `mouse-1' on a candidate and (starting with Emacs
;;  22) `mouse-1-click-follows-link' is an integer, then you will need
;;  to hold the mouse button depressed longer than that many seconds,
;;  or else that candidate will simply by chosen.  If the value is
;;  `t', then this will not work at all.  Any other value presents no
;;  problem.  (Personally, I use `nil'.)]
;;
;;(@* "Different Places for Saving and Retrieving Candidates")
;;  ** Different Places for Saving and Retrieving Candidates **
;;
;;  You can save completion candidates to a different variable from
;;  `icicle-saved-completion-candidates' by using a numeric prefix
;;  argument to command `icicle-candidate-set-save'; that is, use `C-u
;;  N C-M->', where N is an integer.  Alternatively, use `C-M-}',
;;  which is bound to command `icicle-candidate-set-save-to-variable'.
;;
;;  You are prompted for the name of the variable, and you can use
;;  completion when inputting it.  During this completion, the only
;;  available candidates are variables that you have used for saved
;;  candidates (but completion is lax, so you can type a new variable
;;  name).  The same behavior works also for `C->', `C-M-)', and
;;  `C-)'.
;;
;;  To retrieve completion candidates that were previously saved to a
;;  variable other than `icicle-saved-completion-candidates', so that
;;  they become the current set of candidates, use `C-u N C-M-<',
;;  where N is an integer, or `C-M-{' (`icicle-candidate-set-retrieve'
;;  or `icicle-candidate-set-retrieve-from-variable').
;;
;;  Using a plain prefix argument (`C-u' without a number) with
;;  `C-M->' and `C-M-<' saves or retrieves a candidate set using a
;;  cache file, not a variable.  Alternatively, as a shortcut you can
;;  use `C-}' and `C-{' for this.
;;  See (@> "Persistent Sets of Completion Candidates") and
;;  (@file :file-name "icicles-doc2.el" :to "Support for Projects").
;;
;;  When you save candidates to a different variable from
;;  `icicle-saved-completion-candidates', they are not shown in buffer
;;  `*Completions*' using face `icicle-saved-candidate'.  When you
;;  save candidates to a cache file, they are also saved to
;;  `icicle-saved-completion-candidates', so they are shown in
;;  `*Completions*' using face `icicle-saved-candidate'.
;;
;;  `C->' and `C-<' accept the same prefix arguments as `C-M->' and
;;  `C-M-<' , letting you specify the source or destination (variable,
;;  cache file) when you save or retrieve additional candidates.
;;
;;  Note: Use the right type of saved candidates (persistent or not)
;;  for a given command.  It is the particular command that determines
;;  whether or not a given type of saved candidate is appropriate.
;;  For example, you can save search hits when you use
;;  `icicle-search-file' (same as `icicle-search' with a negative
;;  prefix argument), and those saved search-hit candidates
;;  effectively reference files and positions in those files.  And you
;;  can later retrieve and reuse such saved candidates to visit the
;;  search positions.  But those candidates are not merely file names,
;;  so they cannot be used with a command such as `find-file' or
;;  `icicle-file' that expects a file name.  Conversely, you cannot
;;  use a saved set of file names with a command such as
;;  `icicle-search-file' that expects `icicle-search' candidates.
;;
;;(@* "Saving Marked Files and Dirs in Dired")
;;  ** Saving Marked Files and Dirs in Dired **
;;
;;  Besides saving file and directory names that are completion
;;  candidates on the fly, You can create or add to a saved set of
;;  names using the names that are marked in Dired.  The same keys
;;  (e.g., `C-M->') that save candidates in the minibuffer during
;;  completion save the marked names in Dired.
;;
;;  If you use library `Dired+' then you can also use the same keys,
;;  but with the prefix key `M-+' (e.g. `M-+ C-M->') to save not only
;;  the names that are marked in the current Dired buffer but also
;;  those that are marked in any marked subdirectories, recursively.
;;  See (@file :file-name "icicles-doc2.el" :to "Save Marked Names Here and Below").
;;
;;(@* "Set Operations")
;;  ** Set Operations **
;;
;;  The other available set-operation commands for use with completion
;;  candidates, besides saving and retrieving, are these:
;;
;;  * `icicle-candidate-set-swap', bound to `C-%'.  Swap the saved and
;;    current sets of completion candidates.
;;
;;  * `icicle-candidate-set-define', bound to `C-:'.  Define the
;;    current set of completion candidates by evaluating an input
;;    sexp.  The sexp must evaluate to a list of strings, such as is
;;    returned by `all-completions'.  You can use this to substitute
;;    any list of strings, and then operate on them as completions,
;;    using any Icicles functionalities.  Keep in mind, however, that
;;    the completions must be of the proper type for the context in
;;    which they are used.  For example, if you are executing a
;;    command, they must be command names.
;;
;;  * `icicle-candidate-set-complement', bound to `C-~'.  Complement
;;    the current set of candidates: replace the current candidate set
;;    with its set complement.  This means all possible completions of
;;    the appropriate type that do *not* match the current input.  You
;;    can combine this with progressive completion (`M-*' or `S-SPC')
;;    to progressively eliminate candidates that match different
;;    inputs.  This process-of-elimination matching is a common
;;    Icicles usage idiom.
;;
;;  * `icicle-candidate-set-union', bound to `C-+'.  Replace the
;;    current candidate set by its union with the saved set of
;;    candidates.
;;
;;  * `icicle-candidate-set-difference', bound to `C--'.  Replace the
;;    current candidate set by its set difference with the saved set
;;    of candidates.  That is, the saved candidates are subtracted
;;    from the current candidates, and the result becomes the current
;;    candidate set.  To obtain the opposite set difference,
;;    subtracting the current candidates from the saved candidates,
;;    just use `icicle-candidate-set-swap' followed by
;;    `icicle-candidate-set-difference'.
;;
;;  * `icicle-candidate-set-intersection', bound to `C-*'.  Replace
;;    the current candidate set by its intersection with the saved set
;;    of candidates.  Unlike the set intersection provided by `M-*'
;;    (or `S-SPC'), `C-*' is, in itself, a one-time operation.  `M-*'
;;    (or `S-SPC') can be repeated, using the previous intersection as
;;    one of the sets to be intersected in a new operation.  Both
;;    `C-*' and `M-*' use the current set of matching candidates as
;;    one of the sets being intersected.  But `M-*' reads another
;;    input regexp to define the other set to be intersected, whereas
;;    `C-*' uses the saved candidates set as the other set.  `M-*' (or
;;    `S-SPC') is useful for chaining, to achieve progressive
;;    approximation.  `C-*' is useful to perform an intersection on a
;;    set from a previous input reading.
;;
;;  * `icicle-candidate-set-truncate', bound to `M-$'.  Truncate the
;;    set of completion candidates, so that it includes only the first
;;    N candidates (as displayed in `*Completions*').  You are
;;    prompted for N.  You can use this when the order of candidates
;;    represents priority in some way, so that you are interested only
;;    in the topmost candidates.
;;
;;  You can operate on or choose from all input values in the set that
;;  results from any of these set operations.  For example, you can
;;  use `C-~' to see the list of objects that do not match the current
;;  input, to cycle among those objects, or to operate on any or all
;;  of them.  Use `C-~' at any time to switch to the complement of the
;;  current set of candidates.
;;
;;  Example: To cycle through all files whose names do not end in
;;           `el', you can do the following:
;;
;;  1. Use `C-f' to read a file name.
;;  2. Type `el$' to match all file names that end in `el'.
;;  3. Use `S-TAB' to show the matching files.
;;  4. Use `C-~' to flip to the complement: files not ending in `el'.
;;  5. Use `next' or `prior' to cycle among the new set of candidates.
;;
;;  A minibuffer message briefly confirms each of the set operations.
;;
;;  When buffer `*Completions*' is displayed, the union, difference,
;;  and intersection commands scroll the buffer when repeated.
;;  Repeating `icicle-candidate-set-complement' complements the
;;  complement, of course, giving the original set.
;;
;;  Once you have established a set of completion candidates using any
;;  of the candidate-set commands, you can cycle among the candidates
;;  of that set using either prefix or apropos cycling (`down', `up',
;;  `next', `prior', `end', or `home').  However, switching from
;;  prefix to apropos cycling (or completion), or vice versa,
;;  establishes a new completion set of the appropriate type, as
;;  usual.  Switching completion type signifies that you are finished
;;  with the specially defined completion set, and you want to
;;  redefine it using apropos or prefix cycling or completion.
;;
;;  Note: Prefix icompletion (`icomplete.el' or `icomplete+.el' - see
;;        (@> "Icompletion")) does not take into account the candidate
;;        set resulting from a set operation: it always displays the
;;        normal set of prefix completions in the minibuffer.
;;
;;  Note: You might have noticed that, as a mnemonic device, the keys
;;        bound to the various set operations use the corresponding
;;        binary arithmetic or Boolean operators: `~' (unary negation)
;;        for complement (not); `*' (multiplication) for intersection
;;        (and); `+' (addition) for union (or); and `-' (subtraction)
;;        for difference.  Note too that the `C--' and `C-+' bindings
;;        mean that you cannot use these key sequences for prefix
;;        arguments - you must use `C-u N', or `M-N' instead, where N
;;        is a possibly signed integer.
;;
;;(@* "Recompleting Using the Original Domain")
;;  ** Recompleting Using the Original Domain **
;;
;;  Sometimes you want to perform an operation using a saved set of
;;  candidates, and then you want to do something using the original
;;  set of candidates provided by the command's completion domain.
;;  For example, you might want to perform an action on selected files
;;  from a saved candidate set, and then act on files that are not
;;  necessarily in that set.
;;
;;  That is what `C-x C-0' (`icicle-recomplete-from-original-domain')
;;  does: during completion, it forgets about the current set of
;;  candidates and completes your input from scratch, using the
;;  original domain of candidates.
;;
;;  This means that you can also use it after progressive completion,
;;  to pop up to the shallowest minibuffer level (1) and start over,
;;  completing your current input from scratch.
;;
;;  See Also:
;;
;;  * (@> "Multi-Commands") for information about `C-RET'.
;;
;;  * (@> "Choose All Completion Candidates") for information about
;;    `C-!'.
;;
;;  * (@> "Progressive Completion") for information about `M-*' and
;;    `S-SPC'.
;;
;;  * (@> "File-Name Input and Locating Files Anywhere") and
;;    (@> "Persistent Sets of Completion Candidates"), for information
;;    about saving completion candidates persistently and retrieving
;;    them later.
;;
;;  * (@> "History Enhancements"), (@> "Google Matching"), and
;;    (@file :file-name "icicles-doc2.el" :to "Icicles Search Commands, Overview")
;;    for examples of other set operations on input candidates.
;;
;;  * (@file :file-name "icicles-doc2.el" :to "Icicles Dired Enhancements")
;;    for information about saving and reusing sets of file-name
;;    candidates with Dired.
 
;;(@* "Google Matching")
;;
;;  Google Matching
;;  ---------------
;;
;;  This section presents nothing new - but you might not want to skip
;;  it.  It points out something that you might not have picked up
;;  yet.  You've learned about Icicles regexp matching and candidate
;;  set operations, but it can be worthwhile to compare how Icicles
;;  matches inputs against completion candidates with how Google
;;  matches search strings against Web pages.  Summary: You can do
;;  pretty much the same things, but the way you accomplish them is
;;  different.
;;
;;(@* "Domain of Discourse")
;;  ** Domain of Discourse **
;;
;;  In Google, the domain of discourse, that is, the possible set of
;;  search hits, is the set of Web pages.  There are also search
;;  fields that limit the domain of discourse by file type, page
;;  number, update date, page position, freedom of use, and even
;;  morality ("Safe Search").
;;
;;  In Icicles (Emacs), the domain of discourse changes automatically,
;;  depending on the current context.  For command-name input, it is
;;  the set of all named commands; for variable-name input, it is the
;;  set of variable names; and so on.
;;
;;(@* "Global Filtering")
;;  ** Global Filtering **
;;
;;  In Google, you can limit searching to specific Web sites, or
;;  exclude certain Web sites from searching.
;;
;;  In Icicles, you can add extra completion candidates using variable
;;  `icicle-extra-candidates', and you can filter out (other)
;;  candidates globally using filter variables
;;  `icicle-must-match-regexp', `icicle-must-not-match-regexp',
;;  `icicle-must-pass-predicate', and
;;  `icicle-must-pass-after-match-predicate'.  These are internal
;;  Icicles variables.  Normally, you do not change them directly;
;;  instead, a command can use them to limit or extend the effective
;;  domain of discourse.
;;  See (@file :file-name "icicles-doc2.el" :to "Global Filters").
;;
;;  Variables `icicle-must-pass-predicate' and
;;  `icicle-must-pass-after-match-predicate' apply to the textual
;;  candidates that can be displayed in buffer `*Completions*'.  You
;;  can also apply a predicate to the full alist-entry or
;;  obarray-symbol candidates that are supplied to `completing-read'
;;  or `read-file-name' as its COLLECTION argument.  As a programmer,
;;  you can of course do that when your code calls these functions.
;;  As an Icicles user, you can use `M-&' to define and apply
;;  predicates to such alist-entry candidates on the fly, while
;;  completing.
;;  See (@file :file-name "icicles-doc2.el" :to "Icicles Search Commands, Overview").
;;
;;(@* "Word Matching and String Matching")
;;  ** Word Matching and String Matching **
;;
;;  Google matches words, by default, but you can specify an "exact
;;  phrase" to get literal string matching.
;;
;;  By default, Icicles (apropos-)matches regexps, but you can use
;;  `\b' in a regexp to perform word matching, and you can use `C-`'
;;  (`icicle-toggle-regexp-quote') to perform exact (literal)
;;  matching.  See (@> "What About Special-Character Conflicts?").
;;
;;(@* "AND Matching and OR Matching")
;;  ** AND Matching and OR Matching **
;;
;;  Google has search fields for AND matching ("with all of the
;;  words") and OR matching ("with at least one of the words").
;;
;;  In Icicles, you can use progressive completion to perform AND
;;  matching: use `M-*' or `S-SPC' to introduce each term to match.
;;  Alternatively, you can use `C-*'
;;  (`icicle-candidate-set-intersection').  You can use `C-+'
;;  (`icicle-candidate-set-union') to perform OR matching.  Note that,
;;  by definition, unordered AND matching is not possible using a
;;  single regexp.  See (@> "Progressive Completion") and
;;  (@> "Sets of Completion Candidates").
;;
;;(@* "NOT Matching")
;;  ** NOT Matching **
;;
;;  Google has a search field for terms that must not occur in search
;;  hits: "without the words".
;;
;;  In Icicles, you can use `C-~' (`icicle-candidate-set-complement')
;;  to exclude matching completion candidates.  You can chain this
;;  operation, as a form of progressive completion, to exclude any
;;  number of terms: `toto S-TAB C-~ titi C-~ foobar C-~' excludes all
;;  candidates matching toto, titi, or foobar.  Use this
;;  process-of-eliminiation technique to progressively pare down the
;;  set of possible candidates.  Note that such generalized
;;  complementing (as opposed to complementing a character set) is not
;;  possible using a single regexp - you cannot use a regular
;;  expression to say "Show me everything that does *not* match this".
;;  See (@> "Sets of Completion Candidates") and
;;  (@> "Progressive Completion").
 
;;(@* "Buffer-Name Input")
;;
;;  Buffer-Name Input
;;  -----------------
;;
;;  The Icicles commands that read buffer names are multi-commands
;;  (see (@> "Multi-Commands")), so you can act on more than one
;;  buffer during a given command invocation.
;;
;;  These commands all let you use a prefix argument to control which
;;  buffers are completion candidates.  It is the numeric value of the
;;  prefix argument that matters.  Candidates are only the buffers
;;  that satisfy these conditions:
;;
;;  * Plain `C-u': whose mode is derived from the current buffer mode
;;  * `C-u C-u':   visible (possibly in an iconified frame)
;;  * `C-u C-u C-u': invisible
;;  * Zero:        whose mode is the same as the current buffer mode
;;  * Positive:    visiting files
;;  * Negative:    associated with the selected frame
;;
;;  Those are the default behaviors, but you can change them using
;;  option `icicle-buffer-prefix-arg-filtering'.
;;
;;  `icicle-buffer' (`C-x b' in Icicle mode, by default) is a
;;  multi-command that accepts multi-completion input: the first part
;;  matches buffer names, and the second part, which is optional,
;;  matches buffer content.  So for example:
;;
;;    C-x b foo             ; Match buffer names against `foo'
;;    C-x b C-M-j toto      ; Match buffer contents against `toto'
;;    C-x b foo C-M-j toto  ; Match both buffer name and contents
;;
;;  You can use option `icicle-buffer-skip-functions' to specify
;;  patterns for buffer names to exclude from content-searching when
;;  you provide a content-matching pattern to `icicle-buffer'.
;;
;;  In addition to the usual Icicles key bindings, during buffer-name
;;  completion you can use additional keys, which are defined by
;;  option `icicle-buffer-candidate-key-bindings'.  These are the keys
;;  provided by the default value of the option:
;;
;;  * `C-x F' (`icicle-toggle-include-cached-files') Toggle whether
;;    `icicle-buffer' includes cached file names as candidates.  That
;;    is, toggle option `icicle-buffer-include-cached-files-nflag'.  A
;;    prefix argument sets the option value to the numeric prefix
;;    value.
;;
;;  * `C-x R' (`icicle-toggle-include-recent-files') Toggle whether
;;    `icicle-buffer' includes the names of recently accessed files as
;;    candidates.  That is, toggle option
;;    `icicle-buffer-include-recent-files-nflag'.  A prefix argument
;;    sets the option value to the numeric prefix value.
;;
;;  * `C-x m' (`icicle-bookmark-non-file-other-window') Visit a
;;    bookmarked buffer.  This is available only if you use library
;;    `bookmark+.el'.  This too is a multi-command, so you can
;;    actually visit any number of buffer bookmarks with one use of
;;    `C-x m'.  When finished, you can continue with non-bookmark
;;    buffer-name completion.
;;
;;  * `C-x C-m -' (`icicle-remove-buffer-cands-for-derived-mode')
;;    Remove the buffer-name candidates with a major mode that is
;;    derived from a given mode.  You are prompted for the mode.  You
;;    can repeat this key to narrow buffer candidates by mode.  (`C-m'
;;    is the same key as `RET'.)
;;
;;  * `C-x C-m +' (`icicle-keep-only-buffer-cands-for-derived-mode')
;;    Keep only the buffer-name candidates with a major mode that is
;;    derived from a given mode.  You are prompted for the mode.
;;    (`C-m' is the same key as `RET'.)
;;
;;  * `C-x M -' (`icicle-remove-buffer-cands-for-mode') Same as
;;    `C-x C-m -', but excludes ancestor modes.
;;
;;  * `C-x M +' (`icicle-keep-only-buffer-cands-for-mode') Same as
;;    `C-x C-m +', but excludes ancestor modes.
;;
;;  * `C-x * -' (`icicle-remove-buffer-cands-for-modified') Remove the
;;    buffer-name candidates for modified buffers.
;;
;;  * `C-x * +' (`icicle-keep-buffer-cands-for-modified') Keep only
;;    the buffer-name candidates for modified buffers.
;;
;;  * `C-x v -' (`icicle-remove-buffer-cands-for-visible') Remove the
;;    candidate buffers that are visible.  This includes buffers that
;;    are in iconified frames.
;;
;;  * `C-x v +' (`icicle-keep-only-buffer-cands-for-visible') Keep
;;    only the candidate buffers that are visible.  This includes
;;    buffers that are in iconified frames.
;;
;;  * `S-delete' Kill the buffer named by the current completion
;;    candidate.
;;
;;  When cached or recently used file names are included as
;;  candidates, option `icicle-file-skip-functions' is used to exclude
;;  files from content-searching whose names match its patterns,
;;  similarly to what `icicle-buffer-skip-functions' does for buffer
;;  names.
;;
;;  During completion, candidate sorting is specific to buffer names.
;;  `C-,' cycles among the following sort orders:
;;
;;  * by last access
;;  * `*...*' last: put buffers such as `*Messages*' and `*Help*' last
;;  * by buffer size
;;  * by major mode name
;;  * by mode-line mode name (mode name that appears in the mode line)
;;  * by (absolute) file or process name
;;
;;  Remember too that the mode-line candidate help for a buffer-name
;;  candidate includes the mode name, buffer size, and associated file
;;  or directory, if any.  The directory name can be especially
;;  helpful for Dired buffer candidates, since the buffer name does
;;  not tell you the full directory.  For example, if you do `C-u C-x
;;  b' from Dired, so the candidates are names of Dired buffers only,
;;  then cycling shows you the directory for each one.
;;
;;  Similarly, the more detailed help available from `C-M-RET'
;;  etc. also lists the full name of the file or directory associated
;;  with the buffer.
;;
;;  The following user options control buffer-name completion.
;;
;;  * `icicle-buffer-candidate-key-bindings'
;;  * `icicle-buffer-extras' (*)
;;  * `icicle-buffer-ignore-space-prefix-flag'
;;  * `icicle-buffer-include-cached-files-nflag'
;;  * `icicle-buffer-include-recent-files-nflag'
;;  * `icicle-buffer-match-regexp' (*)
;;  * `icicle-buffer-no-match-regexp' (*)
;;  * `icicle-buffer-predicate' (*)
;;  * `icicle-buffer-prefix-arg-filtering'
;;  * `icicle-buffer-require-match-flag'
;;  * `icicle-buffer-skip-functions'
;;  * `icicle-buffer-sort' (*)
;;
;;  You can use option `icicle-buffer-configs' to define buffer
;;  configurations: persistent sets of the option values marked (*).
;;
;;  See Also:
;;
;;  * (@> "Chapter & Verse: Searching Named Containers")
;;  * (@file :file-name "icicles-doc2.el" :to "Customization and General Tips") and
;;    (@file :file-name "icicles-doc2.el" :to "Global Filters") for
;;    information about user options affecting buffer-name completion
;;  * (@> "Chapter & Verse: Searching Named Containers") about
;;    content-searching
;;  * (@> "Match File Names and File Content Too") about
;;    content-searching of files
;;  * (@> "Ido and IswitchB") to use Icicles with a buffer-switching
;;    behavior that is similar to Ido and IswitchB
;;  * (@> "`M-&': Satisfying Additional Predicates") to filter the
;;    buffer candidates on the fly in multiple ways (e.g. size)
 
;;(@* "File-Name Input and Locating Files Anywhere")
;;
;;  File-Name Input and Locating Files Anywhere
;;  -------------------------------------------
;;
;;  Emacs offers two main functions for reading minibuffer input with
;;  completion: `completing-read' and `read-file-name'.  Icicles uses
;;  both of these, and it enhances each of them in various ways for
;;  use in your own Emacs-Lisp code.  These two functions can each be
;;  used to read file-name input, but they do so in very different
;;  ways.
;;
;;  This section provides a reminder description of these two
;;  functions, and then it describes the main Icicles commands that
;;  read file names.
;;
;;  You do not need to use these Icicles commands to get the general
;;  benefits of Icicles enhancements to `completing-read' and
;;  `read-file-name'.  What these commands offer are additional
;;  benefits.
;;
;;(@* "Function `read-file-name'")
;;  ** Function `read-file-name' **
;;
;;  Function `read-file-name' uses relative file-name completion.  It
;;  is specialized for file-name input with completion.  It knows
;;  about files and file names for your current platform.  It knows
;;  about Emacs remote file name syntax (Tramp, Ange FTP).  And
;;  starting with Emacs 23, `TAB' also completes environment variables
;;  during `read-file-name' completion.
;;
;;  Using `read-file-name' is the most flexible way to read a file
;;  name in Emacs, and it is the traditional way.  Unless stated
;;  otherwise, "file-name completion", even in the Icicles doc, refers
;;  to `read-file-name' completion.
;;
;;  When `read-file-name' reads input, only the file name itself, not
;;  the directory portion, is used for matching.  The directory is
;;  understood to be the directory that is present in the minibuffer,
;;  or the value of variable `default-directory' (which you can change
;;  using command `cd', for instance) if there is no directory there.
;;
;;  Whether the value of `default-directory' is automatically inserted
;;  in the minibuffer is controlled by user option
;;  `insert-default-directory'.  But if you do not change the
;;  directory seen in the minibuffer then the behavior is the same
;;  whether or not the `default-directory' name is present in the
;;  minibuffer: it is the assumed directory.  If you prefer, you can
;;  delete the directory name first, using `M-k'.
;;
;;  With `read-file-name', you can thus use apropos completion to
;;  match a file-name substring, without needing to prefix the
;;  substring with `.*' in the minibuffer.  For example, to match the
;;  file named `favorite-foo-file.bar' in directory
;;  `/some/path/to/my/', you need not use `/some/path/to/my/.*foo'; it
;;  is sufficient to use either `foo' or `/some/path/to/my/foo'.
;;
;;  An additional feature of `read-file-name' in Icicle mode is that
;;  candidates that are directory names are highlighted in buffer
;;  `*Completions*' using face `icicle-special-candidate'.
;;
;;(@* "Function `completing-read'")
;;  ** Function `completing-read' **
;;
;;  Function `completing-read' is a general function for reading input
;;  with completion.  It is not specially designed for reading file
;;  names.  It knows nothing about files and file names. It knows
;;  nothing about remote file-name syntax.  When `completing-read'
;;  reads input, it makes no use of `default-directory'.  The
;;  completion candidates are treated as simple strings; they are not
;;  really treated as file names.
;;
;;  Icicles commands that use `completing-read' to read a file name
;;  typically read an absolute name, that is, a name that includes the
;;  directory portion.  This means that you can match against any part
;;  of the full name, including any directory components.  The
;;  directory portions of the candidate file names need not be the
;;  same - you can thus complete against a set of files in multiple
;;  directories.
;;
;;(@* "Remapping Vanilla File Commands to Icicles File Commands")
;;  ** Remapping Vanilla File Commands to Icicles File Commands **
;;
;;  By default, that is, if you do not change the value of user option
;;  `icicle-top-level-key-bindings', some commonly used Emacs commands
;;  for accessing files are remapped to Icicles commands.  For
;;  example, command `find-file', is by default remapped in Icicle
;;  mode to multi-command `icicle-file'.  Remapping means that any
;;  keys, such as `C-x C-f', that would normally be bound to
;;  `find-file' are instead bound to `icicle-file' when you are in
;;  Icicle mode.
;;
;;  Consult `icicle-top-level-key-bindings' to see how this works.  In
;;  particular, note that if you normally bind a different command to
;;  one of the keys mentioned here then the corresponding remapping in
;;  `icicle-top-level-key-bindings' will not have the advertised
;;  effect.
;;
;;  For example, if you normally use the key bindings provided by
;;  library `ffap.el' ("find file at point"), meaning that you invoke
;;  command `ffap-bindings', then for you `C-x C-f' is bound to
;;  `find-file-at-point', not `find-file'.  By default, option
;;  `icicle-top-level-key-bindings' does not remap
;;  `find-file-at-point' to `icicle-file', so even in Icicle mode `C-x
;;  C-f' remains bound to `find-file-at-point'.  If you want `C-x C-f'
;;  to be bound to `icicle-file', then change the `find-file' entry in
;;  `icicle-top-level-key-bindings' (or add another entry for
;;  `find-file-at-point').
;;
;;  What has just been pointed out for Icicles file commands applies
;;  more generally to all Icicle-mode command remapping provided by
;;  option `icicle-top-level-key-bindings'.
;;
;;(@* "Action Keys Bound Dynamically During File-Name Input")
;;  ** Action Keys Bound Dynamically During File-Name Input **
;;
;;  In addition to the usual Icicles key bindings, during file-name
;;  completion the Icicles commands that read file names bind the
;;  following keys dynamically in the minibuffer keymaps, so you can
;;  use them while to perform various operations on the fly when you
;;  input a file name.  These minibuffer bindings are also available
;;  for autofile bookmark commands.
;;
;;  * `C-backspace' (`icicle-up-directory') to navigate up the
;;    directory hierarchy.  It removes the last directory component
;;    (and any partial file name) from your minibuffer input.
;;
;;    (For Emacs versions before Emacs 22, this feature is available
;;    only for completion of absolute file names.)
;;
;;  * `C-c +' (`icicle-make-directory') to create a directory on the
;;    fly.
;;
;;  * `S-delete' to delete the file named by the current completion
;;    candidate.
;;
;;  * `M-|' (`icicle-all-candidates-list-alt-action') to open Dired on
;;    the currently matching file names.  That is, it opens a special
;;    Dired buffer that contains only the matching files.  You are
;;    prompted for the Dired buffer name.  See (@> "Alternative Actions").
;;
;;  * `C-x m' (`icicle-bookmark-file-other-window') to visit a
;;    bookmarked file or directory.  This is available only if you use
;;    library `Bookmark+' (`bookmark+.el').  It is a multi-command, so
;;    you can actually visit any number of file bookmarks.  When
;;    finished, you can continue with non-bookmark file-name
;;    completion.
;;
;;  * `C-x a +' or `C-x a -' to add or remove tags for a file.  These
;;    are delicious-style tags - any labels you like.  If there is not
;;    already an autofile bookmark for the file, then one is created.
;;    (The autofile bookmark is where the tags are persisted.)  You
;;    need library `Bookmark+' for this feature.  See also
;;    (@file :file-name "icicles-doc2.el" :to "Using Tagged Files").
;;
;;  * `C-x a a' to bookmark a file, that is, to create an autofile
;;    bookmark.  This is the same as `C-x a +' and `C-x a -', except
;;    that it does not prompt you for tags to add or remove.  You need
;;    library `Bookmark+' for this feature.
;;
;;  * `C-x C-t *' to narrow the current file-name completion
;;    candidates to those that are tagged with all of the tags you
;;    specify.  You are prompted for the tags.
;;
;;  * `C-x C-t +' - same as `C-x C-t *', except it narrows to the
;;    files tagged with any of the specified tags.
;;
;;  * `C-x C-t % *' to narrow the candidates to those whose tags all
;;    match a specified regexp.  You are prompted for the regexp.
;;
;;  * `C-x C-t % +' - same as `C-x C-t % *', except it narrows to the
;;    files that have any matching tag.
;;
;;  In many cases for Icicles multi-commands that read a file name,
;;  you can use `M-n' to pick up the file name at point, or if none
;;  then the name of the file you are currently visiting.
;;
;;  Note: Whether a command supports file-name globbing wildcards such
;;  as `*' is independent of whether it uses `read-file-name' or
;;  `completing-read'.  It is the command itself that offers such
;;  support or not.  Globbing takes place only after the file name
;;  (with wildcards) is read.  All Icicles commands that visit files
;;  (all of the commands mentioned in this section) let you use
;;  globbing wildcards.  Remember also that file-name globbing, with
;;  its special wildcards, has nothing to do with regexp completion
;;  matching.  See (@> "What About Special-Character Conflicts?") for
;;  more information about file-name globbing.
;;
;;(@* "Icicles Commands that Read File Names")
;;  ** Icicles Commands that Read File Names **
;;
;;  Icicles commands that use `read-file-name' include all
;;  multi-commands, such as `icicle-find-file', that are defined using
;;  `icicle-define-file-command'.  Vanilla Emacs command `find-file'
;;  is another example of a command that uses `read-file-name'.
;;
;;  Icicles commands that use `completing-read' to read file names
;;  include the multi-commands `icicle-find-file-absolute',
;;  `icicle-find-file-in-tags-table', `icicle-recent-file',
;;  `icicle-locate', `icicle-locate-file', and
;;  `icicle-locate-file-no-symlinks'.  These are defined using
;;  `icicle-define-command', not `icicle-define-file-command'.
;;
;;  There are also `-other-window' versions of all of the Icicles
;;  commands that read file names.
;;
;;  The Icicles commands that use `completing-read' to read file names
;;  have an additional feature: you can use a prefix argument to tell
;;  them to combine the last modification date with the file name, as
;;  a multi-completion - see (@> "Multi-Completions").  This means
;;  that you can easily look up files whose modification time or date
;;  matches some (regexp) criterion, such as being sometime in July
;;  2008.
;;
;;  When using a command that reads an absolute file name, remember
;;  that, to save space, you can use `C-x .' to toggle hiding of the
;;  common match portions of the candidates in `*Completions*'.  This
;;  portion is often a long directory substring.
;;
;;  Because this generally facilitates the use of absolute file-name
;;  commands, by default Icicles automatically does this for you.  You
;;  can unhide the common-match portion by hitting the toggle `C-x .'.
;;
;;  You can control which commands automatically turn on hiding of the
;;  expanded common match, by adding/removing property
;;  `icicle-hide-common-match' to/from the command symbol.  For
;;  example, if you do not want `icicle-locate-file' to hide the
;;  common match then add this to your init file (`~/.emacs'):
;;
;;    (put 'icicle-locate-file 'icicle-hide-common-match nil)
;;
;;  On the other hand, you can make any command hide the common match
;;  by putting a non-`nil' value on its symbol.
;;
;;(@* "`icicle-file', `icicle-find-file', `icicle-find-file-absolute'")
;;  *** `icicle-file', `icicle-find-file', `icicle-find-file-absolute' ***
;;
;;  Multi-command `icicle-file' is bound, by default, to `C-x C-f' in
;;  Icicle mode, thus taking the place of `find-file'.  It combines
;;  multi-commands `icicle-find-file' and `icicle-find-file-absolute'.
;;  With no prefix argument, it matches relative file names; with a
;;  prefix argument, it matches absolute names (as ordinary strings).
;;  With a negative prefix argument, you can match also the last
;;  modification date.
;;
;;  An additional feature of these commands is that candidates that
;;  are directory names are highlighted in buffer `*Completions*'
;;  using face `icicle-special-candidate'.
;;
;;  Another feature is that if you use a prefix argument when acting
;;  on an individual file-name candidate then the file is visited in
;;  read-only mode.  The same invocation of `C-x C-f' can thus open
;;  multiple files, some in read-only mode, some not.
;;
;;  (There are also multi-commands `icicle-find-file-read-only' and
;;  `icicle-find-file-abs-read-only', unbound by default, which are
;;  the same as `icicle-find-file' and `icicle-find-file-absolute',
;;  except that they visit files in read-only mode.  For
;;  `icicle-find-file-read-only', the prefix-argument behavior is
;;  flipped: with a prefix argument when you act on a candidate file
;;  it is not visited read-only.)
;;
;;(@* "Match File Names and File Content Too")
;;  *** Match File Names and File Content Too ***
;;
;;  Starting with Emacs 23, the Icicles commands that read file names
;;  let you optionally provide a regexp pattern to match against file
;;  content.  In this they are similar to the buffer-switching
;;  multi-command `icicle-buffer'.
;;
;;  Versions of the file-finding commands are also available that do
;;  not let you search file contents.  Their file names contain
;;  `-no-search'.  Prior to Emacs 23, these are all that are
;;  available, so commands such as `icicle-file' are aliased to them.
;;
;;  (For Emacs 23 and later, commands such as `icicle-file' are
;;  actually aliased to commands that let you search content, and
;;  which have `-of-content' in their name.)
;;
;;  If you provide a pattern to match file content then all files
;;  whose names match the file-name part of your input are searched
;;  for the content pattern.  Icicles looks only for a single
;;  content-match within each file it tests.  Choosing a candidate
;;  file just visits it normally - it does not move to that tested
;;  match or to any other content-match position.  Matching is used
;;  only to filter candidate files.
;;
;;  However, if your input includes a content-matching part that
;;  matches, that part is automatically added to the Isearch regexp
;;  history, `regexp-search-ring', whenever you hit `S-TAB' to
;;  complete your input.  This means that when you visit the file you
;;  can immediately search for matches using `C-M-s' or `C-M-r'.
;;
;;  (You can also work in the other direction, reusing an Isearch
;;  regexp as a content-matching regexp.  See
;;  (@> "Using Completion to Insert Previous Inputs: `M-o'").)
;;
;;  Content-searching is obviously much more costly than file-name
;;  matching, so clearly if you can provide some information about the
;;  file name, that improves performance.  IOW, the more you can limit
;;  the number of files to search, the better.
;;
;;  To search file content, the candidate files are visited, that is,
;;  buffers are created for them and searched.  By default, after the
;;  command is finished these buffers are killed, except for those you
;;  actually chose as completion candidate(s) and any that existed
;;  prior to invoking the command.
;;
;;  This automatic extra-buffers cleanup is controlled by option
;;  `icicle-kill-visited-buffers-flag'.  But providing a prefix
;;  argument to `icicle-file' flips the behavior specified by that
;;  option for the command duration.  (This is not true for the
;;  commands that read absolute file names.  For them, a prefix
;;  argument has a different meaning.)
;;
;;  You can use option `icicle-file-skip-functions' to specify
;;  patterns for file names to exclude from content-searching when you
;;  provide a content-matching pattern.  By default, image files and
;;  byte-compiled Emacs-Lisp files are ignored for content-searching.
;;
;;  If your input contains a content-matching part then, by default,
;;  it is ignored for completion of a directory-name candidate.  The
;;  behavior is governed by options `find-file-run-dired' and
;;  `icicle-file-search-dir-as-dired-flag'.  If both of these options
;;  are non-`nil' then the directory is visited in Dired mode during
;;  completion, and the Dired listing is searched as the content.
;;
;;  In a Dired buffer, there are related content-matching
;;  multi-commands that you can use to visit marked files and
;;  subdirectories whose content matches a regexp.
;;
;;  * `icicle-visit-marked-file-of-content', bound to `C-S-f', aka
;;    `C-F', and `C-M-S-f', aka `C-M-F', for other-window
;;
;;  * `icicle-visit-marked-file-of-content-recursive', bound to `M-+
;;    C-F', and `M-+ C-M-F' for other-window
;;
;;  The latter visit not only the files that are marked in the current
;;  directory, but also those that are marked in any marked
;;  subdirectories, and sub-subdirectories etc., recursively.  They
;;  are available only if you also use library `Dired+'.
;;
;;  File content-matching is available for all Icicles commands that
;;  read file names, including `icicle-recent-file',
;;  `icicle-locate-file', and `icicle-locate'.
;;
;;  Using a negative prefix argument when you invoke `icicle-file'
;;  (`C-x C-f'), or any prefix argument when you invoke a command that
;;  reads absolute file names, means that the completion candidates
;;  have up to three parts: the file name, followed by the last
;;  modification date, followed by the file content.
;;
;;  For example, these each match files with names matching `ici',
;;  last modification dates matching `2013', and contents matching
;;  `define-file':
;;
;;    C-- C-x C-f ici C-M-j 2013 C-M-j define-file
;;
;;    M-x icicle-locate-file ici C-M-j 2013 C-M-j define-file
;;
;;  The parts of your input pattern that match any of these candidate
;;  parts can be empty.  For example, this matches files with any
;;  name, whose last modification date matches `2013 07', and whose
;;  contents match `recent file':
;;
;;    C-- C-x C-f C-M-j 2013 07 C-M-j recent file
;;
;;  See Also:
;;
;;  * (@> "Visit Recent Files or Files for Emacs Tags")
;;  * (@> "Find Files Anywhere, Without Knowing Where")
;;  * (@> "Chapter & Verse: Searching Named Containers")
;;
;;(@* "Cycling into Subdirectories")
;;  *** Cycling into Subdirectories ***
;;
;;  The Icicles commands that use `read-file-name' to read a file or
;;  directory name visit the files or directories you choose.  If you
;;  act on a directory-name completion candidate (e.g. using `RET' or
;;  `C-RET') it is visited using Dired.  And cycling moves past a
;;  directory candidate to the next candidate, the name of a sibling
;;  file or directory.
;;
;;  But sometimes you want not to visit a candidate directory but to
;;  descend into it, to access its own files or subdirectories using
;;  completion.  To do that, you can do either of the following to
;;  expand a directory candidate:
;;
;;  * `C-M-/'
;;  * `C-e TAB' (or `C-e S-TAB')
;;
;;  If you prefer, you can have Icicles cycle into subdirectories
;;  whenever you choose them using `RET' (or `mouse-2' in
;;  `*Completions*'), by setting option
;;  `icicle-find-file-expand-directory-flag' to non-`nil'.
;;
;;  The effect of `RET' is then the same as `C-M-/' - directory
;;  candidates are not visited using Dired.  This behavior does not
;;  occur for multi-command candidate actions such as `C-RET',
;;  `C-mouse-2', and `C-down' - those continue to open Dired on the
;;  candidate directory.
;;
;;  You can toggle this option using `C-x /'.
;;
;;(@* "Visit Recent Files or Files for Emacs Tags")
;;  *** Visit Recent Files or Files for Emacs Tags ***
;;
;;  You can use `icicle-recent-file' to open any file that you have
;;  visited recently, perhaps in a previous Emacs session.
;;
;;  Because Icicles lets you easily sort and filter candidates,
;;  consider customizing option `recentf-max-saved-items' to a large
;;  number, such as 1000.  The default number (20) is very small
;;  because it assumes that you will access the candidates using a
;;  menu.
;;
;;  Commands `icicle-find-file-in-tags-table' and
;;  `icicle-find-file-in-tags-table-other-window' let you visit files
;;  that are listed in the current Emacs tags table.  You can think of
;;  these potential completion candidates as all of the files in a
;;  project defined by the tags table.
;;
;;  `icicle-recent-file' and `icicle-find-file-in-tags-table' are like
;;  `icicle-find-file-absolute': `completing-read' is used, not
;;  `read-file-name' (file names are treated as just strings), and
;;  (for Emacs 23 and later) you can match both file names and file
;;  contents.
;;
;;(@* "Find Files Anywhere, Without Knowing Where")
;;  *** Find Files Anywhere, Without Knowing Where ***
;;
;;  You can use `icicle-locate' or `icicle-locate-file' to find a file
;;  when you do not know precisely what directory it is in.  The
;;  former requires GNU/Linux or UNIX command `locate', to work.  The
;;  latter does not require any external program.
;;
;;  Because it takes advantage of `locate' having indexed files on
;;  your file system, `icicle-locate' can be much faster than
;;  `icicle-locate-file'.  Otherwise, these two Icicles commands work
;;  similarly.
;;
;;  `icicle-locate' and `icicle-locate-file' are like
;;  `icicle-find-file-absolute': file names are absolute, and (for
;;  Emacs 23 and later) you can match both file names and file
;;  contents.  Of course, since these commands work with all of the
;;  files in and under a given directory, this can mean many, many
;;  files, so generally you will want to use an input pattern that
;;  also matches file names.
;;
;;  Since it does not use an index, `icicle-locate-file' looks
;;  throughout a given directory, including throughout all of its
;;  subdirectories.  Command `icicle-locate-file-no-symlinks' is the
;;  same, except that it does not follow symbolic links.  Both of
;;  these commands respect option `icicle-ignored-directories', which
;;  is a list of directories to ignore - by default, version-control
;;  directories.
;;
;;  By default, the target directory for `icicle-locate-file' is the
;;  current directory, but if you supply a non-negative numeric prefix
;;  argument (non-positive means include the date), then you are
;;  prompted for the directory to search.
;;
;;  If you use a plain prefix argument (`C-u') or a double plain
;;  prefix argument (`C-u C-u') then you are prompted to choose a set
;;  of directories to use.  This choosing uses multi-command
;;  `icicle-directory-list'.  Icicles searches for file-name matches
;;  under each of the directories you choose.  For `C-u C-u', the
;;  candidate multi-completions include the last file-modification
;;  date.
;;
;;  In addition, if you turn on the use of proxy candidates (`C-M-_'),
;;  `icicle-directory-list' also offers proxy candidates that are
;;  variables, such as `load-path', whose values are lists that
;;  include directories.  Choosing a proxy candidate adds all of its
;;  directories to the set to be searched.
;;
;;  If you use the root of your file system as the search directory,
;;  then the Icicles file-locating commands will match completion
;;  candidates anywhere in your file system.
;;
;;  This can be quite useful.  It gives you much of the power of the
;;  Unix `find' command just for completing input.  And with
;;  incremental completion (see (@> "Icompletion")), you can see what
;;  matches your input as you type.
;;
;;  Obviously, if you use your entire file system as the set of
;;  completion candidates and you use `icicle-locate-file' (because
;;  you do not have available the external program `locate'), then
;;  gathering and matching such a large set of file names can take
;;  some time.
;;
;;  On my hard drive, for instance, there are 36 GB full of files, and
;;  it takes about 40 seconds to gather all of the file names.  In
;;  spite of this inconvenience, this functionality can be useful.
;;  And of course searching a shallower directory tree presents less
;;  of a performance penalty - you pay for what you get.
;;
;;  However, even if you do not have command `locate', there is a way
;;  of having your cake and eating it too.  You can gather all of the
;;  file names in your file system once, and save that list of
;;  completion candidates to a cache file on disk, as a snapshot.  See
;;  (@> "Persistent Sets of Completion Candidates"), for how to do
;;  this.
;;
;;  You can also do a few things to improve performance when using a
;;  command like `icicle-locate-file' that involves a large set of
;;  completion candidates: Turn off Icomplete mode, Icicles
;;  incremental completion, and sorting.  You can use keys `C-M-#',
;;  `C-#', and `C-,', respectively, to do this.  But because it is
;;  likely that you will want to do these things most, if not all, of
;;  the time, Icicles does this for you automatically, by default.
;;
;;  User options `icicle-icomplete-mode-max-candidates' and
;;  `icicle-sorting-max-candidates' automatically turn off Icomplete
;;  mode and sorting when there are more completion candidates than
;;  the option values, and then they turn these back on when there are
;;  fewer candidates.  This is the case regardless of the current
;;  command.
;;
;;  In addition, Icicles multi-commands that typically have a large
;;  number of candidates turn off Icomplete mode and Icicles
;;  incremental completion, and do not turn them on again, regardless
;;  of the number of current candidates at any moment.  When this
;;  happens, a message informs you.  Just use `C-M-#' and `C-#' to
;;  turn these back on.
;;
;;  You can control which commands do this, by adding/removing
;;  properties `icicle-turn-off-icomplete-mode' and
;;  `icicle-turn-off-incremental-completion' to/from the command
;;  symbol.  For example, if you do not want `icicle-locate-file' to
;;  turn either of these off then add this to your init file
;;  (`~/.emacs'):
;;
;;    (put 'icicle-locate-file 'icicle-turn-off-icomplete-mode nil)
;;    (put 'icicle-locate-file 'icicle-turn-off-incremental-completion nil)
;;
;;  On the other hand, you can make any command turn one of these off
;;  by putting a non-`nil' value on its symbol.
;;
;;(@* "Absolute File Names and Different Directories")
;;  ** Absolute File Names and Different Directories **
;;
;;  Since `completing-read' has no understanding of file-name syntax,
;;  including remote file-name syntax, `icicle-find-file-absolute'
;;  (`C-u C-x C-f') and similar commands are similarly ignorant.  (You
;;  can nevertheless use `C-.' with these Icicles commands, to toggle
;;  respect of `completion-ignored-extensions'.)  In particular, these
;;  commands will not let you complete to a remote file name if the
;;  current directory is local.  They also will not let you complete
;;  to a file name in a different local directory.
;;
;;  Because all Icicles commands that read file names use lax
;;  completion, you can nevertheless visit a file in a different
;;  directory (remote or local) from the current one, even though you
;;  cannot complete your input to such a name.  That is, you can
;;  always use `RET' with any file name as minibuffer input.
;;
;;  So how can you complete your input to an absolute file-name in a
;;  different directory?  By retrieving a saved candidate set that has
;;  such absolute names and then completing against that set.  For
;;  example, you can retrieve a set that represents files on a remote
;;  machine and complete to their names even from a local directory.
;;  All that counts for `completing-read' is that your input can match
;;  candidates, where that matching is ordinary (apropos or prefix)
;;  string matching.
;;
;;  To create such a saved set of names, you can visit the directory
;;  (perhaps remote) that contains the files and then use `C-u C-x
;;  C-f' and `C-}' to save the candidates.  You can later retrieve
;;  this saved set for completion, no matter what the current
;;  directory is.  As another example, you can use
;;  `icicle-locate-file' in a remote directory to create a saved set
;;  that includes remote files that are all somewhere under that
;;  remote directory.
;;
;;  In addition, you can add more file names to an existing saved set
;;  using `C->', `C-)', `M-mouse-3', `insert', or `M-S-mouse-2' - see
;;  (@> "Sets of Completion Candidates"). The added names can come
;;  from a different directory than files already in the saved set. In
;;  this way, you can build up a saved set that includes files from
;;  any directories, some of which can be local and some remote, some
;;  remote from one host, some remote from another, and so on. You can
;;  create a saved set with any mix of absolute file names from any
;;  locations.
;;
;;  Remember this:
;;
;;  * To create and add to a saved set of absolute file names, use a
;;    command that expects absolute file names, and do this from the
;;    directory that contains the files you want to add.
;;
;;  * When you retrieve a saved set of file-name candidates for
;;    completion, use a command that expects the same kind of file
;;    names, relative or absolute, as the saved names.  For example,
;;    if you save a set of project files that are spread over
;;    different directories (and hence are absolute), then retrieve
;;    that candidate set using, say, `C-u C-x C-f' (absolute), not
;;    `C-x C-f' (relative).
;;
;;  Finally, although the commands that read absolute file names are
;;  essentially ignorant of directory hierarchies and of file names as
;;  such, so that they treat their candidates only as simple strings,
;;  most of these commands nevertheless define their domain of
;;  possible file-name candidates relative to some starting directory.
;;
;;  This is the case for all commands that read absolute file names,
;;  except for the `icicle-locate*' family.  You can use `C-c C-d'
;;  (think UNIX command `cd') during completion to change the current
;;  working directory (`default-directory') on the fly.  You are
;;  prompted for the directory.  The domain of possible candidates is
;;  recomputed relative to the new `default-directory'.
;;
;;  Use `C-c C-d' this way as many times as you like.  You can use
;;  this feature to add file names from different directories to a
;;  saved set of candidates.  When the command is finished, the
;;  original `default-directory' is restored.
;;
;;  See Also:
;;
;;  * (@> "Persistent Sets of Completion Candidates") for information
;;    about saving a set of file names persistently
;;  * (@> "Sets of Completion Candidates") for information about
;;    creating, saving, and retrieving sets of file names
;;  * (@> "Dealing With Large Candidate Sets") for ways to deal with a
;;    large number of candidates
;;  * (@> "Multi-Completions")
;;  * (@file :file-name "icicles-doc2.el" :to "Support for Projects")
;;    for more about `icicle-find-file-in-tags-table'
;;  * (@> "Completion On Demand") for information about on-demand
;;    insertion of file names, using completion, from any minibuffer
;;  * (@> "Match Multiple Things Together") about content-searching
;;  * (@> "Buffer-Name Input") about visiting recently used files or
;;    files whose names have been cached, and about content-searching
;;    them
;;  * (@file :file-name "icicles-doc2.el" :to "Using Tagged Files")
;;    for more information about tagging files
 
;;(@* "Persistent Sets of Completion Candidates")
;;
;;  Persistent Sets of Completion Candidates
;;  ----------------------------------------
;;
;;  Section (@> "Sets of Completion Candidates") describes how you can
;;  save the current set of completion candidates and reuse it later.
;;  This is not a persistent save, however; the candidates are simply
;;  saved in variable `icicle-saved-completion-candidates' for the
;;  duration of your Emacs session (or until you save candidates
;;  again).
;;
;;  You can save the current set of completions (whatever it is)
;;  persistently by supplying a plain prefix argument (`C-u') when you
;;  use `C-M->' (`icicle-candidate-set-save') during completion.
;;
;;  Alternatively, you can use `C-}', bound to
;;  `icicle-candidate-set-save-persistently', which does the same
;;  thing.  To retrieve completion candidates that were previously
;;  saved to a cache file, so that they become the current set of
;;  candidates, use either `C-u C-M-<' or `C-{'
;;  (`icicle-candidate-set-retrieve' or
;;  `icicle-candidate-set-retrieve-persistent').
;;
;;  Tip: Suppose you have already saved a set of candidates, but not
;;       persistently, and you now want to write this saved set to a
;;       cache file.  Use `C-M-<' followed by `C-}'.  That is,
;;       retrieve the saved candidates and then save the retrieved
;;       candidates persistently.
;;
;;  Note that using a numeric prefix argument (`C-u' with a number)
;;  with `C-M->' and `C-M-<' saves or retrieves a
;;  completion-candidates set using a variable that you name, not a
;;  cache file.  See (@> "Sets of Completion Candidates").
;;
;;  In addition to persistently saving a candidate set in a file,
;;  which is described in this section, if you also use library
;;  `Bookmark+' then you can alternatively save a set of Icicles
;;  search candidates as a bookmark.  You "jump" to the bookmark to
;;  restore those search results during another Icicles search.
;;
;;(@* "Saving Candidates in Cache Files")
;;  ** Saving Candidates in Cache Files **
;;
;;  If you have used the Emacs file-name cache (see the Emacs manual,
;;  node "File Name Cache"), then you have already used a cache file
;;  of (file-name) completion candidates.  In vanilla Emacs, you use
;;  `C-TAB' (`C-tab') during file-name input to complete to a cached
;;  file name.  In Icicles, you use `C-{'.
;;
;;  In Icicles, the cached candidates are not limited to file names,
;;  and you can have any number of cache files, to save different sets
;;  of completion candidates.  Each cache file saves the set of
;;  candidates that was current when you created (saved) the set.
;;
;;  The fact that a cache file can contain just those candidates that
;;  were current when you saved it is a considerable advantage, when
;;  combined with Icicles features for sculpting the current set of
;;  matching candidates.  As far as I know, Icicles is the only
;;  package to offer this feature.  You spend a few moments to
;;  fine-tune a set of candidates, using, for example, `M-*' or
;;  `S-SPC', `C-~', and `delete', and then save it for later use.
;;  From then on, you can match against exactly those candidates
;;  anytime you want.
;;
;;  For example, you might have a software project that involves only
;;  certain directories and perhaps only certain kinds of files in
;;  those directories are of interest as completion candidates.  Those
;;  directories and files can even be in disparate locations.
;;
;;  Start with command `icicle-locate-file' (or
;;  `icicle-locate-file-no-symlinks').  Then use progressive
;;  completion to match the directories and files you want and chip
;;  away at those you do not want.  Once you get just the set you need
;;  for your project, save that set using `C-}'.  You can have any
;;  number of saved sets, for different projects or different purposes
;;  in the same project.
;;
;;  You name the sets of saved candidates, and these names are
;;  associated with the cache files in user option
;;  `icicle-saved-completion-sets'.  This is an alist of entries, each
;;  of which is of the form (SET-NAME . CACHE-FILE-NAME).  You can
;;  customize this option, or set it in your init file (`~/.emacs').
;;
;;  You can use command `icicle-add/update-saved-completion-set' to
;;  add a new set to `icicle-saved-completion-sets' or update
;;  (replace) an existing such set.  You can use command
;;  `icicle-remove-saved-completion-set' to remove a saved set.
;;
;;  As an alternative to customizing `icicle-saved-completion-sets' or
;;  using command `icicle-add/update-saved-completion-set', you can
;;  simply try to save a set of completion candidates persistently,
;;  using `C-u C-M->' or `C-}'.  You are then prompted for the names
;;  of the candidate set and cache file to use, and the names you
;;  enter are automatically entered in option
;;  `icicle-saved-completion-sets'.  That option is automatically
;;  saved to your `custom-file' (or init file, `~/.emacs'), so the
;;  next time you use Emacs you can retrieve any saved set of
;;  candidates that you like.
;;
;;  When you try to retrieve a persistent set of completion
;;  candidates, you are similarly prompted for the candidate-set name
;;  and the cache-file name.
;;
;;  In addition to saving the current set of completion candidates to
;;  a cache file, you can add individual strings as future completion
;;  candidates to any cache file, and you can remove candidates from a
;;  cache file individually.  You do this using commands
;;  `icicle-add-entry-to-saved-completion-set' and
;;  `icicle-remove-entry-from-saved-completion-set'.
;;
;;  Adding an individual candidate is similar to using the Emacs
;;  file-name cache commands that add file names to the cache, but it
;;  adds only a single candidate.  For file names, adding a directory
;;  name effectively provides completion for all of its files as well,
;;  so there is no need to add each file name as well as the directory
;;  name.  Alternatively, you can always use `C-}' to add all file
;;  names that match your current input.
;;
;;(@* "Filesets and Icicles Saved Completion Sets")
;;  ** Filesets and Icicles Saved Completion Sets **
;;
;;  Starting with release 22, GNU Emacs includes a filesets feature
;;  that lets you create named sets of file names, called "filesets".
;;  It is a powerful feature, letting you define such sets by
;;  intension, using regexp patterns, as well as by extension, listing
;;  file names explicitly.  You can easily use a fileset to define a
;;  project of files.
;;
;;  Icicles lets you use an Emacs fileset any time you can use an
;;  Icicles saved completion set, provided that option
;;  `icicle-filesets-as-saved-completion-sets-flag' is non-`nil'.
;;
;;  That is, you can retrieve fileset file names as the current set of
;;  completion candidates or save the current completion candidates to
;;  a fileset.  Provided
;;  `icicle-filesets-as-saved-completion-sets-flag' is non-`nil', you
;;  can always choose a fileset as the set to retrieve.  To save to a
;;  fileset, use a prefix argument with `C-}' or a zero prefix
;;  argument with `C-M->'.  Saving candidates to a fileset gives you
;;  an alternative to customizing option `filesets-data'.
;;
;;  Being able to use an Emacs fileset in place of an Icicles saved
;;  set lets you use filesets in additional ways.  For example, it
;;  lets you open Dired on only the files in a fileset, for easy
;;  manipulation of the member files.  Conversely, you can save all of
;;  the marked files in a Dired buffer as a fileset. See
;;  (@file :file-name "icicles-doc2.el" :to "Icicles Dired Enhancements").
;;
;;  Beyond letting you use a fileset in place of a persistent Icicles
;;  saved completion set, you can include filesets in such saved
;;  Icicles sets.  That is, you can save one or more filesets of any
;;  kind (`:files', `:tree', etc.) in an Icicles persistent saved set
;;  (cache file).  When you then retrieve such a saved set, all of the
;;  file names specified by all of the included filesets become
;;  completion candidates.
;;
;;  For example, this could be a saved Icicles set that combines a
;;  `:tree' fileset with an explicit `:files' fileset and with two
;;  additional files:
;;
;;  ((:fileset "set1" (:tree "~/my/dir" "^ici.+\\.el$"))
;;   (:fileset "set2" (:files "dired+.el" "c:/my/dir/buff-menu+.el"))
;;   "c:/some/other/dir/foobar.el"
;;   "c:/somewhere/else/toto.el")
;;
;;  This is a great way to put together a project of files from
;;  different directory trees.  And even aside from the use of such a
;;  saved set for completion, this combining of filesets is something
;;  that you cannot do with Emacs filesets alone, as far as I know -
;;  you cannot combine different filesets into super filesets, and a
;;  given fileset can specify files in only one way (`:files',
;;  `:tree', etc.).  Icicles gives you a way to associate related
;;  filesets and use them together as a single set.
;;
;;  You can use commands
;;  `icicle-remove-entry-from-saved-completion-set' and
;;  `icicle-add-entry-to-saved-completion-set' to remove a fileset
;;  from an Icicles saved set or add a fileset to a saved set.  To
;;  add, use a prefix argument to tell
;;  `icicle-add-entry-to-saved-completion-set' that you are adding a
;;  fileset and not a single completion candidate.  To add a single
;;  file (default: the current buffer's file) to a fileset, use
;;  command `icicle-add-file-to-fileset'.
;;
;;(@* "Improving Performance with Persistent Sets")
;;  ** Improving Performance with Persistent Sets **
;;
;;  There are two independent reasons that using a persistent set of
;;  file names can improve performance:
;;
;;  * Avoiding remote file-name completion.  You can complete your
;;    input against remote file names without using Tramp and thus
;;    without accessing the remote file system.  (Once you have chosen
;;    the file you want, visiting it of course makes a remote access.)
;;
;;  * Avoiding generation of a large completion set.  Retrieving a
;;    list of file names is much, much faster than generating such a
;;    list.  So generate once and retrieve often, from a cache.
;;
;;  These are covered in the next two sections.
;;
;;(@* "Avoid Remote File-Name Completion")
;;  *** Avoid Remote File-Name Completion ***
;;
;;  When you complete the name of a remote file, Tramp accesses the
;;  remote file system to see which matching files exist.  This takes
;;  time.  The completion itself is complicated - it involves parsing
;;  the remote file name and calling upon various file handlers.  But
;;  the greatest time spent is in accessing the remote machine.
;;
;;  When you retrieve a (persistently) saved set of file names during
;;  completion, you are telling Emacs that these are the candidates
;;  you want to complete against.  You are not asking Emacs (Tramp) to
;;  tell you what the possible candidates are; you are telling it.
;;  (Obviously you will want to save the completions in a file on the
;;  local machine, so retrieval itself takes no time.)
;;
;;  After retrieving the saved candidates as the only possible ones,
;;  you might type some input and complete it (`TAB' or `S-TAB') to
;;  narrow your choices.  Or you might not bother with completion but
;;  instead pick one of the candidates using `mouse-2' or by cycling
;;  to it and using `RET'.
;;
;;  You can use either relative or absolute file-name completion with
;;  remote file names.  Relative name completion as provided by
;;  `read-file-name' (via `C-x C-f', for example) always involves
;;  Tramp (or ange-ftp, prior to Emacs 22).  When using relative name
;;  completion, you can save time in these ways:
;;
;;  * Turn off incremental completion (using `C-#'), so that Tramp is
;;    used only when you hit `TAB' or `S-TAB', not with each character
;;    you type or delete!
;;
;;  * Use `mouse-2', or cycle and use `RET', so that you avoid
;;    completion altogether.  Tramp is then used only to access the
;;    chosen file.
;;
;;  If you use absolute file-name completion as provided by
;;  `completing-read' (via `C-u C-x C-f', for example), then you need
;;  not worry about turning off incremental completion or avoiding
;;  completion by cycling or using `mouse-2'.  This is because
;;  completion is entirely local - `completing-read' has no notion of
;;  files, let alone remote files.
;;
;;  In addition, if you use absolute file-name completion then you
;;  need not bother to type the (long) remote file-name prefix to get
;;  into the right directory for completion.  Again, `completing-read'
;;  has no notion of files or directories - it just completes an input
;;  pattern against string candidates.  Just type a substring or other
;;  regexp and then hit `S-TAB'.
;;
;;  In general, using absolute file names (`C-u C-x C-f') is the way
;;  to go when dealing with remote files.  There is no need to forego
;;  the advantages of Icicles completion.  On the other hand, if you
;;  are going to work in a directory on a remote machine for some time
;;  using files other than those in some saved completion set, then
;;  you might want to use relative file names (`C-x C-f').
;;
;;(@* "Avoid Generating A Large Completion Set")
;;  *** Avoid Generating A Large Completion Set ***
;;
;;  Section (@> "File-Name Input and Locating Files Anywhere") tells
;;  you how you can locate any file in your file system.  If you save
;;  the set of all file names persistently, you will increase the
;;  performance of using it - it is much faster to retrieve the list
;;  of all file names than it is to generate it.
;;
;;  With 36 GB of files in my file system, my all-file-system cache
;;  file is 20 MB, and retrieving the file-name completions from it
;;  takes only a few seconds.  With this feature, Icicles essentially
;;  gives you the functionality of the Unix `locate' command, but with
;;  the addition of real-time regexp matching.  Here is all you do:
;;
;;    M-x icicle-locate-file RET
;;    C-#        ; Once or twice: turn off incremental completion.
;;    C-{        ; Retrieve all file names from your cache file.
;;               ; You are prompted for the set name and file name.
;;    foo.*bar   ; Regexp to match names with `foo' followed by `bar'.
;;    S-TAB      ; Update `*Completions*' display (because of `C-#').
;;
;;  Of course, once you have retrieved a set of candidates from your
;;  cache file, you can access them again without re-reading the file.
;;  When they are retrieved from your cache they are saved in variable
;;  `icicle-saved-completion-candidates', so the next time you want to
;;  use them, just retrieve them from this variable with `C-M-<'.
;;
;;  See Also:
;;
;;  * (@> "File-Name Input and Locating Files Anywhere") for
;;    information about relative vs absolute file names and about
;;    finding files located anywhere in your file system
;;
;;  * (@> "Icompletion") for information about `C-#' (toggle
;;    incremental completion)
;;
;;  * (@> "Sets of Completion Candidates") for information about
;;    `C-M->' (save current candidates)
;;
;;  * (@> "Dealing With Large Candidate Sets")
;;
;;  * (@file :file-name "icicles-doc2.el" :to "Icicles Bookmark Enhancements")
;;    for information about using autofile bookmarks, which are
;;    another form of persistent file names
 
;;(@* "Dealing With Large Candidate Sets")
;;
;;  Dealing With Large Candidate Sets
;;  ---------------------------------
;;
;;  One of the advantages Icicles provides is the ability to deal with
;;  large sets of completion candidates with ease.  There are other
;;  libraries that also let you cycle among various choices of
;;  different kinds (buffers, files, and so on), but cycling quickly
;;  loses its effectiveness as the number of candidates increases.
;;
;;  Icicles apropos matching lets you work with a large initial set of
;;  candidates by filtering them, quickly reducing the number
;;  candidates to cycle through.  Until you get used to Icicles, you
;;  will be surprised at your ability to manipulate even humongous
;;  sets of choices.
;;
;;  Nevertheless, there can be times when a candidate set is so large
;;  that you need to use a few tricks to deal with it efficiently.
;;  There are two main things that take time when dealing with a large
;;  set: computing the set and displaying it (with highlighting) in
;;  buffer `*Completions*'.  In particular, incremental completion
;;  display is costly because it does both of these, recompute the set
;;  and redisplay it, each time you type or delete a character in the
;;  minibuffer.
;;
;;  Here are some tips to improve performance with a large set of
;;  candidates:
;;
;;  * Turn off incremental completion display in buffer
;;    `*Completions*'.  You can do this on the fly at any time by
;;    using `C-#' in the minibuffer - use `C-#' again to turn it back
;;    on.  See (@> "Icompletion").
;;
;;  * Turn off Icomplete mode.  You can use `C-M-#' in the minibuffer
;;    to do this anytime.
;;
;;  * Turn off candidate sorting, or avoid sort orders that are
;;    particularly costly.  Use `C-,' to change the sort order or turn
;;    sorting off.
;;
;;  * Tell particular Icicles multi-commands to turn off incremental
;;    completion or Icomplete mode, by giving their symbols property
;;    `icicle-turn-off-incremental-completion' or
;;    `icicle-turn-off-icomplete-mode'.
;;
;;  * Compute a large candidate set only once, cache the result, and
;;    reuse it later by reading the cache instead of recomputing.
;;    This is useful, for instance, for the candidate set of all files
;;    on your file system.  You can cache a set of candidates in
;;    either a variable (quickest, but not persistent) or a disk file
;;    (slower, persistent).
;;    See (@> "Persistent Sets of Completion Candidates").
;;
;;  * Compute a large candidate set (and perhaps cache it or filter
;;    it) without displaying it in `*Completions*', by using `C-M-TAB'
;;    (`C-M-tab') or `C-M-S-TAB' (`C-M-S-tab') instead of `TAB' or
;;    `S-TAB', respectively.  These are bound to commands
;;    `icicle-prefix-complete-no-display' and
;;    `icicle-apropos-complete-no-display'.  For example, when
;;    initially computing the set of all files on your file system for
;;    `C-u M-x icicle-locate-file', use `C-M-S-TAB' to compute the
;;    set, then use `C-}' to save it to a cache file - you need never
;;    display it.
;;
;;    (The documentation refers to the keys that do this as
;;    `C-M-S-TAB' and `C-M-TAB'.  Actually, this is only by default.
;;    You can customize this, using options
;;    `icicle-apropos-complete-no-display-keys' and
;;    `icicle-prefix-complete-no-display-keys'.)
;;
;;  See Also:
;;
;;  * (@> "Icompletion") for information about incremental completion
;;    and Icomplete mode
;;
;;  * (@> "Sorting Candidates and Removing Duplicates") for
;;    information about sorting candidates
 
;;(@* "History Enhancements")
;;
;;  History Enhancements
;;  --------------------
;;
;;  This section is about accessing and reusing previous input that
;;  you have typed in the minibuffer.
;;
;;(@* "What Input, What History?")
;;  ** What Input, What History? **
;;
;;  First, what is meant by "input" and "input history"?  In vanilla
;;  Emacs and in this doc, "minibuffer history" and "input history"
;;  generally refer to input that you have typed (or cycled or
;;  completed) in the minibuffer and then entered using `RET' (or
;;  `S-RET').  Emacs provides different history lists for this,
;;  depending on the kind of input.  The most general such list is the
;;  value of variable `minibuffer-history'.
;;
;;  But what about input that you type in the minibuffer (e.g. during
;;  completion) but that you do not enter with `RET'?  That is not
;;  recorded in any standard history list, so you cannot recall it
;;  using `M-p' and `M-n'.
;;
;;  The Icicles doc speaks ambiguously of "minibuffer input".  This
;;  always refers to something that you type in the minibuffer, but
;;  sometimes it means input that you enter with `RET' and sometimes
;;  it does not.  The context and the use of phrases such as "entered"
;;  and "entered with `RET'" should make clear what is meant.  Input
;;  that you type during completion but that you do not necessarily
;;  enter is sometimes referred to in the Icicles doc as "completion
;;  input".
;;
;;  Icicles also records any input that you type during completion but
;;  you do not enter (`RET' or `mouse-2'), because you might need to
;;  retrieve it.  This can be because you cycled among completion
;;  candidates (cycling replaces the input you type) or because
;;  Icicles has expanded your input to the common match among all
;;  matching candidates (see option
;;  `icicle-expand-input-to-common-match').
;;
;;  You can retrieve the last such unentered input during completion
;;  using `C-l' (`icicle-retrieve-previous-input') and `C-S-l', that
;;  is, `C-L', (`icicle-retrieve-next-input').  Use these commands to
;;  cycle among your past completion inputs (backward and forward,
;;  respectively).
;;
;;  User option `icicle-completion-history-max-length' limits the
;;  number of completion inputs to save.
;;
;;  If you customize user option `icicle-C-l-uses-completion-flag' to
;;  non-`nil', then, instead of cycling, `C-l' lets you use Icicles
;;  completion to retrieve a past completion input (`C-L' does the
;;  same thing).  Using completion to retrieve a past input does not
;;  also choose that input as the candidate for the main completion;
;;  it just replaces your current minibuffer input with it.  Because
;;  `C-l' completion uses a recursive minibuffer, you can also use
;;  `C-g' to cancel this completion and return to the main completion.
;;
;;  You can temporarily reverse the effect of
;;  `icicle-C-l-uses-completion-flag' by using a prefix argument
;;  (`C-u') with `C-l'.  Thus, `C-u C-l' uses completion if
;;  `icicle-C-l-uses-completion-flag' is `nil' and cycles if it is
;;  non-`nil'.
;;
;;  The other sections here describe Icicles enhancements for
;;  minibuffer histories.  They are thus concerned only with inputs
;;  that you enter, not with completion inputs that are not entered.
;;
;;(@* "Overview of Minibuffer History Enhancements")
;;  ** Overview of Minibuffer History Enhancements **
;;
;;  Icicles enhances the minibuffer history in these independent ways:
;;
;;  1. Commands invoked using a menu-bar menu are included in the
;;     command history for `M-x'.  This helps you quickly find again
;;     and reuse a (possibly deep) menu item.  It lets you use
;;     completion to access such commands.  And it helps you learn the
;;     commands that correspond to menu items that you use, thus
;;     providing a missing bridge between menu use and minibuffer use.
;;
;;     If you do not want to include menu-item commands in the command
;;     history, then set option `icicle-menu-items-to-history-flag' to
;;     `nil'.
;;
;;     Note: Non-`nil' `icicle-menu-items-to-history-flag' simply
;;     makes Emacs handle menu items that you choose the same way that
;;     it handles commands that you enter using `RET'.  It does not
;;     add such menu items to your completion history, which you
;;     access using `C-l' (see (@> "What Input, What History?"),
;;     above).
;;
;;  2. Minibuffer multi-command `icicle-insert-history-element' (`M-o'
;;     in the minibuffer) lets you use completion to insert any number
;;     of history elements in the minibuffer.
;;
;;  3. Candidates displayed in `*Completions*' are highlighted using
;;     face `icicle-historical-candidate' (blue foreground, by
;;     default), when they have been used previously, so you can more
;;     easily recognize them.  This highlighting is controlled by
;;     option `icicle-highlight-historical-candidates-flag'.  You can
;;     toggle this from the minibuffer at any time using `C-pause'.
;;
;;  4. Command `icicle-toggle-alternative-sorting', (`C-M-,' in the
;;     minibuffer) re-sorts completion candidates, placing previously
;;     used candidates first.  This is a toggle: repeat it to return
;;     to the original order.
;;
;;  5. Command `icicle-keep-only-past-inputs' (`M-pause' in the
;;     minibuffer) restricts the current set of completion candidates
;;     to those that you have used previously.  In other words, it
;;     keeps only those candidates that are highlighted in blue.  To
;;     use `M-pause', you must first have used `TAB' or `S-TAB' to
;;     establish an explicit candidate set.  If you use `C-u M-pause',
;;     then the previously used candidates are ordered
;;     chronologically, most recent first.  Without `C-u', the normal
;;     sort order is used (`icicle-sort-comparer').
;;
;;  6. Command `icicle-history' (`M-h' in the minibuffer) matches the
;;     current input against the minibuffer history directly.  It can
;;     be used during completion.
;;
;;  7. Command `icicle-other-history' (`C-M-pause' in the minibuffer)
;;     lets you use a different history for the current minibuffer
;;     reading.  You can choose the history using completion (even if
;;     the main minibuffer reading does not allow completion).
;;
;;     You can choose among any variables whose names end in `history'
;;     or `ring', which includes the Isearch histories `search-ring'
;;     and `regexp-search-ring'.
;;
;;  8. Commands `icicle-clear-history' and
;;     `icicle-clear-current-history' (`M-K' in the minibuffer)
;;     provide a general way to clean up histories.
;;
;;  9. When you cycle among previously entered inputs using `M-p' and
;;     `M-n', you can use `M-k' (command
;;     `icicle-erase-minibuffer-or-history-element') to delete the
;;     current occurrence from the history list.  This is a quick and
;;     handy way to clean up list entries that you are no longer
;;     interested in.  Only the occurrence that you have cycled to is
;;     deleted; if there are identical entries elsewhere in the
;;     history, they remain.
;;
;;  Some of these enhancements are described below in more detail.
;;  Each of 1-7 lets you see the complete list of previous inputs that
;;  match your current input.
;;
;;  In vanilla Emacs, the history lists are never shown as such; you
;;  can access previous inputs only one at a time, in order (with
;;  `M-p').  In vanilla Emacs, you can use a regexp to search the
;;  history list (via `M-r' and `M-s'), but the regexp matching is not
;;  dynamic, and the first match found is the only one you get.  In
;;  Icicle mode, `M-r' and `M-s' are not available for matching a
;;  history item - use `M-o' instead (see next).
;;
;;  Displaying previous inputs that match the current input sounds
;;  like a minor advantage, but it is actually quite helpful in
;;  practice.  Among other things, it means that you can work with
;;  long history lists in a practical way.
;;
;;(@* "Using Completion to Insert Previous Inputs: `M-o'")
;;  ** Using Completion to Insert Previous Inputs: `M-o' **
;;
;;  Unlike the other minibuffer history enhancements, described below,
;;  which are available only during minibuffer completion, you can use
;;  `M-o' (`icicle-insert-history-element') anytime you are asked for
;;  minibuffer input.  It is thus a form of on-demand completion (see
;;  (@> "Completion On Demand").  It provides a recursive minibuffer
;;  in which you can match previous inputs using completion.
;;
;;  This is a multi-command: you use `C-RET' etc. to choose matching
;;  previous inputs as candidates using a recursive minibuffer.  Yes,
;;  you can even use `C-!' to choose all matching candidates,
;;  inserting them in the current sort order.
;;
;;  When you are finished choosing candidates, use `C-g' (or `RET') to
;;  exit to the previous minibuffer level.  All of the previous inputs
;;  you chose are inserted into the upper-level minibuffer, in order.
;;
;;  This is better than cycling with `M-n' or `M-p', or searching with
;;  `M-s' or `M-r' in vanilla Emacs: You can use Icicles completion
;;  and cycling to quickly access a previous input, no matter how long
;;  ago you entered it.
;;
;;  When completion is available for reading input, if you use `M-o'
;;  to choose previously entered inputs, this just inserts that text
;;  into the minibuffer.  What is in the minibuffer after you use
;;  `M-o' is not used automatically and immediately for the main
;;  completion - you can edit it before entering it using `RET'.
;;
;;  Here's an example: Reuse a previous Isearch search string as the
;;  content-matching part of your input for `C-x C-f' (`icicle-file')
;;  or `C-x b' (`icicle-buffer'):
;;
;;    C-x C-f  C-M-j  C-M-pause
;;
;;    Choose `regexp-search-ring' (or `search-ring') as the history to
;;    use.
;;
;;    M-o S-TAB
;;
;;    Choose a previous search string to insert, then complete to find
;;    a file whose content matches it.
;;
;;  Reminder:
;;
;;    `C-M-j'     lets you provide a regexp to match file content.
;;    `C-M-pause' lets you choose a history.
;;    `M-o'       lets you insert from the history.
;;
;;(@* "Multi-Input Insertion with a Prefix Arg")
;;  *** Multi-Input Insertion with a Prefix Arg ***
;;
;;  This section describes the use of a prefix argument with `M-o'
;;  (`icicle-insert-history-element').  It also applies to `M-r'
;;  (`icicle-roundup').
;;
;;  Because `M-o' lets you insert more than one previous input, there
;;  are different ways to separate these insertions, depending on the
;;  prefix argument.
;;
;;  * With no prefix argument, each inserted candidate you choose is
;;    followed by a space character.
;;
;;  * With a non-positive prefix argument, no such space char is
;;    appended.
;;
;;  * With a non-negative prefix argument, each chosen candidate is
;;    wrapped with "..." before being inserted.
;;
;;  These possibilities provide for different uses of such insertion:
;;
;;  * Space separation is useful if you are preparing minibuffer input
;;    for a command that interprets the input as multiple arguments.
;;    It is also useful for Icicles keys (`M-R', `M-S') that interpret
;;    the input as multi-inputs - see (@> "Multi-Inputs").
;;
;;  * Wrapping with "..." is useful in combination with space
;;    separation when a candidate contains whitespace.  For example,
;;    multiple file name, some of which contain whitespace.
;;
;;  A prefix argument to `M-o' applies to each of the candidates you
;;  choose, by default.  If you use a prefix argument then you will
;;  typically want to exit using `C-g' (not `RET'), so the prefix
;;  argument applies also to the last candidate chosen.
;;
;;  You can override the prefix argument for `M-o' or lack thereof, by
;;  using a prefix argument for an individual candidate action
;;  (e.g. `C-u C-RET').
;;
;;(@* "Putting Previous Candidates First: `C-M-,'")
;;  ** Putting Previous Candidates First: `C-M-,' **
;;
;;  At any time, two of the Icicles sort orders are immediately
;;  available.  These are the values of user options
;;  `icicle-sort-comparer' and `icicle-alternative-sort-comparer'.  By
;;  default, the former usually sorts alphabetically, and the latter
;;  puts all previously used inputs first, before the candidates you
;;  have not yet used.  Each of these groups, used and unused
;;  candidates, is then sorted alphabetically, separately.  So, with
;;  the default alternative sort, you can see all matching candidates
;;  (used and unused), but you privilege those used previously - they
;;  are the first listed in `*Completions*' and the first available
;;  for cycling.
;;
;;  If you prefer, by customizing these user options, you can use
;;  `icicle-historical-alphabetic-p' as the main sort function (option
;;  `icicle-sort-comparer') and some other sort function
;;  (e.g. `icicle-case-string-less-p') as the alternative sort
;;  function.
;;
;;  You can toggle at any time between normal sorting and alternative
;;  sorting, using command `icicle-toggle-alternative-sorting'.
;;  During completion, this is bound to `C-M-,'.  Together with
;;  toggling between normal sorting and not sorting at all, which is a
;;  sort-order choice available through `C-,', this gives you quite a
;;  lot of flexibility.
;;
;;(@* "Matching Only Historical Candidates: `M-h' and `M-pause'")
;;  ** Matching Only Historical Candidates: `M-h' and `M-pause' **
;;
;;  Both `M-h' (`icicle-history') and `M-pause'
;;  (`icicle-keep-only-past-inputs') can be used toward the same end.
;;  They both work for all input types.  They both use the appropriate
;;  history list for the current command.  They both provide
;;  completion and cycling for the minibuffer history.  Use them as
;;  another way to search through a history list or complete to one of
;;  its elements.
;;
;;  For example, If you use `C-x C-f' to find a file, and then use
;;  `M-h' or `M-pause', the completion candidates will be the names of
;;  files that you have previously accessed (file names you have input
;;  in the minibuffer), and which match the current minibuffer input.
;;
;;  `M-h' lets you complete your input against the minibuffer input
;;  history.  `M-pause' lets you restrict the current explicit set of
;;  completion candidates to those that are also in the minibuffer
;;  history.
;;
;;  They provide similar functionality in different ways.  The
;;  difference is that `M-pause' takes the current set of matching
;;  candidates into account.  It is a completion-candidates set
;;  operation, similar to those described in section
;;  (@> "Sets of Completion Candidates").
;;
;;  This means, in particular, that with `M-pause' you can first
;;  perform set operations on the set of candidates, and then use that
;;  result to restrict the history search.  For example, you can first
;;  complement the candidate set using `C-~', then use `M-pause' to
;;  restrict those candidates to matches in the history list.  In this
;;  way, you avoid including matches from the original match set when
;;  searching the history.
;;
;;  Example: You are in a directory with lots of files that have the
;;  prefix `foo' and lots of C-language source files.  You happen to
;;  be interested in another file, however.  One way to get to that
;;  file is to use Dired's ability to mark files by matching a regexp
;;  and then use Dired's ability to omit the marked files from view.
;;  You can scan through those that remain, and pick the one you want.
;;  However, it turns out that even then there are many files to scan.
;;  You accessed the one you want now just the other day, but the file
;;  date is unfortunately not significant.
;;
;;  In Icicles, you use regexp matching and take the set complement of
;;  the hits, just like in Dired: `C-x C-f foo.*\.c$' defines the
;;  candidate set as all files whose names start with `foo' and have
;;  extension `c'.  `C-~' then defines the candidate set as all files
;;  whose names are not like that.  Finally, you use `M-pause' to
;;  restrict the file-name candidates to names that you have used
;;  before.  You've accessed many, many files recently, so just
;;  cycling through the history with `M-p' would be tedious.  You
;;  could match a regexp against the file history, but how can you
;;  come up with a regexp that finds anti-matches?
;;
;;  A consequence of this difference between `M-h' and `M-pause' is
;;  that using `TAB' or `S-TAB' after `M-pause' abandons use of the
;;  minibuffer history and starts a new set of completion candidates.
;;  It simply completes the current input in the context of the
;;  current command; `TAB' and `S-TAB' have nothing to do with the
;;  minibuffer history in this case.  Using `TAB' or `S-TAB' after
;;  `M-h', however, re-completes your input against the current
;;  history list.
;;
;;  Another consequence is that you can use `down' or `C-down' on the
;;  candidates displayed by `M-h', but not on those displayed by
;;  `M-pause'.  For example, to cycle through the doc for each
;;  variable that starts with `icicle-' which you have previously
;;  input, you can use `C-h v icicle- M-h', then repeatedly use
;;  `C-down'.
;;
;;  Also, file-name and directory-name completion works differently in
;;  these two commands.  By default, the current directory is (as
;;  always) inserted into the minibuffer by commands such as
;;  `find-file', so either `M-h' or `M-pause' after `C-x C-f' will
;;  match previously input file names from the current directory.
;;
;;  However, in the case of `M-h', the entire minibuffer input is
;;  matched against the history list, which is a list of absolute file
;;  names.  `M-pause' works only with the current candidate set,
;;  which, if you have already used `TAB' or `S-TAB' in the current
;;  directory, is a set of relative file names in that directory.
;;
;;  This difference has a consequence for apropos (regexp) completion
;;  with `M-h'.  It means that to match a file name using a substring
;;  you must, in the minibuffer, either not specify a directory (erase
;;  it) or explicitly use `.*' before the file-name substring.
;;
;;  For example, with `M-h', `/foo/bar/lph' will not apropos-match the
;;  previously input file name `/foo/bar/alphabet-soup.el'; you should
;;  use either `/foo/bar/.*lph' or `lph' (no directory).
;;
;;  In the case of `M-pause', however, the input is matched against
;;  the history list as restricted by the existing completion list.
;;  And, since apropos file-name completion uses only the relative
;;  file name, without the directory name, as a regexp, the candidate
;;  list that is restricted has already matched the input regexp.  The
;;  action of `M-pause' is simply to filter the list of candidates,
;;  keeping those that are in the history list.  This means that, with
;;  `M-pause', the input `/foo/bar/lph' will match against the
;;  previously input file name `/foo/bar/alphabet-soup.el'.
;;
;;  Note that neither `M-h' nor `M-pause' uses a recursive minibuffer;
;;  they each simply co-opt the current completion, changing it to
;;  completion against the history.  This means that whatever
;;  completion mode (prefix or apropos) was in effect before you use
;;  `M-h' or `M-pause' remains in effect for the history completion as
;;  well.
;;
;;  If this all sounds confusing, just give it a try; it is much
;;  harder to describe than it is to experience.
;;
;;(@* "Using Other Histories; Commands Any Which Way")
;;  ** Using Other Histories; Commands Any Which Way **
;;
;;  This section describes how to complete your input against a
;;  history other than the default history provided for the current
;;  command.  A special case of this, starting with Emacs 23, is
;;  completing a command, abbrev, or keyboard macro name against all
;;  such that were previously executed in any interactive way.  This
;;  includes commands invoked using a menu.
;;
;;(@* "Completing Against All Interactive Commands")
;;  *** Completing Against All Interactive Commands ***
;;
;;  When you execute a command using `M-x', it is added to the history
;;  `extended-command-history'.  Likewise, when you execute a command
;;  or abbrev using `icicle-command-abbrev-command'.  And when you
;;  execute a keyboard macro using `C-x M-e'
;;  (`icicle-execute-named-keyboard-macro'), it is added to history
;;  `icicle-kmacro-history'.
;;
;;  However, when you execute a command, abbrev, or keyboard macro in
;;  other ways than these, it is not added to such a history.  For
;;  example, if you choose a menu item, the associated command is not
;;  added to any of these histories.  Thus, although `M-o' lets you
;;  complete against previously used commands, this does not include
;;  commands that were called via a menu item.
;;
;;  To remedy this, starting with Emacs 23 Icicles can optionally add
;;  all commands that are called using `call-interactively' to the
;;  larger command history `icicle-interactive-history'.  This
;;  includes commands on menus.  To enable this feature, you must
;;  customize option `icicle-populate-interactive-history-flag', to
;;  make it non-`nil'.  Thereafter, when you enter Icicle mode, all
;;  interactive use of commands records them on this special history.
;;
;;  Be aware that use of this feature can slow Emacs down, and the
;;  history list can become quite large.
;;
;;  During completion, you can then use `C-M-pause'
;;  (`icicle-other-history') to complete against this extended set of
;;  previously used commands.  For example, if you use menu item `Open
;;  File', then the corresponding command, `menu-find-file-existing',
;;  becomes available as a completion candidate.  (Recall too that the
;;  command associated with a given menu item is shown in the
;;  `*Completions*' mode line whenever you cycle to it.)  This is a
;;  special case of using `C-M-pause', of which see more below.
;;
;;(@* "Using an Alternative History")
;;  *** Using an Alternative History ***
;;
;;  Anytime you enter input in the minibuffer, regardless of whether
;;  completion is available, `C-M-pause' prompts you for an
;;  alternative history to use - any history you like.  You can choose
;;  the history using completion.
;;
;;  That is, you can change which history to use, on the fly, as many
;;  times as you like.  You can use this before using `M-o', for
;;  instance, to complete against different histories for different
;;  parts of your overall minibuffer input.  If you are inputting a
;;  complex shell command with arguments, for example, you can use
;;  different histories to compose different parts of the overall
;;  command.
;;
;;  (If you are completing a command, abbrev, or keyboard macro, then
;;  for `C-M-pause' to prompt for an alternative history you must use
;;  a prefix argument.  Otherwise, you get the special `C-M-pause'
;;  behavior described above - see
;;  (@> "Completing Against All Interactive Commands").
;;
;;(@* "Cleaning Up History Lists")
;;  ** Cleaning Up History Lists **
;;
;;  Besides the use of `M-k' during history cycling (`M-p', `M-n') to
;;  remove individual input occurrences from the current history list,
;;  you can use commands `icicle-clear-history' and
;;  `icicle-clear-current-history' to clean minibuffer histories
;;  entirely of selected entries.  Command
;;  `icicle-clear-current-history' is bound to `M-K' in the
;;  minibuffer.  It is `icicle-clear-history' specialized to work on
;;  just the current history list.
;;
;;  These commands prompt you for a history entry to delete from a
;;  history list.  These are multi-commands, so you can delete
;;  multiple entries.  For each entry you choose, all of its
;;  occurrences are deleted from the history.
;;
;;  Command `icicle-clear-history' first prompts you for a history
;;  list to act on.  This too is multi-command input, so you can use
;;  `icicle-clear-history' to remove entries from multiple histories.
;;
;;  If you use a prefix argument with these commands, then the
;;  histories are emptied entirely (upon confirmation).  Thus, for
;;  instance, you can use `C-u M-i' at any time during minibuffer
;;  input to completely empty the current history list.
;;
;;  See Also: (@> "More about Multi-Commands") for information about
;;  using `S-delete' to delete objects associated with completion
;;  candidates.
 
;;(@* "Isearch Enhancements")
;;
;;  Isearch Enhancements
;;  --------------------
;;
;;  Icicles provides two different enhancements for searching:
;;
;;  - Icicles search: Top-level Icicles commands that provide an
;;    entirely new and different way for you to search.
;;    This is described in section
;;    (@file :file-name "icicles-doc2.el" :to "Icicles Search Commands, Overview").
;;
;;  - Extensions to standard Emacs incremental search, Isearch.
;;    These are described in this section.
;;
;;    * Search string completion against previous search strings.
;;    * Reuse of the content-matching pattern for `icicle-buffer' and
;;      `icicle-file' as the Isearch regexp-search pattern.
;;    * Occur mode interface for Isearch hits.
;;    * Icicles search (`icicle-search') interface, reusing the
;;      Isearch search string (by default).
;;
;;  See Also: (@file :file-name "icicles-doc2.el" :to "Support for Projects")
;;  for information about using `grep' to search all of the files in a
;;  project.
;;
;;(@* "Isearch Completion Against the Search History")
;;  ** Isearch Completion Against the Search History **
;;
;;  When you search incrementally (`C-s'), Emacs lets you use `M-TAB'
;;  (`M-tab' or `C-M-i', aka `ESC-TAB') to complete your input to a
;;  string that you have sought previously, that is, a string in the
;;  current search history (`search-ring' or `regexp-search-ring').
;;  In Icicle mode, this feature is enhanced so that you can use all
;;  of the completion enhancements provided by Icicles: `M-TAB' is
;;  bound to `icicle-isearch-complete' during Isearch.
;;
;;  Some operating systems grab `M-TAB' for their own use, making it
;;  unavailable for Emacs.  They normally do not grab `ESC TAB', which
;;  in Emacs is typically the same ase `M-TAB'.  For this reason,
;;  Icicles also binds `icicle-isearch-complete' to both `ESC TAB' and
;;  `C-M-TAB'.  (Note: For some versions of MS Windows, you can use
;;  (w32-register-hot-key (icicle-kbd "M-tab")) to allow Emacs to use
;;  `M-TAB'.)
;;
;;  Icicles users are in the habit of using `M-o' to complete the
;;  current minibuffer input against previously entered inputs.
;;  Because of the similarity, you can likewise use `M-o' during
;;  Isearch to append previous isearch strings to the current string.
;;  During Isearch, `M-o' is bound to minibuffer multi-command
;;  `icicle-isearch-history-insert'.
;;
;;  The keys bound by default to `icicle-isearch-complete' in
;;  `isearch-mode-map' are `M-TAB', `ESC TAB', and `C-M-TAB', and the
;;  only key bound by default to `icicle-isearch-history-insert' is
;;  `M-o'.  But you can change the keys to use for these commands by
;;  customizing options `icicle-isearch-complete-keys' and
;;  `icicle-isearch-history-insert-keys', respectively.
;;
;;  When you use `M-o' (or `M-TAB') while searching, Isearch exits
;;  momentarily, giving way to Icicles completion in the minibuffer.
;;  You can then use either `S-TAB' or `TAB' to complete your search
;;  string.  After you finish completing (e.g. by hitting `C-g' or
;;  `RET'), Isearch resumes with the new, modified search string.
;;  It's pretty seamless, and easier to try than to describe.
;;
;;  Reminder: Using `S-TAB' vs `TAB' for regexp vs non-regexp
;;  completion against previous search strings has nothing to do with
;;  regexp vs non-regexp incremental searching.  You can of course use
;;  either kind of searching before or after having used either kind
;;  of completion.  Isearch uses different search histories ("rings")
;;  for regexp and non-regexp searching.  The kind of search in
;;  progress (regexp or not) at the moment you use `M-TAB' or `M-o'
;;  determines which search ring provides the candidates for
;;  completion.
;;
;;(@* "Content-Matching Pattern as Isearch Regexp")
;;  ** Content-Matching Pattern as Isearch Regexp **
;;
;;  When your multi-completion input for commands such as
;;  `icicle-buffer' (`C-x b'), `icicle-file' (`C-x C-f'), and
;;  `icicle-Info-goto-node' (`g' in Info) provides a content-matching
;;  pattern for completion against buffer, file, or Info node content,
;;  and at least one candidate matches, the pattern is saved to the
;;  Isearch regexp history, `regexp-search-ring'.  If you then visit a
;;  matching buffer or file, you can immediately search for each match
;;  using `C-M-s' or `C-M-r'.
;;
;;  This gives you another way to search a set of buffers, files, or
;;  Info nodes.  You can use progressive completion to narrow down the
;;  search hits, either by container (buffer, file, or node name) or
;;  by content.
;;
;;  (This content-searching behavior is available for `icicle-file'
;;  starting with Emacs 23, and for `icicle-Info-goto-node' starting
;;  with Emacs 22.)
;;
;;  See also (@> "Chapter & Verse: Searching Named Containers").
;;
;;(@* "Launch Occur using the Isearch Search String")
;;  ** Launch Occur using the Isearch Search String **
;;
;;  If you use library `color-moccur.el' or library
;;  `occur-schroeder.el', then `C-o' is bound during Isearch to
;;  `isearch-moccur', which provides an Occur buffer interface for
;;  search hits.  This has nothing per se to do with Icicles, but you
;;  might find it useful.  (Library `color-moccur.el' itself binds
;;  `M-o' for this, but `M-o' is used in Icicles for search-string
;;  completion.)
;;
;;(@* "Launch Icicles Search using the Isearch Search String")
;;  ** Launch Icicles Search using the Isearch Search String **
;;
;;  Icicles search is described in section
;;  (@file :file-name "icicles-doc2.el" :to "Icicles Search Commands, Overview").
;;
;;  You can start Icicles search from Isearch: Hit `S-TAB' to choose
;;  the Icicles search initial regexp - the default value is the
;;  current Isearch search string, but you can edit that.  Completion
;;  is available for your input - completion against your previous
;;  Isearch regexp search strings.
;;
;;  For example, use `C-s C-w C-w S-TAB' to pick up the next two words
;;  at the cursor, then type `.*' before and after them and hit `RET'.
;;  That puts you in Icicles search with the completion candidates
;;  being all of the lines in the buffer that contain that two-word
;;  phrase.  Type some more text to narrow the candidate lines to
;;  those that match what you type.  Then use `C-next' to visit search
;;  hits.
;;
;;  With Emacs 22 and later, and provided option
;;  `isearch-allow-scroll' (a misnomer) is non-`nil', you can use a
;;  prefix argument with `S-TAB' to change the behavior.  You still
;;  choose an Isearch search string using completion.  But in this
;;  case the string does not define the Icicles search contexts.
;;  Instead, you are prompted for a search-context regexp to do that.
;;  The Isearch string is copied to the `kill-ring', so you can yank
;;  it into your minibuffer input anytime, to search for it within
;;  each of the search contexts.
;;
;;  The key to initiate Icicles search from Isearch is `S-TAB' only by
;;  default.  You can change this key by customizing option
;;  `icicle-search-from-isearch-keys'.
 
;;  The Icicles doc is continued in file `icicles-doc2.el'.
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
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

(provide 'icicles-doc1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; icicles-doc1.el ends here
