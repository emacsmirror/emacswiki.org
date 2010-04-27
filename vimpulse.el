;;; vimpulse.el --- emulates Vim's most useful features -*- coding: utf-8 -*-

;; Copyright (C) 2007 Brad Beveridge
;; Copyright (C) 2007, 2009 Alessandro Piras
;; Copyright (C) 2008 Frank Fischer
;; Copyright (C) 2009 Jason Spiro <http://www.jspiro.com/>
;; Copyright (C) 2010 Vegard Øye
;;
;; Author: Brad Beveridge et al.
;; Maintainer: Vegard Øye <vegard_oye at hotmail.com>
;; Created: 23 Aug 2007
;; Version: 0.4
;; Keywords: emulations, viper
;; Human-Keywords: vim, visual-mode, rsi, ergonomics, emacs pinky
;; URL: http://www.emacswiki.org/emacs/vimpulse.el
;; Git and bug tracker: http://www.assembla.com/spaces/vimpulse/
;;      Send patches to http://trac-git.assembla.com/vimpulse/report/1
;; Mailing list: <implementations-list at lists.ourproject.org>
;;      http://tinyurl.com/implementations-list
;;      You don't have to subscribe.  We usually reply within a few
;;      days and CC our replies back to you.
;; Related: viper.el, viper-in-more-modes.el
;;
;; Thanks to our old maintainers:
;;      Alessandro Piras
;;      Jason Spiro
;; We'll miss you as maintainers :)
;;
;; This file is not part of GNU Emacs.

;;; Commentary:

;; Vimpulse emulates Vim's most useful features, including Visual mode
;; and text objects.  Vimpulse is a set of modifications to Viper, the
;; minor mode that emulates vi.  Vimpulse is not a minor mode; as soon
;; as it is loaded, Viper will start working in a more Vim-like way.
;;
;; Vimpulse is beta software.  It seems to work quite well with
;; GNU Emacs 22.3 and 23.1.  There are some problems with Undo in
;; XEmacs 21.4.22.  Please send us compatibility info about other
;; versions.
;;
;; Patches and feature requests are welcome.

;;; Installation:

;; If you checked out from Git, do `make all' to produce
;; vimpulse-big.el.  If on Windows, you can run compile.bat.  Then:
;;
;; 1. Copy vimpulse.el (or vimpulse-big.el) to somewhere in your
;;    load-path, e.g., your site-lisp directory.
;;
;; 2. Comment out and add the following line to your .emacs file:
;;
;;        (require 'vimpulse)
;;
;;    If .emacs does not exist, create it.  If you use Windows,
;;    see http://www.gnu.org/software/emacs/windows/faq3.html.
;;
;; That's all the required steps.  The rest is optional:
;;
;; 3. If using GNU Emacs, download and install
;;    http://wonderworks.com/download/redo.el to get C-r
;;    (redo key) to work.  redo.el is included with XEmacs.
;;
;; Vimpulse automatically enables Viper.  You can temporarily disable
;; Viper (and Vimpulse) with the C-z key.

;;; Usage:

;; To use Visual mode, press v in vi (command) mode.  Then use the
;; motion commands to expand the selection.  Press d to delete, c to
;; change, r to replace, or y to copy.  You can use p to paste.  For
;; Line selection, press V instead of v; then you can copy and paste
;; whole lines.  For Block selection, press C-v; now you can copy and
;; paste the selected rectangle.  In Block selection, you may use I
;; or A to insert or append text before or after the selection on each
;; line.
;;
;; Other features:
;;
;; Vimpulse supports text objects: daw, daW, das, dap, dab, daB, da(,
;; da[, da{, da<, da", da', as well as diw, diW, dis, etc.  To change
;; an object: caw, cas, etc.  To yank it: yaw, yas, etc.  To select it:
;; vaw, vas, etc.
;;
;; The extended documentation is still in its early stages, but you
;; can view drafts at
;;
;; http://trac-git.assembla.com/vimpulse/wiki/Documentation
;;
;; The documentation that comes with Vim -- which is online at
;; http://vimdoc.sf.net/ -- may also be helpful.
;;
;; Tips:
;;
;; - Vimpulse makes C-r run `redo' in command mode, but you can
;;   still get reverse isearch by pressing C-s and then C-r.
;;
;; - To change the color of Visual mode, add the following to .emacs:
;;   (set-face-background 'region "blue") ; `zmacs-region' in XEmacs

;;; Change Log:

;; Version 0.4
;;  [vegard_oye at hotmail.com:]
;;  - [NEW] Operator-Pending mode: the cursor's appearance
;;    changes temporarily after y, d, c, etc.
;;  - [NEW] Motion type system: one can change how a motion is
;;    "interpreted" with v, V and C-v. For example, dvj will
;;    delete a characterwise range (the default is linewise).
;;  - [NEW] Keys: gq, gu, gU, g~, g?.
;;  - [NEW] Keybinding functions: `vimpulse-omap' and
;;    `vimpulse-omap-local'.
;;  - [FIX] Vimpulse's text objects handle whitespace
;;    more like Vim's.
;;  - [FIX] Various bugs submitted to the mailing list --
;;    thanks, everyone.
;;  - The code for applying an "operator" like d to a "motion"
;;    like w is completely rewritten. Operators are simple
;;    to define (with `vimpulse-range'), and can be applied
;;    to regular Emacs movement commands as well.
;;  - The text objects have been redefined in terms of the new
;;    framework. They are implemented as selection commands;
;;    see the `vimpulse-define-text-object' macro for details.
;;  - The code for adding Viper states is generalized.
;;    Both Visual mode and Operator-Pending mode are
;;    defined with the `vimpulse-define-state' macro.
;;  - The comments use a more conventional format: ;;;; for major
;;    headings (one per file), ;;; for subsections (within each file),
;;    ;; for individual pieces of code and ; for trailing comments.
;;    This is easier to maintain and complies with section D.7 of
;;    the GNU Emacs Lisp Reference Manual.
;;
;; Version 0.3.1
;;  [vegard_oye at hotmail.com:]
;;  - [NEW] Emacs-compatible Visual selection.
;;    It is now a Viper state proper, with a user map
;;    and a major mode extension map.
;;    [NEW] Visual keys: u, U, ~, >, <, J, O, gv --
;;    thanks, Frank Fischer.
;;  - [NEW] Movement keys: C-o, C-i, C-w hjkl, gb, gd, N%, +, _.
;;  - [NEW] Keybinding functions: `vimpulse-map',
;;    `vimpulse-imap' and `vimpulse-vmap'.
;;  - [NEW] Backspace in Replace mode restores text.
;;  - [NEW] Basic vi navigation in help buffers.
;;  - [NEW] Vimpulse has its own customization group.
;;  - [FIX] Improved text objects support, including Visual mode.
;;  - [FIX] Various bugs listed at EmacsWiki or submitted to the
;;    mailing list or bug tracker -- thanks.
;;  - All Vimpulse bindings are now in `viper-vi-basic-map',
;;    leaving `viper-vi-global-user-map' for the user.
;;    The same is true of Visual mode.
;;  - Easier installation. rect-mark.el is no longer needed,
;;    nor is cl.el.
;;  - All tabs are replaced by spaces.
;;  - The file encoding is UTF-8.
;;  [laynor at gmail.com:]
;;  - Added some small fixes, and promoted the experimental stuff to
;;    stable, as it seems to work well and not loading it caused
;;    problems.
;;
;; Version 0.3.0
;;  [laynor at gmail.com:]
;;  - [NEW] Register support on text object commands.
;;  - [NEW] Issuing : in Visual mode has a behavior closer
;;    to Vim's.
;;  [jasonspiro3 at gmail.com:]
;;  - [FIX] The Enter key now does what it should do -- insert a
;;    newline -- even when longlines-mode is on.
;;  - Comment changes.
;;
;; Version 0.2.6.9
;;  [laynor at gmail.com:]
;; - [FIX & NEW] Text objects support fixed and integrated with Viper.
;;   Now count works (e.g., you can do 3caw and it works correctly),
;;   and it's possible to repeat the commands with ".".
;;
;; Version 0.2.6.8
;;  [laynor at gmail.com:]
;; - [NEW] Text object support: paren blocks, sentences, word, Words,
;;   quoted expressions, paragraphs. Delete and change commands.
;;   Example commands: diw, ci(, das, etc.
;; - [FIX] It's now possible to exit Visual mode by pressing the
;;   ESC key or ^[.
;;
;; Version 0.2.6.7
;;  [jasonspiro3 at gmail.com:]
;;  - No code changes.
;;  - Fixed up "thanks" section below to mention Mieszko
;;    <sillyfox at yahoo.com>'s full name. He wrote a small patch
;;    which was included long ago. I must have forgotten to include it
;;    in the changelog.
;;
;; Version 0.2.6.6
;;  [laynor at gmail.com:]
;; - Fixed pasting in Visual mode, works like in Vim now
;;   (experimental, see point 6 of installation instructions).
;;
;; Version 0.2.6.5
;;  [laynor at gmail.com:]
;; - Fixed some major suckage with the change command. Still alpha,
;;   comments welcome. To use it, see the installation instructions,
;;   point 6 (it's still experimental).
;; - Cleaned namespace, hope there are no hidden bugs.
;; - Fixed loading on Emacs snapshot.
;;
;; Version 0.2.6.4
;;  [laynor at gmail.com:]
;;  - This can probably be considered a major release.
;;  - [FIX & NEW] Rewritten Visual mode, v and V variants (no
;;    changes to Visual Block still). It does not use the region like
;;    before: highlighting is done through overlays, and the region is
;;    set inside the command code before calling the Viper commands.
;;    = in Visual mode calls `vimpulse-visual-indent-command'. The
;;    Visual mode (apart from Block mode) looks and feels like Vim.
;;  - [NEW] Enhanced paren-matching. Moving the cursor on a closing
;;    paren in Normal mode now highlights the opening paren.
;;  - [NEW] Pressing RET in Insert mode automatically indents the new
;;    line.
;;  - [NEW] ^[ works.
;;  - [FIX] a<ESC> leaves the cursor in the same location as it was
;;    before (it advanced the cursor 1 character before --
;;    `viper-exit-insert-state's fault).
;;  - [FIX] cW doesn't suck anymore at the end of a line.
;;
;; Version 0.2.6.3:
;;  [frank.fischer at s2001.tu-chemnitz.de:]
;;  - Support more Visual Block mode features: insert, append, delete,
;;    yank, change.
;;  - Change some Vimpulse and Viper functions to handle Block mode
;;    properly.
;;  - Update documentation to reflect Visual Block mode.
;;  - The = key in Visual mode calls `indent-region'.
;;
;; Version 0.2.6.2:
;;  [jasonspiro3 at gmail.com:]
;;  - Improved XEmacs compatibility.
;;  - Small documentation improvements.
;;
;; Version 0.2.6.1:
;;  [jasonspiro3 at gmail.com:]
;;  - Removed duplicate definition of `vimpulse-detect-mark-deactivate'
;;    and duplicate `add-hook' call to add the hook. I must have added
;;    the extra copies by accident when doing my last big merge; now
;;    they are gone.
;;
;; Version 0.2.6.0:
;;  [jasonspiro3 at gmail.com:]
;;  - Merged a patch for the function that powers * and #. Based on
;;    Ryoichi's patch and a cleaned-up version of Weihua's patch --
;;    thanks. Now * and # will search for entire symbol at point,
;;    including underscores, not just word at point.
;;  - TODO addition.
;;
;; Version 0.2.5.1:
;;  [jasonspiro3 at gmail.com:]
;;  - Redefined viper-adjust-undo to do nothing. This way, in Insert
;;    mode, typing then moving the cursor then typing more counts as
;;    two separately undoable actions instead of one. Thanks to Weihua
;;    JIANG and to max_ from IRC #emacs for the idea.
;;  - Small extra TODO.
;;
;; Version 0.2.5.0:
;;  [jasonspiro3 at gmail.com:]
;;  I've ignored my local changes for too long. Here they are:
;;  - Added keybindings from a Usenet post by Samuel Padgett.
;;  - Made change (cw, etc.) commands work more like Vim (my code).
;;  - I removed (setq ex-cycle-other-window nil); although it is very
;;    useful, it merely works around a problem with Viper. I plan to
;;    discuss it with the Viper maintainer instead.
;;  - Other changes and bugfixes from various people.
;;
;; Version 0.2.0.3:
;;  [jasonspiro3 at gmail.com:]
;;  - Added Brad's `viper-jump-to-tag-at-point'.
;;
;; Version 0.2.0.2:
;;  [jasonspiro3 at gmail.com:]
;;  - Small C-w keys and doc fixes.
;;
;; Version 0.2.0.1:
;;  [cppjavaperl:]
;;  - Added support for Visual Block mode (i.e., rectangle selection).
;;  - Made C-p look for matches *prior* to the cursor and added C-n
;;    binding to look for matches *before* the cursor. This works more
;;    like Vim does.
;;  [jasonspiro3 at gmail.com:]
;;  - Since Vimpulse has no website, I added a prominent pointer at
;;    the top to the installation instructions.
;;
;; Version 0.2.0.0: Brad merged in several changes, including:
;;  - Exit Visual mode when the mark deactivates.
;;  - Changed the window manipulation to be global.
;;  - Added gf (goto file at point).
;;  - Added \C-] and \C-t, tag jump & pop.
;;  - Added a helper function for defining keys.
;;  - Commented out `show-paren-function', what is it meant to do?
;;
;; Version 0.1.0.1: No code changes. Small documentation changes,
;; including updates on moving-left bug.
;;
;; Version 0.1: Initial release.

;;; Acknowledgements:

;; Special thanks to Brad Beveridge, the original author of Vimpulse.
;;
;; Thanks to:
;;
;;      cppjavaperl <cppjavaperl at yahoo.com>
;;      Frank Fischer <frank.fischer at s2001.tu-chemnitz.de>
;;      John <jn at ngedit.com>
;;      John J Foerch <jjfoerch at earthlink.net>
;;      José Alfredo Romero L. <escherdragon at gmail.com>
;;      Mieszko <sillyfox at yahoo.com>
;;      rhinoryan
;;      Rick Sladkey, author of rect-mark.el
;;      Ryoichi Kanetaka <ryoichi.kanetaka at gmail.com>
;;      Samuel Padgett
;;      Štěpán Němec <stepnem at gmail.com>
;;      Stephen Bach <stephen at sjbach.com>
;;      Stian S.
;;      Toby Cubitt
;;      Wang Xin
;;      Weihua Jiang <weihua.jiang at gmail.com>
;;
;; and all the other people who have sent in bug reports and feedback.
;; Also, thanks to Michael Kifer and Viper's contributors.
;;
;; We love patches.  Would you like to see your name here?
;; Please send code and/or documentation patches to the maintainer.
;; Ideas, comments, and test results are appreciated too.

;;; Bugs:

;; (We would appreciate it very much if you report bugs.)
;;
;; Known bugs:
;;
;; - cw with a count doesn't work quite the same as Vim when the point
;;   is just before a space between words.
;;     - Fix plan: try cw with a count, then try dwi with a count; or
;;       ask on a relevant forum how the commands differ; or check
;;       how it works in vi/Vim, then check the Vim manual for more
;;       info; then, decide how to best fix it.
;;         - Vim's behavior seems inconsistent in this case.  I think
;;           it would be best to make the fix optional (defaulting to
;;           standard Vim behavior, though), as I (at least) like
;;           Vimpulse's behavior better in this case.
;;
;; - Undo has problems in XEmacs.

;;; Development and documentation TODOs:

;; - Make sure I have added all stuff in Brad's Viper additions and
;;   from my collection, then start documenting already.  Once there
;;   are even the simplest of docs (a nice keymap), people will have a
;;   far easier time using Vimpulse and so I bet more will contribute.
;;
;; - The / key should allow isearch that works like Vim's, or until
;;   that's implemented, it should at least remap / to
;;   `isearch-forward' or `viper-isearch-forward'.  This should be an
;;   option that should be disabled by default.  For now, have Viper
;;   load .vimrc and check for Vim-specific option strings like "set
;;   incsearch". If anyone complains, rethink that plan.
;;
;; - Folding.  This should be implemented as a separate Lisp library
;;   usable for even non-Viper users. Which foldmethods to do first?  I
;;   personally only use foldmethod=marker, and even that only rarely.
;;
;; - i_C-(I forgot the letter) should do (copy-from-above-command 1)
;;   from misc.el.
;;
;; - Add :set spell / :set nospell that uses flyspell-mode.
;;
;; - Add support for tabs.el, a tabs mode that works sensibly (get it
;;   from Emacs Lisp List).
;;     - Minimum needed: :tabedit, :tabnext, :tabprevious.
;;     - Since I'm emulating Vim, emulate its tab pages feature.  So a
;;       tab page should be able to hold one or more buffers.
;;
;; - Add Customize option to let users stop C-r from being Redo?
;;
;; - Replace redo.el with Toby Cubitt's awesome undo-tree.el.
;;   http://www.emacswiki.org/emacs/UndoTree
;;
;; - Copy more features from Brad's work in darcs and from vimpact
;;   into Vimpulse.
;;
;; - Doc: look in Google chat log, find description of one-char-off
;;   bug, see if it applies to this or to the not-yet-released
;;   viper-x, and if to this, mention under Bugs.
;;
;; - Doc: list all new keys (and maybe all differences from Viper) in
;;   Usage section.
;;
;; - Doc: describe all new keys in Usage section; can look at Vim
;;   manual for ideas.
;;
;; - Modify how tramp works so it also automatically handles URLs
;;   typed in the netrw syntax, e.g., http:// etc.  But first ask tramp
;;   upstream if they could please make those changes themselves.
;;
;; - Improve CTRL-O for jumping back in the jumplist and CTRL-I for
;;   jumping forwards (for undoing one CTRL-O).  The global mark ring
;;   is not what I want.  I wonder if Emacs' tags functionality allows
;;   a jumplist.  I wonder if Viper does tags like nvi does.
;;     - Try code.google.com/p/ejumplist/source/browse/trunk/jumplist.el
;;
;; - On my PC (I run Ubuntu), if you start plain Vim and then press
;;   CTRL-O many times, it starts opening recently opened files.  Is
;;   that useful?  Should Vimpulse have persistent jump table
;;   functionality like that, and if so, should it use recentf or
;;   Vim's .viminfo file or some tag functionality in Emacs?  How will
;;   it interact with the fact that in Emacs, it's not traditional to
;;   suddenly close files without warning?
;;
;; - Make sentence movement work like in Vim.  I wonder if this can be
;;   done by setting Viper options.
;;     - In Vim, according to :help sentence, end of sentence is:
;;         - '.', '?', or '!'
;;         - then (optionally) one or more '"', ''', ')', and ']'
;;           characters
;;         - then a newline, space, or tab.
;;         - A paragraph or section boundary is also a sentence
;;           boundary, but I bet Viper handles that, and if it doesn't,
;;           it should.
;;             - A paragraph begins after each truly empty line (no
;;               whitespace chars on it) or after certain col-1 nroff
;;               macros.  A sentence begins after a form feed (^L), or
;;               certain nroff macros, in column 1.
;;             - The characters '{' and '}' sometimes affect paragraph
;;               definitions.  See :help paragraph.
;;     - In Viper, on the other hand, I bet sentences are like in vi,
;;       where tabs aren't whitespace, and you need at least two spaces
;;       after the punctuation mark.
;;
;; - Implement smartcase searching.
;;
;; - Try to get Vimpulse included with upstream Viper; also, ideally,
;;   if you pressed "v" in Viper, Viper would offer to load Vimpulse.
;;   (Likely to happen?  Consider that Michael Kifer, the Viper
;;   maintainer, told me he doesn't need Vim keys.  Then again, maybe I
;;   could convince him that it's worth it to ship Vim keys, for other
;;   people's benefit.)  Also, consider that some of the code (like
;;   Operator-Pending mode) addresses problems mentioned in viper.el.
;;
;; - E-mail ridip <rdp at inthefaith.net> and ask him for his Vimpulse
;;   contribs and his DVORAK stuff.
;;
;; - E-mail to Tromey for upload into ELPA?  We'd have to redo this
;;   when a new major version comes out.  Or maybe we should just
;;   contribute some auto-ELPA-management code.  By the way, should we
;;   move Vimpulse into CVS somewhere?
;;
;; - Maybe merge all feature requests that anyone has ever sent into a
;;   "Feature requests" section here.

;;; Development plans:

;; The design plan for Vimpulse is for it to only emulate features
;; that are in Vim.  Therefore, other features do not belong in
;; Vimpulse unless one can get the Vim people to implement those
;; features too.
;;
;; At the same time, Vimpulse should strive for customizability and
;; extensibility, so that the user can modify it just as easily as the
;; rest of Emacs.

;;; Undecided development questions:

;; - In Vimpulse, like in real Vim, C-r only does Redo in vi (command)
;;   mode; in Insert mode it does something else.  (In Vimpulse that
;;   "something else" is reverse isearch.)  Should it do reverse
;;   isearch in Insert mode too?
;;
;; - In Vim, when a line starts with a "// " or ";; " comment and one
;;   presses enter, Vim extends the comment onto the next line.  What
;;   Vim function is it that does this?  Is the function enabled in
;;   plain vanilla Vim 7 as shipped by vim.org?  (Check by seeing how
;;   it works on Vim for Windows running on either Windows or Wine.)
;;   Is it mostly useful or mostly annoying?  Is it worth implementing
;;   in Emacs, considering there are other easy ways to create
;;   comments?
;;
;; - With some delete commands, Viper shows a message like "Deleted 50
;;   characters" in the minibuffer.  Is that annoying?
;;     - Update 1 month later: I hardly notice the message.
;;     - Dear users: Do you think I should disable the message?
;;
;; - I want to allow buffer-switching without using the C-x key, since
;;   C-x b RET an extremely large amount of times per day is
;;   uncomfortable for my right pinky, which presses RET.  There's
;;   already :b which seems to just invoke `switch-to-buffer'.  Is this
;;   right?  Is it bad if I make Vimpulse emulate set autowrite=on
;;   then write new multi-buffer code?  What should the code's user
;;   interface be like?  I really should switch back to Vim for a day,
;;   learn more about how it deals with multiple buffers at once (and
;;   maybe also with tab pages) and emulate whatever of Vim's is most
;;   convenient.  What do you think of all the above?\
;;     - Update: IIRC :set hidden lets you switch buffers w/o saving
;;     - Update from Sebastien Rocca Serra: :set wildmenu plus
;;       tab-completion makes :b very pleasant to use when you have
;;       50+ buffers open.  Wildmenu is almost like iswitchb or ido.
;;     - I wonder how well that stuff works with just a few buffers open.
;;
;; - Simulate Vim's set virtualedit=onemore option to make C-x C-e
;;   possible without first advancing to next line?
;;
;; - Would it be bad to edit users' .viminfo files without asking
;;   permission, or should some variable have to be customized on to do
;;   such a thing?
;;
;; - Is there any need to implement Vim's new
;;   [count]dk-can-go-past-top-of-file-without-error functionality (to
;;   me, no need) or any related functionality?
;;
;; - What to do about XEmacs?  It doesn't ship with woman.  I wonder
;;   if woman is in some XEmacs package?

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Code:

(defconst vimpulse-version "0.4"
  "The current version of Vimpulse")

;; Load Viper
(unless (and (boundp 'viper-mode) viper-mode)
  (setq viper-mode t)
  (setq viper-inhibit-startup-message t)
  (setq viper-expert-level 5)
  (setq viper-want-ctl-h-help t))
(require 'viper)

;; Load redo.el if available. Sadly we can't use APEL's require
;; function to get 'noerror functionality because GNU Emacs 21
;; doesn't ship with APEL included.
(unless (featurep 'redo)
  (condition-case nil
      (require 'redo)
    (error nil)))

;;; Customization group for Vimpulse

(defgroup vimpulse nil
  "Vim emulation within Emacs."
  :group  'emulations
  :link   '(custom-group-link "viper")
  :prefix 'vimpulse-)

(defcustom vimpulse-experimental t
  "Whether or not to use experimental features.
Turned on by default, so you will give feedback :P"
  :group 'vimpulse
  :type  'boolean)

;; The secrets discovered from untold diggings among
;; the ruins of Customize code
(defun vimpulse-custom-value-p (symbol)
  "Non-nil if SYMBOL has a customized value."
  (or (get symbol 'customized-value)
      (get symbol 'customized-face)
      (get symbol 'saved-value)))

(defmacro vimpulse-setq-custom (sym val)
  "Set the customized value of SYM to VAL."
  `(prog1 (setq ,sym ,val)              ; return VAL
     (when (get ',sym 'custom-autoload)
       (custom-load-symbol ',sym))
     (put ',sym 'customized-value (list (custom-quote ,val)))))

(defmacro vimpulse-setq-custom-default (symbol value)
  "Set the customized default value of SYMBOL to VALUE."
  `(prog1 ,value                        ; return VALUE
     (when (get ',symbol 'custom-autoload)
       (custom-load-symbol ',symbol))
     (put ',symbol 'standard-value (list (custom-quote ,value)))))

(defmacro vimpulse-setq (sym val)
  "Set SYM to VAL, defaults included, unless SYM is customized.
SYM is unquoted. Returns VAL."
  `(cond
    ;; Customized value: just set custom standard value
    ((vimpulse-custom-value-p ',sym)
     (vimpulse-setq-custom-default ,sym ,val))
    ;; Customized variable: set custom and regular values
    ((custom-variable-p ',sym)
     (vimpulse-setq-custom-default ,sym ,val)
     (vimpulse-setq-custom ,sym ,val)
     (setq-default ,sym ,val)
     (setq ,sym ,val))
    ;; Regular variable; set default and local values
    (t
     (setq-default ,sym ,val)
     (setq ,sym ,val))))

;;; Carefully set Viper/woman variables

(defun vimpulse-initialize-variables ()
  "Set various variables, unless customized."
  ;; Can backspace past start of insert/line
  (vimpulse-setq viper-ex-style-editing nil)
  ;; Don't create new frame for manpages
  (vimpulse-setq woman-use-own-frame nil)
  ;; Don't prompt upon K key (manpage display)
  (vimpulse-setq woman-use-topic-at-point t)
  ;; No start-up message
  (vimpulse-setq viper-inhibit-startup-message t)
  ;; Viper expert level 5
  (vimpulse-setq viper-expert-level 5)
  ;; Make cursor color consistent
  (vimpulse-setq viper-insert-state-cursor-color
                 viper-vi-state-cursor-color)
  ;; Cursor moves backwards when exiting Insert state
  (vimpulse-setq viper-ESC-moves-cursor-back t)
  ;; Not in Vim: C-h is indispensable in Emacs
  (vimpulse-setq viper-want-ctl-h-help t)
  ;; Refresh Viper settings
  (viper-change-state-to-vi))

(if (and (boundp 'after-init-time) after-init-time)
    (vimpulse-initialize-variables)
  (add-hook 'after-init-hook 'vimpulse-initialize-variables))

(provide 'vimpulse-dependencies)

;;;; Redefinitions of some of Viper's functions

(defcustom vimpulse-want-change-state nil
  "Whether commands like \"cw\" invoke Replace state, vi-like.
The default is to delete the text and enter Insert state,
like in Vim."
  :group 'vimpulse
  :type  'boolean)

(defadvice viper-change
  (around vimpulse-want-change-state activate)
  "Disable Replace state if `vimpulse-want-change-state' is nil."
  (cond
   (vimpulse-want-change-state
    ad-do-it)
   (t
    ;; We don't want Viper's Replace mode when changing text;
    ;; just delete and enter Insert state
    (setq viper-began-as-replace t)
    (kill-region beg end)
    (viper-change-state-to-insert))))

;;; Code for adding extra states

;; State index variables: for keeping track of which modes
;; belong to which states, et cetera
(defvar vimpulse-state-vars-alist
  '((vi-state
     (id . viper-vi-state-id)
     (change-func . viper-change-state-to-vi)
     (basic-mode . viper-vi-basic-minor-mode)
     (basic-map . viper-vi-basic-map)
     (diehard-mode . viper-vi-diehard-minor-mode)
     (diehard-map . viper-vi-diehard-map)
     (modifier-mode . viper-vi-state-modifier-minor-mode)
     (modifier-alist . viper-vi-state-modifier-alist)
     (kbd-mode . viper-vi-kbd-minor-mode)
     (kbd-map . viper-vi-kbd-map)
     (global-user-mode . viper-vi-global-user-minor-mode)
     (global-user-map . viper-vi-global-user-map)
     (local-user-mode . viper-vi-local-user-minor-mode)
     (local-user-map . viper-vi-local-user-map)
     (need-local-map . viper-need-new-vi-local-map)
     (intercept-mode . viper-vi-intercept-minor-mode)
     (intercept-map . viper-vi-intercept-map))
    (insert-state
     (id . viper-insert-state-id)
     (change-func . viper-change-state-to-insert)
     (basic-mode . viper-insert-basic-minor-mode)
     (basic-map . viper-insert-basic-map)
     (diehard-mode . viper-insert-diehard-minor-mode)
     (diehard-map . viper-insert-diehard-map)
     (modifier-mode . viper-insert-state-modifier-minor-mode)
     (modifier-alist . viper-insert-state-modifier-alist)
     (kbd-mode . viper-insert-kbd-minor-mode)
     (kbd-map . viper-insert-kbd-map)
     (global-user-mode . viper-insert-global-user-minor-mode)
     (global-user-map . viper-insert-global-user-map)
     (local-user-mode . viper-insert-local-user-minor-mode)
     (local-user-map . viper-insert-local-user-map)
     (need-local-map . viper-need-new-insert-local-map)
     (intercept-mode . viper-insert-intercept-minor-mode)
     (intercept-map . viper-insert-intercept-map))
    (replace-state
     (id . viper-replace-state-id)
     (change-func . viper-change-state-to-replace)
     (basic-mode . viper-replace-minor-mode)
     (basic-map . viper-replace-map))
    (emacs-state
     (id . viper-emacs-state-id)
     (change-func . viper-change-state-to-emacs)
     (modifier-mode . viper-emacs-state-modifier-minor-mode)
     (modifier-alist . viper-emacs-state-modifier-alist)
     (kbd-mode . viper-emacs-kbd-minor-mode)
     (kbd-map . viper-emacs-kbd-map)
     (global-user-mode . viper-emacs-global-user-minor-mode)
     (global-user-map . viper-emacs-global-user-map)
     (local-user-mode . viper-emacs-local-user-minor-mode)
     (local-user-map . viper-emacs-local-user-map)
     (need-local-map . viper-need-new-emacs-local-map)
     (intercept-mode . viper-emacs-intercept-minor-mode)
     (intercept-map . viper-emacs-intercept-map)))
  "Alist of Vimpulse state variables.
Entries have the form (STATE . ((VAR-TYPE . VAR) ...)).
For example, the basic state keymap has the VAR-TYPE `basic-map'.")

(defvar vimpulse-state-modes-alist
  '((vi-state
     (viper-vi-intercept-minor-mode . t)
     (viper-vi-minibuffer-minor-mode . (viper-is-in-minibuffer))
     (viper-vi-local-user-minor-mode . t)
     (viper-vi-global-user-minor-mode . t)
     (viper-vi-kbd-minor-mode . (not (viper-is-in-minibuffer)))
     (viper-vi-state-modifier-minor-mode . t)
     (viper-vi-diehard-minor-mode
      . (not (or viper-want-emacs-keys-in-vi
                 (viper-is-in-minibuffer))))
     (viper-vi-basic-minor-mode . t))
    (insert-state
     (viper-insert-intercept-minor-mode . t)
     (viper-replace-minor-mode . (eq state 'replace-state))
     (viper-insert-minibuffer-minor-mode . (viper-is-in-minibuffer))
     (viper-insert-local-user-minor-mode . t)
     (viper-insert-global-user-minor-mode . t)
     (viper-insert-kbd-minor-mode . (not (viper-is-in-minibuffer)))
     (viper-insert-state-modifier-minor-mode . t)
     (viper-insert-diehard-minor-mode
      . (not (or viper-want-emacs-keys-in-insert
                 (viper-is-in-minibuffer))))
     (viper-insert-basic-minor-mode . t))
    (replace-state
     (viper-insert-intercept-minor-mode . t)
     (viper-replace-minor-mode . (eq state 'replace-state))
     (viper-insert-minibuffer-minor-mode . (viper-is-in-minibuffer))
     (viper-insert-local-user-minor-mode . t)
     (viper-insert-global-user-minor-mode . t)
     (viper-insert-kbd-minor-mode . (not (viper-is-in-minibuffer)))
     (viper-insert-state-modifier-minor-mode . t)
     (viper-insert-diehard-minor-mode
      . (not (or viper-want-emacs-keys-in-insert
                 (viper-is-in-minibuffer))))
     (viper-insert-basic-minor-mode . t))
    (emacs-state
     (viper-emacs-intercept-minor-mode . t)
     (viper-emacs-local-user-minor-mode . t)
     (viper-emacs-global-user-minor-mode . t)
     (viper-emacs-kbd-minor-mode . (not (viper-is-in-minibuffer)))
     (viper-emacs-state-modifier-minor-mode . t)))
  "Alist of Vimpulse state mode toggling.
Entries have the form (STATE . ((MODE . EXPR) ...)), where STATE
is the name of a state, MODE is a mode associated with STATE and
EXPR is an expression with which to enable or disable MODE.
The first modes get the highest priority.")

(defvar vimpulse-state-maps-alist nil
  "Alist of Vimpulse modes and keymaps.
Entries have the form (MODE . MAP-EXPR), where MAP-EXPR is an
expression for determining the keymap of MODE.")

;; State-changing code: this uses the variables above
(defadvice viper-normalize-minor-mode-map-alist
  (after vimpulse-states activate)
  "Normalize Vimpulse state maps."
  (let (temp mode map alists toggle toggle-alist)
    ;; Determine which of `viper--key-maps' and
    ;; `minor-mode-map-alist' to normalize
    (cond
     ((featurep 'xemacs)
      (setq alists '(viper--key-maps minor-mode-map-alist)))
     ((>= emacs-major-version 22)
      (setq alists '(viper--key-maps)))
     (t
      (setq alists '(minor-mode-map-alist))))
    ;; Normalize the modes in the order
    ;; they are toggled by the current state
    (dolist (entry (reverse (cdr (assq viper-current-state
                                       vimpulse-state-modes-alist))))
      (setq mode (car entry)
            map (eval (cdr (assq mode vimpulse-state-maps-alist))))
      (when map
        (dolist (alist alists)
          (setq temp (default-value alist))
          (setq temp (assq-delete-all mode temp)) ; already there?
          (add-to-list 'temp (cons mode map))
          (set-default alist temp)
          (setq temp (eval alist))
          (setq temp (assq-delete-all mode temp))
          (add-to-list 'temp (cons mode map))
          (set alist temp))))))

(defadvice viper-refresh-mode-line (after vimpulse-states activate)
  "Refresh mode line tag for Vimpulse states."
  (let ((id (assq viper-current-state vimpulse-state-vars-alist)))
    (setq id (eval (cdr (assq 'id (cdr id)))))
    (when id
      (set (make-local-variable 'viper-mode-string) id)
      (force-mode-line-update))))

(defadvice viper-set-mode-vars-for (after vimpulse-states activate)
  "Toggle Vimpulse state modes."
  (let (enable disable)
    ;; Determine which modes to enable
    (setq enable (cdr (assq state vimpulse-state-modes-alist)))
    (when enable
      ;; Determine which modes to disable
      (dolist (entry vimpulse-state-modes-alist)
        (dolist (mode (mapcar 'car (cdr entry)))
          (unless (assq mode enable)
            (add-to-list 'disable mode t))))
      ;; Enable modes
      (dolist (entry enable)
        (when (boundp (car entry))
          (set (car entry) (eval (cdr entry)))))
      ;; Disable modes
      (dolist (entry disable)
        (when (boundp entry)
          (set entry nil))))))

(defadvice viper-change-state (before vimpulse-states activate)
  "Update `viper-insert-point'."
  (let (mark-active)
    (unless (mark t)
      (push-mark nil t nil)))
  (when (and (eq 'insert-state new-state)
             (not (memq viper-current-state '(vi-state emacs-state))))
    (viper-move-marker-locally 'viper-insert-point (point))))

(defun vimpulse-modifier-map (state &optional mode)
  "Return the current major mode modifier map for STATE.
If none, return an empty keymap (`viper-empty-keymap')."
  (setq mode (or mode major-mode))
  (setq state (assq state vimpulse-state-vars-alist))
  (setq state (eval (cdr (assq 'modifier-alist (cdr state)))))
  (if (keymapp (cdr (assoc mode state)))
      (cdr (assoc mode state))
    viper-empty-keymap))

(defun vimpulse-modify-major-mode (mode state keymap)
  "Modify key bindings in a major-mode in a Viper state using a keymap.

If the default for a major mode is emacs-state, then
modifications to this major mode may not take effect until the
buffer switches state to Vi, Insert or Emacs. If this happens,
add `viper-change-state-to-emacs' to this major mode's hook.
If no such hook exists, you may have to put an advice on the
function that invokes the major mode. See `viper-set-hooks'
for hints.

The above needs not to be done for major modes that come up in
Vi or Insert state by default."
  (let (alist elt)
    (setq alist (cdr (assq state vimpulse-state-vars-alist)))
    (setq alist (cdr (assq 'modifier-alist alist)))
    (if (setq elt (assoc mode (eval alist)))
        (set alist (delq elt (eval alist))))
    (set alist (cons (cons mode keymap) (eval alist)))
    (viper-normalize-minor-mode-map-alist)
    (viper-set-mode-vars-for viper-current-state)))

(fset 'viper-modify-major-mode 'vimpulse-modify-major-mode)

(defun vimpulse-add-local-keys (state alist)
  "Override some vi-state or insert-state bindings in the current buffer.
The effect is seen in the current buffer only.
Useful for customizing  mailer buffers, gnus, etc.
STATE is 'vi-state, 'insert-state, or 'emacs-state
ALIST is of the form ((key . func) (key . func) ...)
Normally, this would be called from a hook to a major mode or
on a per buffer basis.
Usage:
      (viper-add-local-keys state '((key-str . func) (key-str . func)...))"
  (let (local-user-map need-local-user-map)
    (setq local-user-map (cdr (assq state vimpulse-state-vars-alist)))
    (when local-user-map
      (setq need-local-user-map
            (cdr (assq 'need-local-user-map local-user-map)))
      (setq local-user-map
            (cdr (assq 'local-user-map local-user-map)))
      (when (eval need-local-user-map)
        (set local-user-map (make-sparse-keymap))
        (set need-local-user-map nil))
      (viper-modify-keymap (eval local-user-map) alist)
      (viper-normalize-minor-mode-map-alist)
      (viper-set-mode-vars-for viper-current-state))))

(fset 'viper-add-local-keys 'vimpulse-add-local-keys)

;; Macro for defining new Viper states. This saves us the trouble of
;; defining and indexing all those minor modes manually.
(defmacro vimpulse-define-state (state doc &rest body)
  "Define a new Viper state STATE.
DOC is a general description and shows up in all docstrings.
Then follows one or more optional keywords:

:id ID                  Mode line indicator.
:hook LIST              Hooks run before changing to STATE.
:change-func FUNC       Function to change to STATE.
:basic-mode MODE        Basic minor mode for STATE.
:basic-map MAP          Keymap of :basic-mode.
:diehard-mode MODE      Minor mode for when Viper want to be vi.
:diehard-map MAP        Keymap of :diehard-mode.
:modifier-mode MODE     Minor mode for modifying major modes.
:modifier-alist LIST    Keymap alist for :modifier-mode.
:kbd-mode MODE          Minor mode for Ex command macros.
:kbd-map MAP            Keymap of :kbd-mode.
:global-user-mode MODE  Minor mode for global user bindings.
:global-user-map MAP    Keymap of :global-user-mode.
:local-user-mode MODE   Minor mode for local user bindings.
:local-user-map MAP     Keymap of :local-user-mode.
:need-local-map VAR     Buffer-local variable for :local-user-mode.
:intercept-mode         Minor mode for vital Viper bindings.
:intercept-map          Keymap of :intercept-mode.
:enable LIST            List of other modes enabled by STATE.
:prefix PREFIX          Variable prefix, default \"vimpulse-\".
:advice TYPE            Toggle advice type, default `after'.

It is not necessary to specify all of these; the minor modes are
created automatically unless you provide an existing mode. The
only keyword you should really specify is :id, the mode line tag.
For example:

    (vimpulse-define-state test
      \"A simple test state.\"
      :id \"<T> \")

The basic keymap of this state will then be
`vimpulse-test-basic-map', and so on.

Following the keywords is optional code to be executed each time
the state is enabled or disabled. This is stored in a `defadvice'
of `viper-change-state'. :advice specifies the advice type
\(default `after'). The advice runs :hook before completing."
  (declare (debug (&define name stringp
                           [&rest [keywordp sexp]]
                           def-body))
           (indent defun))
  (let (advice basic-map basic-mode change change-func diehard-map
               diehard-mode enable enable-modes-alist enable-states-alist
               global-user-map global-user-mode hook id id-string
               intercept-map intercept-mode kbd-map kbd-mode keyword
               local-user-map local-user-mode modes-alist modifier-alist
               modifier-mode name name-string need-local-map prefix
               prefixed-name-string state-name state-name-string vars-alist)
    ;; Collect keywords
    (while (keywordp (setq keyword (car body)))
      (setq body (cdr body))
      (cond
       ((eq :prefix keyword)
        (setq prefix (vimpulse-unquote (pop body))))
       ((eq :enable keyword)
        (setq enable (vimpulse-unquote (pop body))))
       ((eq :advice keyword)
        (setq advice (vimpulse-unquote (pop body))))
       ((memq keyword '(:state-id :id))
        (setq id (vimpulse-unquote (pop body))))
       ((memq keyword '(:state-hook :hook))
        (setq hook (vimpulse-unquote (pop body))))
       ((memq keyword '(:change-func :change))
        (setq change-func (vimpulse-unquote (pop body))))
       ((memq keyword '(:basic-mode :basic-minor-mode))
        (setq basic-mode (vimpulse-unquote (pop body))))
       ((eq :basic-map keyword)
        (setq basic-map (vimpulse-unquote (pop body))))
       ((memq keyword '(:local-user-mode
                        :local-mode
                        :local-user-minor-mode))
        (setq local-user-mode (vimpulse-unquote (pop body))))
       ((memq keyword '(:local-user-map :local-map))
        (setq local-user-map (vimpulse-unquote (pop body))))
       ((memq keyword '(:need-new-local-map
                        :need-local-map
                        :need-map))
        (setq need-local-map (vimpulse-unquote (pop body))))
       ((memq keyword '(:global-user-mode
                        :global-mode
                        :global-user-minor-mode))
        (setq global-user-mode (vimpulse-unquote (pop body))))
       ((memq keyword '(:global-user-map :global-map))
        (setq global-user-map (vimpulse-unquote (pop body))))
       ((memq keyword '(:state-modifier-minor-mode
                        :state-modifier-mode
                        :modifier-minor-mode
                        :modifier-mode))
        (setq modifier-mode (vimpulse-unquote (pop body))))
       ((memq keyword '(:state-modifier-alist :modifier-alist))
        (setq modifier-alist (vimpulse-unquote (pop body))))
       ((memq keyword '(:diehard-mode :diehard-minor-mode))
        (setq diehard-mode (vimpulse-unquote (pop body))))
       ((eq :diehard-map keyword)
        (setq diehard-map (vimpulse-unquote (pop body))))
       ((memq keyword '(:kbd-mode :kbd-minor-mode))
        (setq kbd-mode (vimpulse-unquote (pop body))))
       ((eq :kbd-map keyword)
        (setq kbd-map (vimpulse-unquote (pop body))))
       ((memq keyword '(:intercept-mode :intercept-minor-mode))
        (setq intercept-mode (vimpulse-unquote (pop body))))
       ((eq :intercept-map keyword)
        (setq intercept-map (vimpulse-unquote (pop body))))
       (t
        (pop body))))
    ;; Set up the state name
    (setq name-string (replace-regexp-in-string
                       "-state$" "" (symbol-name state)))
    (setq name (intern name-string))
    (setq state-name-string (concat name-string "-state"))
    (setq state-name (intern state-name-string))
    (when (and prefix (symbolp prefix))
      (setq prefix (symbol-name prefix)))
    (setq prefix (or prefix "vimpulse-"))
    (setq prefix (concat (replace-regexp-in-string
                          "-$" "" prefix) "-"))
    (setq prefixed-name-string (concat prefix name-string))
    ;; Create state variables
    (setq id
          (vimpulse-define-symbol
           id (concat prefixed-name-string "-state-id")
           (format "<%s> " (upcase name-string)) 'stringp
           (format "Mode line tag indicating %s.\n\n%s"
                   state-name doc)))
    (setq hook
          (vimpulse-define-symbol
           hook (concat prefixed-name-string "-state-hook")
           nil 'listp (format "*Hooks run just before the switch to %s \
is completed.\n\n%s" state-name doc)))
    (setq basic-mode
          (vimpulse-define-symbol
           basic-mode
           (concat prefixed-name-string "-basic-minor-mode")
           nil nil (format "Basic minor mode for %s.\n\n%s"
                           state-name doc) t))
    (setq basic-map
          (vimpulse-define-symbol
           basic-map (concat prefixed-name-string "-basic-map")
           (make-sparse-keymap) 'keymapp
           (format "The basic %s keymap.\n\n%s" state-name doc)))
    (setq diehard-mode
          (vimpulse-define-symbol
           diehard-mode
           (concat prefixed-name-string "-diehard-minor-mode")
           nil nil (format "This minor mode is in effect when \
the user wants Viper to be vi.\n\n%s" doc) t))
    (setq diehard-map
          (vimpulse-define-symbol
           diehard-map
           (concat prefixed-name-string "-diehard-map")
           (make-sparse-keymap) 'keymapp
           (format "This keymap is in use when the user asks \
Viper to simulate vi very closely.
This happens when `viper-expert-level' is 1 or 2.  \
See `viper-set-expert-level'.\n\n%s" doc)))
    (setq modifier-mode
          (vimpulse-define-symbol
           modifier-mode
           (concat prefixed-name-string "-state-modifier-minor-mode")
           nil nil (format "Minor mode used to make major \
mode-specific modifications to %s.\n\n%s" state-name doc) t))
    (setq modifier-alist
          (vimpulse-define-symbol
           modifier-alist
           (concat prefixed-name-string "-state-modifier-alist")
           nil 'listp))
    (setq kbd-mode
          (vimpulse-define-symbol
           kbd-mode
           (concat prefixed-name-string "-kbd-minor-mode")
           nil nil
           (format "Minor mode for Ex command macros in Vi state.
The corresponding keymap stores key bindings of Vi macros defined with
the Ex command :map.\n\n%s" doc) t))
    (setq kbd-map
          (vimpulse-define-symbol
           kbd-map
           (concat prefixed-name-string "-kbd-map")
           (make-sparse-keymap) 'keymapp
           (format "This keymap keeps keyboard macros defined \
via the :map command.\n\n%s" doc)))
    (setq global-user-mode
          (vimpulse-define-symbol
           global-user-mode
           (concat prefixed-name-string "-global-user-minor-mode")
           nil nil (format "Auxiliary minor mode for global \
user-defined bindings in %s.\n\n%s" state-name doc) t))
    (setq global-user-map
          (vimpulse-define-symbol
           global-user-map
           (concat prefixed-name-string "-global-user-map")
           (make-sparse-keymap) 'keymapp
           (format "Auxiliary map for global user-defined keybindings \
in %s.\n\n%s" state-name doc)))
    (setq local-user-mode
          (vimpulse-define-symbol
           local-user-mode
           (concat prefixed-name-string "-local-user-minor-mode")
           nil nil (format "Auxiliary minor mode for user-defined \
local bindings in %s.\n\n%s" state-name doc) t))
    (setq local-user-map
          (vimpulse-define-symbol
           local-user-map
           (concat prefixed-name-string "-local-user-map")
           (make-sparse-keymap) 'keymapp
           (format "Auxiliary map for per-buffer user-defined \
keybindings in %s.\n\n%s" state-name doc) t))
    (setq need-local-map
          (vimpulse-define-symbol
           need-local-map
           (concat prefix "need-new-" name-string "-local-map")
           t (lambda (val) (eq val t)) nil t))
    (put need-local-map 'permanent-local t)
    (setq intercept-mode
          (vimpulse-define-symbol
           intercept-mode
           (concat prefixed-name-string "-intercept-minor-mode")
           nil nil
           (format "Mode for binding Viper's vital keys.\n\n%s" doc)))
    (setq intercept-map
          (vimpulse-define-symbol
           intercept-map
           (concat prefixed-name-string "-intercept-map")
           viper-vi-intercept-map 'keymapp
           (format "Keymap for binding Viper's vital keys.\n\n%s" doc)))
    ;; Set up change function
    (if (and change-func (symbolp change-func))
        (setq change change-func)
      (setq change
            (intern (concat prefix "change-state-to-" name-string))))
    (unless (functionp change-func)
      (setq change-func
            `(lambda ()
               ,(format "Change Viper state to %s." state-name)
               (viper-change-state ',state-name))))
    (unless (fboundp change)
      (fset change change-func))
    ;; Remove old index entries
    (dolist (entry (list basic-mode
                         diehard-mode
                         modifier-mode
                         kbd-mode
                         global-user-mode
                         local-user-mode
                         intercept-mode))
      (setq vimpulse-state-maps-alist
            (assq-delete-all entry vimpulse-state-maps-alist)))
    (setq vimpulse-state-modes-alist
          (assq-delete-all state-name vimpulse-state-modes-alist))
    (setq vimpulse-state-vars-alist
          (assq-delete-all state-name vimpulse-state-vars-alist))
    ;; Index keymaps
    (add-to-list 'vimpulse-state-maps-alist
                 (cons basic-mode basic-map))
    (add-to-list 'vimpulse-state-maps-alist
                 (cons diehard-mode diehard-map))
    (add-to-list 'vimpulse-state-maps-alist
                 (cons modifier-mode
                       `(if (keymapp
                             (cdr (assoc major-mode
                                         ,modifier-alist)))
                            (cdr (assoc major-mode
                                        ,modifier-alist)))))
    (add-to-list 'vimpulse-state-maps-alist
                 (cons kbd-mode kbd-map))
    (add-to-list 'vimpulse-state-maps-alist
                 (cons global-user-mode global-user-map))
    (add-to-list 'vimpulse-state-maps-alist
                 (cons local-user-mode local-user-map))
    (add-to-list 'vimpulse-state-maps-alist
                 (cons intercept-mode intercept-map))
    ;; Index minor mode toggling.
    ;; First, sort lists from symbols in :enable.
    (unless (listp enable)
      (setq enable (list enable)))
    (dolist (entry enable)
      (let ((mode entry) (val t))
        (when (listp entry)
          (setq mode (car entry)
                val (cadr entry)))
        (when (and mode (symbolp mode))
          (add-to-list 'enable-modes-alist (cons mode val) t))))
    ;; Then add the state's own modes to the front
    ;; if they're not already there
    (dolist (mode (list (cons basic-mode t)
                        (cons diehard-mode
                              '(not (or viper-want-emacs-keys-in-vi
                                        (viper-is-in-minibuffer))))
                        (cons modifier-mode t)
                        (cons kbd-mode '(not (viper-is-in-minibuffer)))
                        (cons global-user-mode t)
                        (cons local-user-mode t)
                        (cons intercept-mode t)))
      (unless (assq (car mode) enable-modes-alist)
        (add-to-list 'enable-modes-alist mode)))
    ;; Add the result to `vimpulse-state-modes-alist'
    ;; and update any state references therein
    (add-to-list 'vimpulse-state-modes-alist
                 (cons state-name enable-modes-alist) t)
    (vimpulse-refresh-state-modes-alist)
    (viper-normalize-minor-mode-map-alist)
    ;; Index state variables
    (setq vars-alist
          (list (cons 'id id)
                (cons 'hook hook)
                (cons 'change-func change-func)
                (cons 'basic-mode basic-mode)
                (cons 'basic-map basic-map)
                (cons 'diehard-mode diehard-mode)
                (cons 'diehard-map diehard-map)
                (cons 'modifier-mode modifier-mode)
                (cons 'modifier-alist modifier-alist)
                (cons 'kbd-mode kbd-mode)
                (cons 'kbd-map kbd-map)
                (cons 'global-user-mode global-user-mode)
                (cons 'global-user-map global-user-map)
                (cons 'local-user-mode local-user-mode)
                (cons 'local-user-map local-user-map)
                (cons 'need-local-map need-local-map)
                (cons 'intercept-mode intercept-mode)
                (cons 'intercept-map intercept-map)))
    (add-to-list 'vimpulse-state-vars-alist
                 (cons state-name vars-alist) t)
    ;; Make toggle-advice (this is the macro expansion)
    (setq advice (or advice 'after))
    `(defadvice viper-change-state (,advice ,state-name activate)
       ,(format "Toggle %s." state-name)
       ,@body
       (when (eq ',state-name new-state)
         (run-hooks ',hook)))))

(when (fboundp 'font-lock-add-keywords)
  (font-lock-add-keywords
   'emacs-lisp-mode
   '(("(\\(vimpulse-define-state\\)\\>[ \f\t\n\r\v]*\\(\\sw+\\)?"
      (1 font-lock-keyword-face)
      (2 font-lock-function-name-face nil t)))))

;; These are for making `vimpulse-define-state' more forgiving
(defun vimpulse-unquote (exp)
  "Return EXP unquoted."
  (if (and (listp exp)
           (eq 'quote (car exp)))
      (eval exp)
    exp))

(defun vimpulse-define-symbol
  (sym-or-val varname varval &optional val-p doc local)
  "Accept a symbol or a value and define a variable for it.
If SYM-OR-VAL is a symbol, set that symbol's value to VARVAL.
If SYM-OR-VAL is a value, set VARNAME's value to SYM-OR-VAL.
VAL-P checks whether SYM-OR-VAL's value is \"valid\", in which
case it is kept; otherwise we default to VARVAL. DOC is the
docstring for the defined variable. If LOCAL is non-nil,
create a buffer-local variable. Returns the result."
  (cond
   ((and sym-or-val (symbolp sym-or-val)) ; nil is a symbol
    (setq varname sym-or-val))
   ((or (not val-p) (funcall val-p sym-or-val))
    (setq varval sym-or-val)))
  (when (stringp varname)
    (setq varname (intern varname)))
  (unless (and (boundp varname) val-p
               (funcall val-p (eval varname)))
    (eval `(defvar ,varname (quote ,varval) ,doc))
    (set varname varval)
    (when local
      (make-variable-buffer-local varname)))
  varname)

(defun vimpulse-refresh-state-modes-alist (&optional state &rest states)
  "Expand state references in `vimpulse-state-modes-alist'."
  (cond
   (state
    (let* ((state-entry (assq state vimpulse-state-modes-alist))
           (state-list (cdr state-entry))
           mode toggle)
      (setq state-entry nil)
      (dolist (modes (reverse state-list) state-entry)
        (setq mode (car modes))
        (setq toggle (cdr modes))
        (if (and (assq mode vimpulse-state-modes-alist)
                 (not (eq mode state))
                 (not (memq mode states)))
            (setq modes (vimpulse-refresh-state-modes-alist
                         mode (append (list state) states)))
          (setq modes (list modes)))
        (dolist (entry (reverse modes) state-entry)
          (setq state-entry (assq-delete-all (car entry) state-entry))
          (if toggle
              (add-to-list 'state-entry entry)
            (add-to-list 'state-entry (cons (car entry) nil)))))))
   (t
    (dolist (state-entry vimpulse-state-modes-alist)
      (setq state (car state-entry))
      (setq state-entry
            (vimpulse-refresh-state-modes-alist state))
      (setq vimpulse-state-modes-alist
            (assq-delete-all state vimpulse-state-modes-alist))
      (add-to-list 'vimpulse-state-modes-alist
                   (cons state state-entry) t)))))

;;; Viper bugs (should be forwarded to Kifer)

;; `viper-deflocalvar's definition lacks a `declare' statement,
;; so Emacs tends to mess up the indentation. Luckily, the
;; relevant symbol properties can be set up with `put'.
;; TODO: E-mail Michael Kifer about updating the definition
(put 'viper-deflocalvar 'lisp-indent-function 'defun)
(put 'viper-loop 'lisp-indent-function 'defun)
(put 'viper-deflocalvar 'function-documentation
     "Define VAR as a buffer-local variable.
DEFAULT-VALUE is the default value and DOCUMENTATION is the
docstring. The variable becomes buffer-local whenever set.")

(when (fboundp 'font-lock-add-keywords)
  (font-lock-add-keywords
   'emacs-lisp-mode
   '(("(\\(viper-deflocalvar\\)\\>[ \f\t\n\r\v]*\\(\\sw+\\)?"
      (1 font-lock-keyword-face)
      (2 font-lock-variable-name-face nil t))
     ("(\\(viper-loop\\)\\>" 1 font-lock-keyword-face))))

;; e/E bug: on a single-letter word, ce may change two words
(defun vimpulse-end-of-word-kernel ()
  (when (viper-looking-at-separator)
    (viper-skip-all-separators-forward))
  (cond
   ((viper-looking-at-alpha)
    (viper-skip-alpha-forward "_"))
   ((not (viper-looking-at-alphasep))
    (viper-skip-nonalphasep-forward))))

(defun vimpulse-end-of-word (arg &optional careful)
  "Move point to end of current word."
  (interactive "P")
  (viper-leave-region-active)
  (let ((val (viper-p-val arg))
        (com (viper-getcom arg)))
    (cond
     (com
      (viper-move-marker-locally 'viper-com-point (point))
      (when (and (not (viper-looking-at-alpha))
                 (not (viper-looking-at-alphasep)))
        (setq val (1+ val))))
     ((viper-end-of-word-p)
      (setq val (1+ val))))
    (viper-loop val (viper-end-of-word-kernel))
    (if com
        (viper-execute-com 'viper-end-of-word val com)
      (viper-backward-char-carefully))))

(defun vimpulse-end-of-Word (arg)
  "Forward to end of word delimited by white character."
  (interactive "P")
  (viper-leave-region-active)
  (let ((val (viper-p-val arg))
        (com (viper-getcom arg)))
    (cond
     (com
      (viper-move-marker-locally 'viper-com-point (point))
      (when (and (not (viper-looking-at-alpha))
                 (not (viper-looking-at-alphasep)))
        (setq val (1+ val))))
     ((viper-end-of-word-p)
      (setq val (1+ val))))
    (viper-loop val
      (viper-end-of-word-kernel)
      (viper-skip-nonseparators 'forward))
    (if com
        (viper-execute-com 'viper-end-of-word val com)
      (viper-backward-char-carefully))))

(fset 'viper-end-of-word-kernel 'vimpulse-end-of-word-kernel)
(fset 'viper-end-of-word 'vimpulse-end-of-word)
(fset 'viper-end-of-Word 'vimpulse-end-of-Word)

(provide 'vimpulse-viper-function-redefinitions)

;;;; General utility code used by all of Vimpulse;
;;;; may be useful to the end user

;;; Autogenerated vi bindings

(defvar vimpulse-viper-movement-cmds
  '(viper-backward-Word viper-backward-char viper-backward-paragraph
    viper-backward-sentence viper-backward-word
    viper-beginning-of-line viper-command-argument
    viper-digit-argument viper-end-of-Word viper-end-of-word
    viper-exec-mapped-kbd-macro viper-find-char-backward
    viper-find-char-forward viper-forward-Word viper-forward-char
    viper-forward-paragraph viper-forward-sentence viper-forward-word
    viper-goto-char-backward viper-goto-char-forward viper-goto-eol
    viper-goto-line viper-line-to-bottom viper-line-to-middle
    viper-line-to-top viper-next-line viper-previous-line
    viper-scroll-down-one viper-scroll-down viper-scroll-up
    viper-scroll-up-one viper-window-bottom viper-window-middle
    viper-window-top vimpulse-end-of-previous-word
    vimpulse-goto-first-line vimpulse-goto-definition
    vimpulse-goto-line vimpulse-search-backward-for-symbol-at-point
    vimpulse-search-forward-for-symbol-at-point vimpulse-jump-backward
    vimpulse-jump-forward vimpulse-visual-toggle-normal
    vimpulse-visual-toggle-line vimpulse-visual-toggle-block)
  "List of Viper/Vimpulse movement commands.")

(defvar vimpulse-core-movement-cmds
  '(viper-backward-char
    viper-next-line
    viper-previous-line
    viper-forward-char
    viper-ex)
  "List of Viper \"core\" movement commands.
These should be present in every mode, to avoid confusion.")

(defun vimpulse-augment-keymap
  (map augment-alist &optional replace)
  "Augment MAP with bindings from AUGMENT-ALIST.
If REPLACE is non-nil, bindings in MAP may be overwritten.
AUGMENT-ALIST has the format ((KEY . DEF) ...),
where KEY and DEF are passed to `define-key'."
  (let (key def num)
    (dolist (binding augment-alist)
      (setq key (car binding)
            def (cdr binding)
            num (lookup-key map key))
      (cond
       (replace
        (when (numberp num)
          (define-key map (vimpulse-truncate key num) nil))
        (define-key map key def))
       (t
        (when (numberp num)
          (setq num (lookup-key map (vimpulse-truncate key num))))
        (unless num
          (define-key map key def)))))))

(defun vimpulse-add-vi-bindings (map cmds &optional replace filter)
  "Add vi bindings for CMDS to MAP.
Add forcefully if REPLACE is t. Don't add keys matching FILTER,
which is a list of key vectors."
  (let (pmap keys)
    (unless filter
      (when (and (boundp 'viper-want-ctl-h-help)
                 viper-want-ctl-h-help)
        (add-to-list 'filter [?\C-h]))
      (unless (and (boundp 'vimpulse-want-C-u-like-Vim)
                   vimpulse-want-C-u-like-Vim)
        (add-to-list 'filter [?\C-u])))
    (setq pmap (make-sparse-keymap))
    (dolist (cmd cmds map)
      (dolist (vimap (list viper-vi-intercept-map
                           viper-vi-local-user-map
                           viper-vi-global-user-map
                           viper-vi-kbd-map
                           viper-vi-diehard-map
                           viper-vi-basic-map))
        (setq keys (where-is-internal cmd vimap))
        (dolist (key keys)
          (unless (let (match)
                    (dolist (entry filter match)
                      (when (equal key (vimpulse-truncate
                                        entry (length key)))
                        (setq match t))))
            (when (or (not (lookup-key pmap key))
                      (numberp (lookup-key pmap key)))
              (vimpulse-augment-keymap map
                                       `((,key . ,cmd))
                                       replace)
              ;; To prioritize between maps in `vimap',
              ;; we keep track of bindings by augmenting `pmap'.
              (vimpulse-augment-keymap pmap
                                       `((,key . ,cmd))))))))))

(defun vimpulse-add-movement-cmds (map &optional replace)
  "Add Viper/Vimpulse movement commands to MAP.
The commands are taken from `vimpulse-viper-movement-cmds' and looked
up in vi keymaps. If REPLACE is non-nil, may overwrite bindings
in MAP."
  (vimpulse-add-vi-bindings map vimpulse-viper-movement-cmds replace))

;; The default for this function is to replace rather than augment,
;; as core navigation should be present everywhere
(defun vimpulse-add-core-movement-cmds (map &optional augment)
  "Add \"core\" movement commands to MAP, forcefully.
The commands are taken from `vimpulse-core-movement-cmds'.
If AUGMENT is non-nil, don't overwrite bindings in MAP."
  (vimpulse-add-vi-bindings map
                            vimpulse-core-movement-cmds
                            (not augment)))

(defun vimpulse-inhibit-movement-cmds (map &optional replace)
  "Remap Viper movement commands to `viper-nil' in MAP.
The commands are taken from `vimpulse-viper-movement-cmds'.
If REPLACE is non-nil, may overwrite bindings in MAP."
  (dolist (cmd vimpulse-viper-movement-cmds)
    (eval `(vimpulse-augment-keymap
            map '(([remap ,cmd] . viper-nil))
            replace))))

(defun vimpulse-inhibit-destructive-cmds (map &optional replace)
  "Remap destructive Viper commands to `viper-nil' in MAP.
This isn't complete since `viper-command-argument' is left out so
that yanking may work, but as change and delete fail silently in
read-only buffers anyway, it does the job."
  (dolist (cmd '(viper-Append
                 viper-Insert
                 viper-append
                 viper-change-to-eol
                 viper-insert
                 viper-kill-line
                 viper-substitute
                 viper-substitute-line
                 vimpulse-change
                 vimpulse-delete
                 vimpulse-visual-append
                 vimpulse-visual-insert))
    (eval `(vimpulse-augment-keymap
            map '(([remap ,cmd] . viper-nil))
            replace))))

(defmacro vimpulse-remap (keymap from to)
  "Remap FROM to TO in KEYMAP.
For XEmacs compatibility, KEYMAP should have a `remap-alist'
property referring to a variable used for storing a \"remap
association list\"."
  (if (featurep 'xemacs)
      `(let ((remap-alist (get ',keymap 'remap-alist))
             (from ,from) (to ,to))
         (when remap-alist
           (add-to-list remap-alist (cons from to))))
    `(let ((keymap ,keymap) (from ,from) (to ,to))
       (define-key keymap `[remap ,from] to))))

;;; Vector tools

(defun vimpulse-truncate (vector length &optional offset)
  "Return a copy of VECTOR truncated to LENGTH.
If LENGTH is negative, skip last elements of VECTOR.
If OFFSET is specified, skip first elements of VECTOR."
  ;; If LENGTH is too large, trim it
  (when (> length (length vector))
    (setq length (length vector)))
  ;; If LENGTH is negative, convert it to the positive equivalent
  (when (> 0 length)
    (setq length (+ (length vector) length)))
  (when (> 0 length)
    (setq length 0))
  (if offset
      (setq length (- length offset))
    (setq offset 0))
  (let ((result (make-vector length t)))
    (dotimes (idx length result)
      (aset result idx (aref vector (+ idx offset))))))

(defun vimpulse-memq-recursive (elt list)
  "Return t if ELT is an element of LIST.
LIST may be nested."
  (let ((this (car list))
        (rest (cdr list)))
    (cond
     ((eq this elt)
      t)
     ((and this (listp this)) ; nil is a list
      (vimpulse-memq-recursive elt this))
     (rest
      (vimpulse-memq-recursive elt rest)))))

;;; Movement

(defun vimpulse-move-to-column (column &optional dir force)
  "Move point to column COLUMN in the current line.
Places point at left of the tab character (at the right
if DIR is non-nil) and returns point.
If `vimpulse-visual-block-untabify' is non-nil, then
tabs are changed to spaces. (FORCE untabifies regardless.)"
  (interactive "p")
  (if (or vimpulse-visual-block-untabify force)
      (move-to-column column t)
    (move-to-column column)
    (when (or (not dir) (and (numberp dir) (> 1 dir)))
      (when (< column (current-column))
        (unless (bolp)
          (backward-char)))))
  (point))

(defmacro vimpulse-limit (start end &rest body)
  "Eval BODY, but limit point to buffer-positions START and END.
Both may be nil. Returns position."
  (declare (indent 2))
  `(let ((start (or ,start (point-min)))
         (end   (or ,end   (point-max))))
     (when (< end start)
       (setq start (prog1 end
                     (setq end start))))
     (save-restriction
       (narrow-to-region start end)
       ,@body
       (point))))

(defmacro vimpulse-skip (dir bounds &rest body)
  "Eval BODY, but limit point to BOUNDS in DIR direction.
Returns position."
  (declare (indent 2))
  `(let ((dir ,dir) (bounds ,bounds) start end)
     (setq dir (if (and (numberp dir) (> 0 dir)) -1 1))
     (dolist (bound bounds)
       (unless (numberp bound)
         (setq bounds (delq bound bounds))))
     (when bounds
       (if (> 0 dir)
           (setq start (apply 'min bounds))
         (setq end (apply 'max bounds))))
     (vimpulse-limit start end ,@body)))

(defun vimpulse-skip-regexp (regexp dir &rest bounds)
  "Move point in DIR direction based on REGEXP and BOUNDS.
REGEXP is passed to `looking-at' or `looking-back'.
If DIR is positive, move forwards to the end of the regexp match,
but not beyond any buffer positions listed in BOUNDS.
If DIR is negative, move backwards to the beginning of the match.
Returns the new position."
  (setq dir (if (and (numberp dir) (> 0 dir)) -1 1))
  (setq regexp (or regexp ""))
  (vimpulse-skip dir bounds
    (if (> 0 dir)
        (when (looking-back regexp nil t)
          (goto-char (match-beginning 0)))
      (when (looking-at regexp)
        (goto-char (match-end 0))))))

;; XEmacs only has `looking-at'
(unless (fboundp 'looking-back)
  (defun looking-back (regexp &optional limit greedy)
    "Return t if text before point matches regular expression REGEXP."
    (let ((start (point))
          (pos
           (save-excursion
             (and (re-search-backward
                   (concat "\\(?:" regexp "\\)\\=") limit t)
                  (point)))))
      (if (and greedy pos)
          (save-restriction
            (narrow-to-region (point-min) start)
            (while (and (> pos (point-min))
                        (save-excursion
                          (goto-char pos)
                          (backward-char 1)
                          (looking-at
                           (concat "\\(?:" regexp "\\)\\'"))))
              (setq pos (1- pos)))
            (save-excursion
              (goto-char pos)
              (looking-at (concat "\\(?:"  regexp "\\)\\'")))))
      (not (null pos)))))

(defun vimpulse-backward-up-list (&optional arg)
  "Like `backward-up-list', but breaks out of strings."
  (interactive "p")
  (let ((orig (point)))
    (setq arg (or arg 1))
    (while (progn
             (condition-case
                 nil (backward-up-list arg)
               (error nil))
             (when (eq orig (point))
               (backward-char)
               (setq orig (point)))))))

;;; Region

(defun vimpulse-region-face ()
  "Return face of region."
  (if (featurep 'xemacs) 'zmacs-region 'region))

(defun vimpulse-deactivate-region (&optional now)
  "Deactivate region, respecting Emacs version."
  (cond
   ((and (boundp 'cua-mode) cua-mode)
    (cua--deactivate now))
   ((featurep 'xemacs)
    (let ((zmacs-region-active-p t))
      (zmacs-deactivate-region)))
   (now
    (setq mark-active nil))
   (t
    (setq deactivate-mark t))))

(defun vimpulse-activate-region (&optional pos)
  "Activate mark if there is one. Otherwise set mark at point.
If POS if specified, set mark at POS instead."
  (setq pos (or pos (mark t) (point)))
  (cond
   ((and (boundp 'cua-mode) cua-mode)
    (let ((opoint (point))
          (oldmsg (current-message))
          message-log-max
          cua-toggle-set-mark)
      (goto-char (or pos (mark t) (point)))
      (unwind-protect
          (cua-set-mark)
        (message oldmsg))
      (goto-char opoint)))
   (t
    (let (this-command)
      (push-mark pos t t)))))

(defun vimpulse-set-region (beg end &optional widen dir)
  "Set Emacs region to BEG and END.
Preserves the order of point and mark, unless specified by DIR:
a positive number means mark goes before or is equal to point,
a negative number means point goes before mark. If WIDEN is
non-nil, only modifies region if it does not already encompass
BEG and END. Returns nil if region is unchanged."
  (cond
   (widen
    (vimpulse-set-region
     (min beg end (or (region-beginning) (point)))
     (max beg end (or (region-end) (point)))
     nil dir))
   (t
    (unless (region-active-p)
      (vimpulse-activate-region))
    (let* ((oldpoint (point))
           (oldmark  (or (mark t) oldpoint))
           (newmark  (min beg end))
           (newpoint (max beg end)))
      (when (or (and (numberp dir) (> 0 dir))
                (and (not (numberp dir))
                     (< oldpoint oldmark)))
        (setq newpoint (prog1 newmark
                         (setq newmark newpoint))))
      (unless (or (and (numberp dir)
                       (= (min oldpoint oldmark)
                          (min newpoint newmark))
                       (= (max oldpoint oldmark)
                          (max newpoint newmark)))
                  (and (= oldpoint newpoint)
                       (= oldmark  newmark)))
        (set-mark newmark)
        (goto-char newpoint))))))

;;; Overlays (extents in XEmacs)

(cond
 ((featurep 'xemacs)                    ; XEmacs
  (fset 'vimpulse-delete-overlay 'delete-extent)
  (fset 'vimpulse-overlays-at 'extents-at))
 (t                                     ; GNU Emacs
  (fset 'vimpulse-delete-overlay 'delete-overlay)
  (fset 'vimpulse-overlays-at 'overlays-at)))

;; `viper-make-overlay' doesn't handle FRONT-ADVANCE
;; and REAR-ADVANCE properly in XEmacs
(defun vimpulse-make-overlay
  (beg end &optional buffer front-advance rear-advance)
  "Create a new overlay with range BEG to END in BUFFER.
In XEmacs, create an extent."
  (cond
   ((featurep 'xemacs)
    (let ((extent (make-extent beg end buffer)))
      (set-extent-property extent 'start-open front-advance)
      (set-extent-property extent 'end-closed rear-advance)
      (set-extent-property extent 'detachable nil)
      extent))
   (t
    (make-overlay beg end buffer front-advance rear-advance))))

(defun vimpulse-overlay-before-string (overlay string &optional face)
  "Set the `before-string' property of OVERLAY to STRING.
In XEmacs, change the `begin-glyph' property."
  (cond
   ((featurep 'xemacs)
    (setq face (or face (get-text-property 0 'face string)))
    (when (and string (not (glyphp string)))
      (setq string (make-glyph string)))
    (when face
      (set-glyph-face string face))
    (set-extent-begin-glyph overlay string))
   (t
    (viper-overlay-put overlay 'before-string string))))

(defun vimpulse-overlay-after-string (overlay string &optional face)
  "Set the `after-string' property of OVERLAY to STRING.
In XEmacs, change the `end-glyph' property."
  (cond
   ((featurep 'xemacs)
    (setq face (or face (get-text-property 0 'face string)))
    (when (and string (not (glyphp string)))
      (setq string (make-glyph string)))
    (when face
      (set-glyph-face string face))
    (set-extent-end-glyph overlay string))
   (t
    (viper-overlay-put overlay 'after-string string))))

(provide 'vimpulse-utils)

;;;; Keybindings

;;; C-u

(defcustom vimpulse-want-C-u-like-Vim nil
  "Whether C-u scrolls like in Vim, off by default."
  :group 'vimpulse
  :type  'boolean)

(unless vimpulse-want-C-u-like-Vim
  (define-key viper-vi-basic-map "\C-u" 'universal-argument))

;;; vi (command) mode keys

(define-key viper-vi-basic-map "y" 'vimpulse-yank)
(define-key viper-vi-basic-map "d" 'vimpulse-delete)
(define-key viper-vi-basic-map "c" 'vimpulse-change)
(define-key viper-vi-basic-map "\"" 'vimpulse-read-register)
(define-key viper-vi-basic-map "r" 'vimpulse-replace)
(define-key viper-vi-basic-map "J" 'vimpulse-join)
(define-key viper-vi-basic-map "K" 'woman)
(define-key viper-vi-basic-map "=" 'vimpulse-indent)
(define-key viper-vi-basic-map "<" 'vimpulse-shift-left)
(define-key viper-vi-basic-map ">" 'vimpulse-shift-right)
(define-key viper-vi-basic-map "~" 'vimpulse-invert-char)
(define-key viper-vi-basic-map "g" nil) ; delete `viper-nil' binding
(define-key viper-vi-basic-map "gb" 'vimpulse-end-of-previous-word)
(define-key viper-vi-basic-map "gd" 'vimpulse-goto-definition)
(define-key viper-vi-basic-map "gf" 'find-file-at-point)
(define-key viper-vi-basic-map "gg" 'vimpulse-goto-first-line)
(define-key viper-vi-basic-map "gh" 'backward-char)
(define-key viper-vi-basic-map "gj" 'next-line)
(define-key viper-vi-basic-map "gk" 'previous-line)
(define-key viper-vi-basic-map "gl" 'forward-char)
(define-key viper-vi-basic-map "gq" 'vimpulse-fill)
(define-key viper-vi-basic-map "gw" 'vimpulse-fill)
(define-key viper-vi-basic-map "gu" 'vimpulse-downcase)
(define-key viper-vi-basic-map "gU" 'vimpulse-upcase)
(define-key viper-vi-basic-map "g?" 'vimpulse-rot13)
(define-key viper-vi-basic-map "g~" 'vimpulse-invert-case)
(define-key viper-vi-basic-map "zb" 'viper-line-to-bottom)
(define-key viper-vi-basic-map "zh" 'scroll-right)
(define-key viper-vi-basic-map "zl" 'scroll-left)
(define-key viper-vi-basic-map "zt" 'viper-line-to-top)
(define-key viper-vi-basic-map "zz" 'viper-line-to-middle)
(define-key viper-vi-basic-map "*" 'vimpulse-search-forward-for-symbol-at-point)
(define-key viper-vi-basic-map "#" 'vimpulse-search-backward-for-symbol-at-point)
(define-key viper-vi-basic-map "+" 'vimpulse-previous-line-skip-white)
(define-key viper-vi-basic-map "_" 'vimpulse-next-line-skip-white)
(define-key viper-vi-basic-map "\C-]" 'vimpulse-jump-to-tag-at-point)
(define-key viper-vi-basic-map "\C-t" 'pop-tag-mark)

;; Map undo and redo from XEmacs' redo.el
(define-key viper-vi-basic-map "u" 'undo)
(when (fboundp 'redo)
  (define-key viper-vi-basic-map "\C-r" 'redo))

;; Window manipulation
(define-key viper-vi-basic-map "\C-w" (make-sparse-keymap))
(define-key viper-vi-basic-map "\C-w\C-w" 'vimpulse-cycle-windows)
(define-key viper-vi-basic-map "\C-ww" 'vimpulse-cycle-windows)
(define-key viper-vi-basic-map "\C-wo" 'delete-other-windows)
(define-key viper-vi-basic-map "\C-wc" 'delete-window)
(define-key viper-vi-basic-map "\C-ws" 'split-window-vertically)
(define-key viper-vi-basic-map "\C-wv" 'split-window-horizontally)

(when (fboundp 'windmove-left)
  (define-key viper-vi-basic-map "\C-wh" 'windmove-left)
  (define-key viper-vi-basic-map "\C-wj" 'windmove-down)
  (define-key viper-vi-basic-map "\C-wk" 'windmove-up)
  (define-key viper-vi-basic-map "\C-wl" 'windmove-right))

;;; Insert mode keys

;; Vim-like completion keys
(define-key viper-insert-basic-map "\C-p" 'dabbrev-expand)
(define-key viper-insert-basic-map "\C-n" 'vimpulse-abbrev-expand-after)
(define-key viper-insert-basic-map [delete] 'delete-char) ;; delete key
                                        ; make ^[ work
(define-key viper-insert-basic-map (kbd "ESC") 'viper-exit-insert-state)

;; My code (Alessandro)
(defun vimpulse-indent-lines (count)
  (save-excursion
    (dotimes (i count)
      (indent-according-to-mode)
      (forward-line))))

;; His code (Brad)
(defun vimpulse-cycle-windows ()
  "Cycle point to another window."
  (interactive)
  (select-window (next-window)))

;;; r, J, =, >, <

(defun vimpulse-replace (beg end)
  "Replace all selected characters with ARG."
  (interactive (vimpulse-range nil nil nil nil 'forward-char))
  (let ((length (abs (- end beg))))
    (cond
     ((eq 'block vimpulse-this-motion-type)
      (viper-replace-char 1)
      (let ((char (char-after (point)))
            (length (abs (- (save-excursion
                              (goto-char beg)
                              (current-column))
                            (save-excursion
                              (goto-char end)
                              (current-column))))))
        (vimpulse-apply-on-block
         (lambda (beg end)
           (goto-char beg)
           (delete-region beg end)
           (insert (make-string length char)))
         beg end)))
     (t
      (viper-replace-char length)))))

(defun vimpulse-join (beg end)
  "Join the selected lines."
  (interactive (vimpulse-range nil nil t nil 'vimpulse-line))
  (let ((num (count-lines beg end)))
    (unless (< 2 num)
      (setq num 2))
    (viper-join-lines num)))

(defun vimpulse-indent (beg end)
  "Indent text according to mode."
  (interactive (vimpulse-range t nil t))
  (indent-region beg end nil)
  (when viper-auto-indent
    (back-to-indentation)))

(defun vimpulse-shift-left (beg end)
  "Shift all selected lines to the left."
  (interactive (vimpulse-range))
  (let ((nlines (count-lines beg end)))
    (viper-next-line (cons (1- nlines) ?<))))

(defun vimpulse-shift-right (beg end)
  "Shift all selected lines to the right."
  (interactive (vimpulse-range))
  (let ((nlines (count-lines beg end)))
    (viper-next-line (cons (1- nlines) ?>))))

;;; gq, gu, gU

(defun vimpulse-fill (beg end)
  "Fill text."
  (interactive (vimpulse-range t t))
  (setq end (save-excursion
              (goto-char end)
              (skip-chars-backward " ")
              (point)))
  (save-excursion
    (fill-region beg end)))

(defun vimpulse-downcase (beg end)
  "Convert text to lower case."
  (interactive (vimpulse-range))
  (if (eq 'block vimpulse-this-motion-type)
      (vimpulse-apply-on-block 'downcase-region beg end)
    (downcase-region beg end))
  (when (and viper-auto-indent
             (looking-back "^[ \f\t\v]*"))
    (back-to-indentation)))

(defun vimpulse-upcase (beg end)
  "Convert text to upper case."
  (interactive (vimpulse-range))
  (if (eq 'block vimpulse-this-motion-type)
      (vimpulse-apply-on-block 'upcase-region beg end)
    (upcase-region beg end)
    (when (and viper-auto-indent
               (looking-back "^[ \f\t\v]*"))
      (back-to-indentation))))

(defun vimpulse-invert-case (beg end)
  "Convert text to inverted case."
  (interactive (vimpulse-range))
  (let (char)
    (save-excursion
      (cond
       ((eq 'block vimpulse-this-motion-type)
        (let (vimpulse-this-motion-type)
          (vimpulse-apply-on-block 'vimpulse-invert-case beg end)))
       (t
        (goto-char beg)
        (while (< beg end)
          (setq char (following-char))
          (delete-char 1 nil)
          (if (eq char (upcase char))
              (insert-char (downcase char) 1)
            (insert-char (upcase char) 1))
          (setq beg (1+ beg))))))
    (when (and viper-auto-indent
               (looking-back "^[ \f\t\v]*"))
      (back-to-indentation))))

(defun vimpulse-invert-char (beg end)
  "Invert case of character."
  (interactive (vimpulse-range nil nil nil nil 'forward-char))
  (vimpulse-invert-case beg end))

(defun vimpulse-rot13 (beg end)
  "ROT13 encrypt text."
  (interactive (vimpulse-range))
  (rot13-region beg end))

;;; gg

(defun vimpulse-goto-first-line (arg)
  "Go to first line."
  (interactive "P")
  (let ((val (viper-P-val arg))
        (com (viper-getCom arg)))
    (when (eq ?c com) (setq com ?C))
    (viper-move-marker-locally 'viper-com-point (point))
    (viper-deactivate-mark)
    (push-mark nil t)
    (cond
     ((null val)
      (goto-char (point-min)))
     (t
      (goto-line val)))
    (when com
      (viper-execute-com 'vimpulse-goto-line val com))))

;;; +, _

(defun vimpulse-previous-line-skip-white (&optional arg)
  "Go ARG lines backward and to the first non-blank character."
  (interactive "P")
  (let ((val (viper-p-val arg))
        (com (viper-getcom arg)))
    (when com
      (viper-move-marker-locally 'viper-com-point (point)))
    (forward-line (- val))
    (back-to-indentation)
    (when com
      (viper-execute-com 'vimpulse-previous-line-nonblank val com))))

(defun vimpulse-next-line-skip-white (&optional arg)
  "Go ARG lines forward and to the first non-blank character."
  (interactive "P")
  (let ((val (viper-p-val arg))
        (com (viper-getcom arg)))
    (when com
      (viper-move-marker-locally 'viper-com-point (point)))
    (forward-line val)
    (back-to-indentation)
    (when com
      (viper-execute-com 'vimpulse-next-line-nonblank val com))))

;;; *, #

(defun vimpulse-search-string (&optional pos thing backward regexp)
  "Find something to search for near POS or point.
THING is a `thing-at-point', default `symbol'.
BACKWARD, if t, specifies reverse direction.
REGEXP, if t, means the string is `regexp-quote'd.
Returns the empty string if nothing is found."
  (save-excursion
    (setq pos (or pos (point))
          thing (or thing 'symbol))
    (goto-char pos)
    (let ((str (thing-at-point thing)))
      ;; If there's nothing under point, go forwards
      ;; (or backwards) to find it
      (while (and (not str) (or (and backward (not (bobp)))
                                (and (not backward) (not (eobp)))))
        (if backward (backward-char) (forward-char))
        (setq str (thing-at-point 'symbol)))
      (setq str (or str ""))
      ;; No text properties, thank you very much
      (set-text-properties 0 (length str) nil str)
      (when regexp
        (setq str (regexp-quote str)))
      str)))

(defun vimpulse-search-for-symbol (&optional backward pos search)
  "Search forwards or backwards for the symbol under point.
If BACKWARD is t, search in the reverse direction.
SEARCH is a regular expression to use for searching instead of
the symbol under point; it is wrapped in \"\\\\_<\" and \"\\\\_>\".
POS specifies an alternative position to search from. Note that
if POS is specified and at the beginning of a match, that match
is highlighted rather than skipped past."
  (setq search (or search (vimpulse-search-string
                           (point) 'symbol backward t)))
  (cond
   ((string= "" search)
    (error "No string under cursor"))
   (t
    (setq viper-s-string  (concat "\\_<" search "\\_>")
          viper-s-forward (not backward))
    (cond
     (pos
      (unless (region-active-p)
        (push-mark nil t))
      (goto-char pos)
      (cond
       ((looking-at search)
        (save-excursion
          (search-forward search))
        (viper-flash-search-pattern))
       (t
        (viper-search viper-s-string (not backward) 1)
        (unless (region-active-p)
          (pop-mark)))))
     (t
      (viper-search viper-s-string (not backward) 1))))))

(defun vimpulse-search-forward-for-symbol-at-point ()
  (interactive)
  (vimpulse-search-for-symbol))

(defun vimpulse-search-backward-for-symbol-at-point ()
  (interactive)
  (vimpulse-search-for-symbol t))

;;; gb

(defun vimpulse-beginning-of-Word-p ()
  (save-excursion
    (or (bobp)
        (when (viper-looking-at-alpha)
          (backward-char)
          (not (viper-looking-at-alpha))))))

(defun vimpulse-end-of-previous-word (arg)
  "Move point to end of previous word."
  (interactive "P")
  (viper-leave-region-active)
  (let ((val (viper-p-val arg))
        (com (viper-getcom arg)))
    (when com
      (viper-move-marker-locally 'viper-com-point (point)))
    (unless (vimpulse-beginning-of-Word-p)
      (viper-backward-Word 1))
    (viper-backward-Word val)
    (viper-end-of-Word '(1 . ?r))
    (unless com
      (backward-char))
    (when com
      (viper-execute-com 'viper-end-of-word val com))))

;;; gd

(defun vimpulse-goto-definition ()
  "Go to definition or first occurrence of symbol under cursor."
  (interactive)
  (let ((str (vimpulse-search-string (point) 'symbol))
        ientry ipos)
    (cond
     ((string= "" str)
      (error "No string under cursor"))
     ;; If imenu is available, try it
     ((or (featurep 'imenu)
          (load "imenu" t))
      (setq ientry
            (condition-case nil
                (imenu--make-index-alist)
              (error nil)))
      (setq ientry (assoc str ientry))
      (setq ipos (cdr ientry))
      (unless (markerp ipos)
        (setq ipos (cadr ientry)))
      (cond
       ;; imenu found a position, so go there and
       ;; highlight the occurrence
       ((and (markerp ipos)
             (eq (current-buffer) (marker-buffer ipos)))
        (vimpulse-search-for-symbol nil ipos str))
       ;; imenu failed, so just go to first occurrence in buffer
       (t
        (vimpulse-search-for-symbol nil (point-min)))))
     ;; No imenu, so just go to first occurrence in buffer
     (t
      (vimpulse-search-for-symbol nil (point-min))))))

(defun vimpulse-jump-to-tag-at-point ()
  (interactive)
  (let ((tag (thing-at-point 'word)))
    (find-tag tag)))

;;; Auto-indent

(defadvice viper-line (after vimpulse activate)
  "Indent if `viper-auto-indent' is t."
  (and (boundp 'viper-auto-indent) viper-auto-indent
       (eq ?C (cdr arg))
       (indent-according-to-mode)))

;;; C-o, C-i

(viper-deflocalvar vimpulse-mark-list nil
  "List of mark positions to jump to with `vimpulse-jump-forward'.
They are stored as markers, the current position first:

    (car vimpulse-mark-list)  = current position (last popped)
    (cdr vimpulse-mark-list)  = future positions (previously popped)
    (cadr vimpulse-mark-list) = next position (to jump to)

In other words, a sort of \"reverse mark ring\": marks which are
popped off the mark ring, are collected here.")

(defadvice set-mark (after vimpulse activate)
  "Clear `vimpulse-mark-list'."
  (mapc (lambda (marker)
          (set-marker marker nil))
        vimpulse-mark-list)
  (setq vimpulse-mark-list nil))

(defadvice push-mark (after vimpulse activate)
  "Clear `vimpulse-mark-list'."
  (mapc (lambda (marker)
          (set-marker marker nil))
        vimpulse-mark-list)
  (setq vimpulse-mark-list nil))

(defun vimpulse-jump-backward (arg)
  "Go to older position in jump list.
To go the other way, press \\[vimpulse-jump-forward]."
  (interactive "p")
  (let ((current-pos (make-marker)) i)
    (unless vimpulse-mark-list
      (move-marker current-pos (point))
      (add-to-list 'vimpulse-mark-list current-pos))
    (dotimes (arg arg)
      (setq current-pos (make-marker))
      ;; Skip past duplicate entries in the mark ring
      (setq i (length mark-ring))
      (while (progn (move-marker current-pos (point))
                    (let (vimpulse-mark-list)
                      ;; Protect `vimpulse-mark-list'
                      (set-mark-command 0))
                    (setq i (1- i))
                    (and (= (point) current-pos) (< 0 i))))
      ;; Already there?
      (move-marker current-pos (point))
      (unless (= current-pos (car vimpulse-mark-list))
        (setq vimpulse-mark-list
              (cons current-pos vimpulse-mark-list))))))

(defun vimpulse-jump-forward (arg)
  "Go to newer position in jump list.
To go the other way, press \\[vimpulse-jump-backward]."
  (interactive "p")
  (let (current-pos next-pos)
    (dotimes (arg arg)
      (setq current-pos (car vimpulse-mark-list)
            next-pos (cadr vimpulse-mark-list))
      (when next-pos
        ;; Protect `vimpulse-mark-list'
        (let (vimpulse-mark-list)
          (push-mark current-pos t nil))
        (goto-char next-pos)
        (setq vimpulse-mark-list (cdr vimpulse-mark-list))))))

(define-key viper-vi-basic-map "\C-o" 'vimpulse-jump-backward)
(define-key viper-vi-basic-map "\C-i" 'vimpulse-jump-forward)
(unless (key-binding "\C-c\C-o")
  (global-set-key "\C-c\C-o" 'open-line)) ; some may miss this command

;;; Replace backspace

(defcustom vimpulse-backspace-restore t
  "Whether Backspace restores the original text in Replace mode.
On by default."
  :group 'vimpulse
  :type  'boolean)

(viper-deflocalvar vimpulse-replace-alist nil
  "Alist of characters overwritten in Replace mode.
Used by `vimpulse-replace-backspace' to restore text.
The format is (POS . CHAR).")

(defun vimpulse-replace-pre-command ()
  "Remember the character under point."
  (cond
   (viper-replace-minor-mode
    (unless (assq (point) vimpulse-replace-alist)
      (add-to-list 'vimpulse-replace-alist
                   (cons (point) (char-after)))))
   ;; If not in Replace mode, remove itself
   (t
    (remove-hook 'pre-command-hook 'vimpulse-replace-pre-command))))

(add-hook 'viper-replace-state-hook
          (lambda ()
            (setq vimpulse-replace-alist nil)
            (vimpulse-replace-pre-command)
            (add-hook 'pre-command-hook
                      'vimpulse-replace-pre-command)))

(defun vimpulse-replace-backspace ()
  "Restore character under cursor.
If `vimpulse-backspace-restore' is nil,
call `viper-del-backward-char-in-replace' instead."
  (interactive)
  (cond
   (vimpulse-backspace-restore
    (backward-char)
    (let ((oldchar (cdr (assq (point) vimpulse-replace-alist))))
      (when oldchar
        (save-excursion
          (delete-char 1)
          (insert oldchar)))))
   (t
    (viper-del-backward-char-in-replace))))

(defadvice viper-adjust-keys-for (after vimpulse activate)
  "Map <backspace> to `vimpulse-replace-backspace' in Replace mode."
  (define-key viper-replace-map [backspace] 'vimpulse-replace-backspace))

;; Getting dabbrev to search forwards first and then backwards
;; is tricky, because (dabbrev-expand -1) just fails when it
;; doesn't find a following match
(defun vimpulse-abbrev-expand-after ()
  "Expand to the nearest following word.
Search backwards if a match isn't found."
  (interactive)
  ;; Back up global variables
  (let ((abbrev (and (boundp 'dabbrev--last-abbreviation)
                     dabbrev--last-abbreviation))
        (expansion (and (boundp 'dabbrev--last-expansion)
                        dabbrev--last-expansion))
        (location (and (boundp 'dabbrev--last-abbrev-location)
                       dabbrev--last-abbrev-location)))
    ;; Expand in same direction as previously,
    ;; initially forward
    (condition-case nil
        (if (eq this-command last-command)
            (dabbrev-expand nil)
          (setq dabbrev--last-direction -1)
          (dabbrev-expand -1))
      ;; Failure wipes out global variables; restore them
      ;; and search in opposite direction
      (error (progn
               (setq dabbrev--last-abbreviation abbrev
                     dabbrev--last-expansion expansion
                     dabbrev--last-abbred-location location)
               (setq dabbrev--last-direction 1)
               (dabbrev-expand nil) nil)))))

(provide 'vimpulse-misc-keybindings)

;;;; Modal keybinding functions

;; This provides the functions `vimpulse-map', `vimpulse-imap',
;; `vimpulse-vmap' and `vimpulse-omap', which mimic :map, :imap,
;; :vmap and :omap in Vim, as well as `vimpulse-define-key', a
;; general-purpose function for binding keys in a "careful" way.
;;
;; BACKGROUND
;;
;; The :map, :imap, :vmap and :omap commands of Vim let one make two
;; key mappings starting with the same sequence of characters without
;; one overwriting the other. For example:
;;
;;     :imap aa foo
;;     :imap aaa bar
;;
;; When Vim has read "aa" in Insert mode, it will wait for another
;; character to decide whether to insert "foo" or "bar". If the user
;; types "a", "bar" is inserted; if another letter, "foo" plus that
;; letter.
;;
;; Compare with the analogous use of Emacs' `global-set-key' function:
;;
;;     (global-set-key "aa" 'foo)
;;     (global-set-key "aaa" 'bar)
;;
;; Here, the first binding is simply overwritten by the more specific
;; second. The end result is that "aaa" is bound to `bar', while any
;; other sequence starting with "aa" is not bound to anything.
;;
;; The solution is a set of Vim-like or "modal" functions for making
;; new key bindings "on top of" previous bindings. They are
;; `vimpulse-map', `vimpulse-imap', `vimpulse-vmap' and
;; `vimpulse-omap', which mimic Vim's commands, and
;; `vimpulse-define-key', a general function for specifying the
;; keymap. Returning to the example:
;;
;;     (vimpulse-imap "aa" 'foo)
;;     (vimpulse-imap "aaa" 'bar)
;;
;; This will bind "aaa" to `bar', and "aa" + any other key to `foo'.
;; The syntax is the same as that of `global-set-key'. The key
;; sequence may be specified as a string, like above, as a vector
;; (like [?a ?b ?c]), or as a call to `kbd' (like (kbd "a b c")).
;;
;; To make a binding in vi (command) mode, use `vimpulse-map';
;; in Insert mode, `vimpulse-imap'; in Visual mode, `vimpulse-vmap';
;; in Operator-Pending mode, `vimpulse-omap'. The more general
;; `vimpulse-define-key' function lets one specify the keymap to store
;; the binding in, as when using `define-key':
;;
;;     (vimpulse-define-key keymap "abc" 'command)
;;
;; IMPLEMENTATION
;;
;; The code depends on a little-known GNU Emacs feature called
;; "default key bindings". A default key binding is a binding ending
;; with the Lisp symbol t, which roughly stands for "any other key".
;; Default bindings allow a keymap to bind all possibilities without
;; having to enumerate them. For example, we may bind the sequence
;; "AB" + any key as such:
;;
;;     (global-set-key (kbd "A B <t>") 'foo)
;;
;; This means that "ABA" will execute `foo', as will "ABB", "ABC",
;; and so on. For more on default key bindings, see the GNU Emacs
;; Lisp Reference Manual, chapter 22.3: "Format of Keymaps".
;;
;; What is done by functions like `vimpulse-define-key' and
;; `vimpulse-map' (which depends on the former) is to generate these
;; default bindings automatically. If "AB" is already bound to `foo'
;; and we modally bind "ABC" to `bar', the old binding is first
;; replaced by a default binding, as if we issued the following:
;;
;;     (global-set-key (kbd "A B") nil) ; delete old binding
;;     (global-set-key (kbd "A B <t>") 'foo)
;;     (global-set-key (kbd "A B C") 'bar)
;;
;; Then, "ABC" runs `bar', while "AB" + any other key than C
;; runs `foo'.
;;
;; This almost gets us where we want with regard to Vimpulse, but not
;; quite. The problem is that quite a few commands must necessarily
;; read and parse keyboard input to decide what to do. For instance,
;; Viper binds "d" to the general command `viper-command-argument',
;; which, depending on the next key-presses, deletes a line, two
;; words, or any motion entered by the user. What happens if we decide
;; to modally bind, say, "dq" to a custom command `foo' of our own?
;;
;;     (global-set-key (kbd "d") nil) ; delete old binding
;;     (global-set-key (kbd "d <t>") 'viper-command-argument)
;;     (global-set-key (kbd "d q") 'foo)
;;
;; Now, if the user enters "dq", `foo' is called. But when the user
;; enters "dw" to delete a word, `viper-command-argument' is called
;; only after the "w" is entered. This destroys the logic of the
;; command, which depends on "d" being the last key-press (stored in
;; `last-command-event') before "w" is read through `read-char'. It
;; obviously won't work as intended with a single "w" missing a
;; preceding "d", which is what it sees.
;;
;; So, we need to find a way to pass "d" and "w" along in the proper
;; manner; that is, to make the default binding appear the same as the
;; old binding it replaces. This is done by `vimpulse-modal-pre-hook',
;; which unreads "w" (so it can be read again) and changes
;; `last-command-event' to "d". Of course, this behavior is only
;; needed for default key bindings, and only for default key bindings
;; made by the modal binding functions. To that end, every time
;; `vimpulse-define-key' makes a default binding, the binding is
;; listed in `vimpulse-modal-alist' for future reference. Checking
;; against the list, `vimpulse-modal-pre-hook' only does its thing if
;; the current binding comes back positive.
;;
;; XEmacs is somewhat fuzzy about its command loop variables, not
;; allowing direct modification of `last-command-event'. However,
;; shadowing it with a `let' binding is possible, and a wrap-around
;; advice of the current command is employed to accomplish this. Also,
;; XEmacs does not have default key bindings in quite the same way as
;; GNU Emacs; `vimpulse-default-binding' takes care of the differences.
;;
;; LIMITATIONS
;;
;; Vim has a `timeout' option which lets one specify the time in
;; milliseconds that is waited for a key code or mapped key sequence
;; to complete. Emacs, on the other hand, will wait indefinitely. This
;; behavior is probably not implementable.

;;; Variables

;; This deals with default key bindings
(defvar vimpulse-last-command-event nil
  "Value for overwriting `last-command-event'.
Used by `vimpulse-modal-pre-hook'.")

(defvar vimpulse-modal-alist nil
  "Key bindings for which `vimpulse-modal-pre-hook' is active.
That is, `last-command-event' and `read-char' work differently
for these bindings. The format is (KEY-VECTOR . COMMAND).")

;;; Advice

;; For XEmacs, construct a wrap-around advice of the current command
;; shadowing the read-only command loop variables with a
;; `let' binding
(defmacro vimpulse-advice-command (command)
  "Make wrap-around advice for shadowing `last-command-event'.
XEmacs does not allow us to change its command loop variables
directly, but shadowing them with a `let' binding works."
  `(defadvice ,command (around vimpulse-modal activate)
     "Shadow `last-command-event' with a `let' binding."
     (cond
      (vimpulse-last-command-event
       (let* ((last-command-event
               (character-to-event vimpulse-last-command-event))
              (last-command-char vimpulse-last-command-event)
              (last-input-event last-command-event)
              (last-input-char last-command-char))
         ad-do-it))
      (t
       ad-do-it))))

;;; General functions

;; These deal with key sequences
(defun vimpulse-strip-prefix (key-sequence)
  "Strip any prefix argument keypresses from KEY-SEQUENCE.
This is useful for deriving a \"standard\" key-sequence from
`this-command-keys', to be looked up in `vimpulse-modal-alist'."
  (let* ((offset 0)
         (temp-sequence (vconcat key-sequence))
         (key (aref temp-sequence offset))
         (length (length temp-sequence)))
    ;; If XEmacs, get rid of the event object type
    (and (featurep 'xemacs) (eventp key)
         (setq key (event-to-character key nil t)))
    ;; Any keys bound to `universal-argument', `digit-argument' or
    ;; `negative-argument' or bound in `universal-argument-map'
    ;; are considered prefix keys.
    (while (and (or (memq (key-binding (vector key) t)
                          '(universal-argument
                            digit-argument
                            negative-argument))
                    (lookup-key universal-argument-map
                                (vector key)))
                (setq offset (1+ offset))
                (< offset length))
      (setq key (aref temp-sequence offset))
      (and (featurep 'xemacs) (eventp key)
           (setq key (event-to-character key nil t))))
    (vimpulse-truncate temp-sequence length offset)))

(defun vimpulse-modal-check (key-sequence)
  "Return t if KEY-SEQUENCE defaults to `this-command',
but only for bindings listed in `vimpulse-modal-alist'."
  (let ((temp-sequence (vimpulse-strip-prefix key-sequence)))
    (setq temp-sequence (vimpulse-truncate temp-sequence -1))
    (and this-command ; may be nil
         (not (key-binding key-sequence)) ; only default bindings
         (eq this-command
             (cdr (assoc temp-sequence vimpulse-modal-alist))))))

(defun vimpulse-modal-remove (key-vector &optional recursive)
  "Delete entry with KEY-VECTOR from `vimpulse-modal-alist'.
If RECURSIVE is non-nil, also delete entries whose key-vectors
start with KEY-VECTOR."
  (if recursive
      (dolist (entry vimpulse-modal-alist)
        (when (equal key-vector
                     (vimpulse-truncate (car entry)
                                        (length key-vector)))
          (setq vimpulse-modal-alist
                (delq entry vimpulse-modal-alist))))
    (assq-delete-all key-vector vimpulse-modal-alist)))

(defun vimpulse-xemacs-def-binding
  (keymap key def &optional modal-binding define-func)
  "Make a default binding in XEmacs. If MODAL-BINDING is
non-nil, advice DEF by means of `vimpulse-advice-command'."
  (let ((temp-sequence (vconcat key))
        (submap (lookup-key keymap key)))
    (unless define-func (setq define-func 'define-key))
    (and modal-binding (commandp def)
         (eval `(vimpulse-advice-command ,def)))
    (and (< 1 (length temp-sequence))
         (eq t (aref temp-sequence (1- (length temp-sequence))))
         (setq temp-sequence (vimpulse-truncate temp-sequence -1)))
    ;; The following is from
    ;; http://tracker.xemacs.org/XEmacs/its/msg2021
    (unless (keymapp submap)
      (setq submap (make-sparse-keymap)))
    (set-keymap-default-binding submap def)
    (funcall define-func keymap temp-sequence submap)))

(defun vimpulse-default-binding
  (keymap key def &optional modal-binding define-func)
  "Make a default binding in GNU Emacs or XEmacs,
whichever is appropriate. If MODAL-BINDING is non-nil,
the binding is listed in `vimpulse-modal-alist'."
  (let ((temp-sequence (vconcat key)))
    (unless define-func (setq define-func 'define-key))
    (cond
     ((featurep 'xemacs)
      (vimpulse-xemacs-def-binding
       keymap temp-sequence def modal-binding define-func))
     (t
      (unless (eq t (aref temp-sequence (1- (length temp-sequence))))
        (setq temp-sequence (vconcat temp-sequence [t])))
      (funcall define-func keymap temp-sequence def)))
    (when modal-binding
      (add-to-list 'vimpulse-modal-alist
                   (cons (vimpulse-truncate temp-sequence -1) def)))))

;;; Hook run before each command

;; If the current command is a default key binding made by the modal
;; binding functions, we need to unread the last input events and
;; change some command loop variables to give the command the
;; impression of its "old" binding
(defun vimpulse-modal-pre-hook ()
  "Update `vimpulse-last-command-event' and `unread-command-events'.
If the current key-sequence defaults to a shorter key-sequence,
the difference is stored in these two variables, to be passed on
via the `last-command-event' variable and the `read-char'
functions, respectively."
  (setq vimpulse-last-command-event nil)
  (let ((key-sequence (vconcat (this-command-keys))))
    ;; If XEmacs, get rid of the event object type
    (when (featurep 'xemacs)
      (setq key-sequence (events-to-keys key-sequence)))
    (while (and (< 1 (length key-sequence))
                (vimpulse-modal-check key-sequence))
      ;; Unread last event
      (setq vimpulse-last-command-event
            (elt key-sequence (1- (length key-sequence))))
      (when (featurep 'xemacs)
        (setq vimpulse-last-command-event
              (character-to-event vimpulse-last-command-event)))
      (add-to-list 'unread-command-events vimpulse-last-command-event)
      ;; Change command loop variables
      (setq vimpulse-last-command-event
            (elt key-sequence (1- (1- (length key-sequence)))))
      (unless (featurep 'xemacs) ; if XEmacs, do this with advice
        (setq last-command-event vimpulse-last-command-event)
        (setq last-command-char  vimpulse-last-command-event)
        (setq last-input-event   vimpulse-last-command-event)
        (setq last-input-char    vimpulse-last-command-event))
      (setq key-sequence
            (vimpulse-truncate key-sequence -1)))))

;;; hook run after each command

;; This merely ensures `vimpulse-last-command-event' is reset
(defun vimpulse-modal-post-hook ()
  "Erase `vimpulse-last-command-event'."
  (setq vimpulse-last-command-event nil))

(add-hook 'pre-command-hook  'vimpulse-modal-pre-hook)
(add-hook 'post-command-hook 'vimpulse-modal-post-hook)

;;; Modal binding functions

;; `vimpulse-define-key' is general; `vimpulse-map', `vimpulse-imap'
;; and `vimpulse-vmap' imitate Vim's :map, :imap and :vmap,
;; respectively.
(defun vimpulse-define-key
  (keymap key def &optional dont-list define-func)
  "Carefully bind KEY to DEF in KEYMAP.
\"Carefully\" means that if a subset of the key sequence is already
bound, a default binding is made so that the new binding won't
overwrite the old. E.g., if we want to carefully bind \"A B C\" to
`foo', and \"A B\" is already bound to `bar', the end result is

    \"A B C\"   => `foo'
    \"A B <t>\" => `bar'

which means that \"A B D\", for example, defaults to `bar'. (For
more on default bindings, see `define-key'.) The default binding
gets listed in `vimpulse-modal-alist', so that, with regard to
command loop variables, it appears exactly the same as the
binding it replaced. To override this, use DONT-LIST.
DEFINE-FUNC specifies a function to be used in place of
`define-key'.

To remove a binding, bind it to nil.

NOTE: If the original binding \"A B\" is not stored in KEYMAP,
but in some other map which is active only in a certain
state (say, Insert mode), this function can detect that binding
only if called in the same state. The functions `vimpulse-map',
`vimpulse-imap' and `vimpulse-vmap' take care of this."
  (let (key-vector temp-sequence current-binding previous-binding)
    ;; For each subset of KEY-VECTOR (stored in `temp-sequence'), check
    ;; the binding (stored in `current-binding'); if it isn't bound,
    ;; use `previous-binding'.
    (setq define-func (or define-func 'define-key))
    (setq key-vector key)
    (when (stringp key-vector)
      (condition-case nil
          (setq key-vector (kbd key-vector))
        (error nil)))
    (setq key-vector (vconcat key-vector))
    (cond
     ;; nil unbinds the key-sequence
     ((not def)
      (funcall define-func keymap key-vector def)
      (while (and (< 1 (length key-vector))
                  (not (lookup-key keymap key-vector)))
        (vimpulse-modal-remove key-vector t)
        (setq key-vector (vimpulse-truncate key-vector -1))))
     ;; undefined also unbinds, but less forcefully
     ((eq 'undefined def)
      (if (keymapp (lookup-key keymap key-vector))
          (vimpulse-default-binding keymap key-vector nil t define-func)
        (funcall define-func keymap key-vector def))
      (vimpulse-modal-remove key-vector))
     ;; Regular binding: convert previous bindings to default bindings
     (t
      (dotimes (i (1- (length key-vector)))
        (setq temp-sequence (vimpulse-truncate key-vector (1+ i)))
        (setq current-binding (lookup-key keymap temp-sequence t))
        (when (or (numberp current-binding) (not current-binding))
          (setq current-binding
                (or (key-binding temp-sequence t) previous-binding)))
        (setq previous-binding current-binding)
        ;; If `current-binding' is a keymap, do nothing, since our modal
        ;; binding can exist happily as part of that keymap. However, if
        ;; `current-binding' is a command, we need to make room for the
        ;; modal binding by creating a default binding.
        (unless (keymapp current-binding)
          (setq temp-sequence (vconcat temp-sequence [t]))
          (setq current-binding (lookup-key keymap temp-sequence t))
          (when (or (numberp current-binding) (not current-binding))
            (setq current-binding
                  (or (key-binding temp-sequence t) previous-binding))
            (define-key keymap
              (vimpulse-truncate temp-sequence -1) nil)
            (vimpulse-default-binding
             keymap temp-sequence current-binding
             (not dont-list) define-func))
          (setq previous-binding current-binding)))
      ;; Defaults are taken care of; we may now bind the key.
      ;; If a longer binding starting with KEY-VECTOR exists,
      ;; make a default binding so it's not overwritten.
      (if (keymapp (lookup-key keymap key-vector))
          (vimpulse-default-binding
           keymap key-vector def (not dont-list) define-func)
        (funcall define-func keymap key def))))))

(defvar vimpulse-modal-map (make-sparse-keymap)
  "Keymap of bindings overwritten by `vimpulse-map' et al.")

(define-minor-mode vimpulse-modal-minor-mode
  "Minor mode of bindings overwritten by `vimpulse-map' et al."
  :keymap vimpulse-modal-map
  (dolist (entry vimpulse-modal-alist)
    (unless (lookup-key vimpulse-modal-map (car entry))
      (define-key vimpulse-modal-map (car entry) (cdr entry))))
  (when vimpulse-modal-minor-mode
    (viper-normalize-minor-mode-map-alist)))

(add-to-list 'vimpulse-state-maps-alist
             (cons 'vimpulse-modal-minor-mode 'vimpulse-modal-map))

(defun vimpulse-map-state (state key def &optional modes)
  "Modally bind KEY to DEF in STATE.
Don't use this function directly; see `vimpulse-map',
`vimpulse-imap', `vimpulse-vmap' and `vimpulse-omap' instead."
  (let* ((old-state viper-current-state)
         (map (cdr (assq state vimpulse-state-vars-alist)))
         (basic-map (eval (cdr (assq 'basic-map map))))
         (global-user-map (eval (cdr (assq 'global-user-map map)))))
    (viper-set-mode-vars-for state)
    (let ((viper-current-state state))
      (viper-normalize-minor-mode-map-alist))
    (cond
     (modes
      (dolist (mode modes)
        (if (eq t mode)
            (vimpulse-define-key global-user-map key def)
          (setq map (vimpulse-modifier-map state mode))
          (vimpulse-define-key map key def)
          (viper-modify-major-mode mode state map))))
     (t
      (vimpulse-define-key basic-map key def)))
    (viper-set-mode-vars-for old-state)
    (viper-normalize-minor-mode-map-alist)))

(defun vimpulse-map-state-local (state key def)
  "Make a buffer-local binding for KEY and DEF in STATE.
Don't use this function directly; see `vimpulse-map-local',
`vimpulse-imap-local' and `vimpulse-vmap-local' instead."
  (viper-add-local-keys state `((,key . ,def))))

(defun vimpulse-map (key def &rest modes)
  "Modally bind KEY to DEF in vi (command) state.
The syntax is the same as that of `global-set-key', e.g.,

    (vimpulse-map \"abc\" 'abc-command)

The optional MODES argument specifies which major modes the
binding is seen in:

    (vimpulse-map \"abc\" 'abc-command 'lisp-mode 'text-mode)

Otherwise, the binding is universal, but has lower priority.
Pass t to MODES to create an universal binding with presedence
over mode-specific bindings."
  (vimpulse-map-state 'vi-state key def modes))

(defun vimpulse-imap (key def &rest modes)
  "Modally bind KEY to DEF in Insert state.
The syntax is the same as that of `global-set-key', e.g.,

    (vimpulse-imap \"abc\" 'abc-command)

The optional MODES argument specifies which major modes the
binding is seen in:

    (vimpulse-imap \"abc\" 'abc-command 'lisp-mode 'text-mode)

Otherwise, the binding is universal, but has lower priority.
Pass t to MODES to create an universal binding with presedence
over mode-specific bindings."
  (vimpulse-map-state 'insert-state key def modes))

(defun vimpulse-vmap (key def &rest modes)
  "Modally bind KEY to DEF in the Visual state.
The syntax is the same as that of `global-set-key', e.g.,

    (vimpulse-vmap \"abc\" 'abc-command)

The optional MODES argument specifies which major modes the
binding is seen in:

    (vimpulse-vmap \"abc\" 'abc-command 'lisp-mode 'text-mode)

Otherwise, the binding is universal, but has lower priority.
Pass t to MODES to create an universal binding with presedence
over mode-specific bindings."
  (vimpulse-map-state 'visual-state key def modes))

(defun vimpulse-omap (key def &rest modes)
  "Modally bind KEY to DEF in the Operator-Pending state.
The syntax is the same as that of `global-set-key', e.g.,

    (vimpulse-omap \"abc\" 'abc-command)

The optional MODES argument specifies which major modes the
binding is seen in:

    (vimpulse-omap \"abc\" 'abc-command 'lisp-mode 'text-mode)

Otherwise, the binding is universal, but has lower priority.
Pass t to MODES to create an universal binding with presedence
over mode-specific bindings."
  (vimpulse-map-state 'operator-state key def modes))

(defun vimpulse-map! (key def &rest modes)
  "Bind KEY to DEF in vi (command) state and the Visual state.
To bind in Insert state, use `vimpulse-imap'."
  (vimpulse-map key def modes)
  (vimpulse-vmap key def modes))

(defun vimpulse-map-local (key def)
  "Make a buffer-local binding of KEY to DEF in vi (command) state.
The syntax is the same as that of `local-set-key', e.g.,

    (vimpulse-map-local \"abc\" 'abc-command)

You would typically use this in a mode hook. To make a global
binding, use `vimpulse-map'."
  (vimpulse-map-state-local 'vi-state key def))

(defun vimpulse-imap-local (key def)
  "Make a buffer-local binding of KEY to DEF in Insert state.
The syntax is the same as that of `local-set-key', e.g.,

    (vimpulse-imap-local \"abc\" 'abc-command)

You would typically use this in a mode hook. To make a global
binding, use `vimpulse-imap'."
  (vimpulse-map-state-local 'insert-state key def))

(defun vimpulse-vmap-local (key def)
  "Make a buffer-local binding of KEY to DEF in the Visual state.
The syntax is the same as that of `local-set-key', e.g.,

    (vimpulse-vmap-local \"abc\" 'abc-command)

You would typically use this in a mode hook. To make a global
binding, use `vimpulse-vmap'."
  (vimpulse-map-state-local 'visual-state key def))

(defun vimpulse-omap-local (key def)
  "Make a buffer-local binding of KEY to DEF in the Operator-Pending state.
The syntax is the same as that of `local-set-key', e.g.,

    (vimpulse-omap-local \"abc\" 'abc-command)

You would typically use this in a mode hook. To make a global
binding, use `vimpulse-omap'."
  (vimpulse-map-state-local 'visual-state key def))

(provide 'vimpulse-modal)

;;;; Ex commands

;; All this code is taken from Brad Beveridge's extended viper
(defvar vimpulse-extra-ex-commands
  '(("b" "buffer")
    ("bdelete" (vimpulse-kill-current-buffer))
    ("bnext" "next")
    ("clo" "close")
    ("close" (delete-window))
    ("on" "only")
    ("only" (delete-other-windows))
    ("quit" (save-buffers-kill-emacs))
    ("split" (split-window))
    ("syntax" (global-font-lock-mode))
    ;; Emacs and Vim use inverted naming conventions for splits
    ("vsplit" (split-window-horizontally)))
  "Extra Ex commands, added to `ex-token-alist' when Vimpulse loads.")

(defun vimpulse-kill-current-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer nil))

;; Additional Ex mode features: `ex-token-alist' is defined as a
;; constant, but it appears I can safely push values to it!
(dolist (entry vimpulse-extra-ex-commands)
  (setq ex-token-alist
        (delete (assoc (car entry) ex-token-alist) ex-token-alist))
  (add-to-list 'ex-token-alist entry t))

(provide 'vimpulse-ex)

;;;; Paren matching

;; When highlighting matching parentheses, Emacs matches the closing
;; parenthesis before the cursor, instead of under it (like in Vim).
;; This code provides an alternate parenthesis matching function
;; used when Viper is in vi (command) mode, so that the parenthesis
;; under the cursor is matched. This makes it possible to visually
;; inspect a closing parenthesis at the end of the line.
;;
;; In Insert mode, Emacs' scheme is deemed best and kept as is.
;;
;; Custom paren-matching LOADED BY DEFAULT.
;; To avoid loading it, set `vimpulse-enhanced-paren-matching' to nil
;; in your .emacs before loading Vimpulse.

;; Do we really need this option?
(defcustom vimpulse-enhanced-paren-matching t
  "Enhanced matching of parentheses, on by default."
  :group 'vimpulse
  :type  'boolean)

;; Load and enable paren.el if available
(unless (featurep 'paren)
  (condition-case nil
      (require 'paren)
    (error nil)))
(and (fboundp 'show-paren-mode)
     (not (vimpulse-custom-value-p 'show-paren-mode))
     ;; Fast paren-matching
     (vimpulse-setq show-paren-delay 0)
     (show-paren-mode 1))

(defvar vimpulse-paren-overlay-open nil
  "Overlay used to highlight the opening paren.")

(defvar vimpulse-paren-overlay-close nil
  "Overlay used to highlight the closing paren.")

(defun vimpulse-paren-open-p (&optional pos)
  "Return t if the character at point (or POS) is an opening paren."
  (setq pos (or pos (point)))
  (let ((class (syntax-after pos)))
    (when class
      (setq class (syntax-class class))
      (= 4 class))))

(defun vimpulse-paren-close-p (&optional pos)
  "Return t if the character at point (or POS) is an closing paren."
  (setq pos (or pos (point)))
  (let ((class (syntax-after pos)))
    (when class
      (setq class (syntax-class class))
      (= 5 class))))

(defun vimpulse-paren-match (&optional pos)
  "Return the position of possible matching paren at point (or POS).
If not a paren, return `not-a-paren'. If not found, return nil."
  (setq pos (or pos (point)))
  (condition-case nil
      (cond
       ((vimpulse-paren-open-p pos)
        (1- (scan-sexps pos 1)))
       ((vimpulse-paren-close-p pos)
        (scan-sexps (1+ pos) -1))
       (t
        'not-a-paren))
    (error nil)))

(defun vimpulse-paren-match-p (pos1 pos2)
  "Return t if POS1 and POS2 are matching characters.
Checks the characters at position POS1 and POS2 and returns t
if they are matching characters (in a paren-match meaning),
nil otherwise."
  (let ((class1 (car (syntax-after pos1)))
        (match1 (cdr (syntax-after pos1)))
        (class2 (car (syntax-after pos2)))
        (match2 (cdr (syntax-after pos2))))
    (or (eq match1 (char-after pos2))
        (eq match2 (char-after pos1))
        (eq match1 match2))))

(defun vimpulse-paren-highlight (face &optional pos)
  "Highlight the paren at point (or POS) with FACE."
  (setq pos (or pos (point)))
  (let ((ovl (if (vimpulse-paren-open-p pos)
                 vimpulse-paren-overlay-open
               vimpulse-paren-overlay-close)))
    (viper-overlay-put ovl 'face face)
    (viper-move-overlay ovl pos (1+ pos))))

;; FIXME: this description sucks
(defun vimpulse-paren-highlight-pair (&optional pos)
  "Highlight paren pair.
Highlights the paren at point (or POS) and eventual matching
or mismatched paren."
  (setq pos (or pos (point)))
  (let ((match (vimpulse-paren-match pos)))
    (cond
     ((not match)
      (vimpulse-paren-highlight 'show-paren-mismatch pos))
     ((eq match 'not-a-paren)
      (vimpulse-delete-overlay vimpulse-paren-overlay-open)
      (vimpulse-delete-overlay vimpulse-paren-overlay-close))
     ((/= pos (vimpulse-paren-match match))
      (vimpulse-paren-highlight 'show-paren-mismatch pos))
     ((vimpulse-paren-match-p pos match)
      (vimpulse-paren-highlight 'show-paren-match pos)
      (vimpulse-paren-highlight 'show-paren-match match))
     (t
      (vimpulse-paren-highlight 'show-paren-mismatch pos)
      (vimpulse-paren-highlight 'show-paren-mismatch match)))))

(defadvice show-paren-function (around vimpulse-paren activate)
  "Use custom highlighting if `vimpulse-enhanced-paren-matching' is t."
  ;; Define overlays if they don't exist
  (cond
   (vimpulse-enhanced-paren-matching
    (unless (viper-overlay-live-p vimpulse-paren-overlay-open)
      (setq vimpulse-paren-overlay-open
            (viper-make-overlay (point) (point) nil t nil)
            vimpulse-paren-overlay-close
            (viper-make-overlay (point) (point) nil t nil))
      (vimpulse-delete-overlay vimpulse-paren-overlay-open)
      (vimpulse-delete-overlay vimpulse-paren-overlay-close))
    (cond
     ;; Viper not in Insert, Replace or Emacs state
     ((and (not (eq viper-current-state 'insert-state))
           (not (eq viper-current-state 'replace-state))
           (not (eq viper-current-state 'emacs-state))
           show-paren-mode viper-mode)
      ;; Safely delete the overlays used by `show-paren-function'
      ;; and call our custom function instead
      (and (viper-overlay-live-p show-paren-overlay)
           (vimpulse-delete-overlay show-paren-overlay))
      (and (viper-overlay-live-p show-paren-overlay-1)
           (vimpulse-delete-overlay show-paren-overlay-1))
      (vimpulse-paren-highlight-pair))
     ;; Viper in Insert mode
     (t
      ;; Delete the overlays used by our custom function
      (vimpulse-delete-overlay vimpulse-paren-overlay-open)
      (vimpulse-delete-overlay vimpulse-paren-overlay-close)
      ad-do-it)))
   (t
    ad-do-it)))

(provide 'vimpulse-paren-matching)

;;;; Operator-Pending mode

;; This provides a framework for combining "motions" and "operators".
;; A motion is any command moving point. An operator is a command
;; acting on the text moved over by a motion.
;;
;; Defining operator commands is similar to defining commands acting
;; on the region. That is, both must have two arguments, BEG and END,
;; and an `interactive' specification that stores the relevant range
;; in those arguments:
;;
;;     (defun foo-region (beg end)
;;       (interactive "r")
;;       ;; Do stuff from BEG to END
;;       )
;;
;;     (defun foo-operator (beg end)
;;       (interactive (vimpulse-range))
;;       ;; Do stuff from BEG to END
;;       )
;;
;; If you like, you can convert any region command to an operator
;; with `vimpulse-convert-to-operator'.
;;
;; When the latter command above is run, `vimpulse-range' will query
;; the user for a motion and determine the resulting range to pass on
;; to the command's arguments. (In Visual mode, however, it skips the
;; querying and returns the selection boundaries instead.)
;;
;; While a motion is read from the keyboard, a temporary Viper state,
;; Operator-Pending mode, is entered. This state inherits bindings
;; from the regular vi state, but it may also define its own, for
;; instance text objects. Text objects are like motions, but define a
;; starting point as well as an ending point. They are implemented
;; simply as selection commands.
;;
;; As in Vim, a motion may specify a motion type, such as `line'.
;; The following motion types are defined:
;;
;;   * `line': the motion range is extended to whole lines.
;;   * `inclusive': the ending character is included.
;;   * `exclusive' (default): the ending character is excluded.
;;
;; For example, (put 'foo 'motion-type 'line) gives `foo' a type of
;; `line'. If unspecified, the motion is considered `exclusive'.
;; You can override the type with v, V and C-v: for example,
;; dvj will delete an exclusive range rather than a linewise.
;;
;; The benefit of a dedicated state when an "operator" is "pending" is
;; code separation. In the original scheme, every Viper motion must
;; manually do the work of deleting/changing/yanking the text moved
;; over, making that action repeatable, etc. The new framework handles
;; everything automatically and orthogonally, enabling the use of
;; plain Emacs movement commands (like S-exp navigation) as motions.
;;
;; A smattering of compatibility macros ensure that certain Viper
;; motions are repeated correctly. In the long run, Viper's motions
;; should be rewritten; I'll have to contact Michael Kifer and hear
;; what he thinks about this. For what it's worth, the following code
;; addresses "TODO item #1" in viper.el.

(vimpulse-define-state operator
  "Operator-pending mode is when an operator is pending,
awaiting a motion (after \"d\", \"y\", \"c\", etc.)."
  :id "<OP> "
  :hook '(vimpulse-set-operator-cursor-type)
  :enable '(vimpulse-operator-remap-minor-mode
            (viper-vi-kbd-minor-mode nil)
            vi-state vimpulse-modal-minor-mode)
  (cond
   ((eq 'operator-state viper-current-state)
    (vimpulse-modal-minor-mode 1))
   (t
    (vimpulse-modal-minor-mode -1))))

;; This is a short-lived state, only used for calculating
;; motion ranges. If anything goes wrong and we enter the
;; command loop, exit to vi state immediately.
(defun vimpulse-operator-exit-hook ()
  "Exit Operator-Pending mode."
  (when (eq 'operator-state viper-current-state)
    (viper-change-state-to-vi)))

(add-hook 'pre-command-hook 'vimpulse-operator-exit-hook)
(add-hook 'post-command-hook 'vimpulse-operator-exit-hook)

;; We place all remap bindings in a keymap of their own.
;; This enables Visual mode only to inherit text object
;; bindings from Operator-Pending mode, not any remapping.
(defvar vimpulse-operator-remap-map (make-sparse-keymap))

(defvar vimpulse-operator-remap-alist nil
  "Association list of command remappings in Operator-Pending mode.")

(define-minor-mode vimpulse-operator-remap-minor-mode
  "Minor mode of bindings overwritten by `vimpulse-map' et al."
  :keymap vimpulse-operator-remap-map)

(put 'vimpulse-operator-remap-map
     'remap-alist 'vimpulse-operator-remap-alist)

;; Command loop variables
(defvar vimpulse-this-operator nil
  "Current operator.
In general, motions and operators are orthogonal, with some exceptions:
\"cw\" and \"dw\" work on slightly different ranges, for example.
Motions can check this variable if they need to know what
operator receives their range. See also `vimpulse-this-motion'.")

(defvar vimpulse-this-motion nil
  "Current motion.
In general, motions and operators are orthogonal, with some exceptions:
\"cc\" may indent the current line while \"cw\" may not, for example.
Operators may check this variable if they need to know what
motion produced the current range. See also `vimpulse-this-operator'.")

;; The last motion count is stored in `viper-d-com'
(defvar vimpulse-this-count nil
  "Current count (operator count times motion count).")

(defvar vimpulse-this-motion-type nil
  "Current motion type.
May be `block', `line', `inclusive', `exclusive' or nil.")

(defvar vimpulse-last-motion-type nil
  "Last repeated range type.
May be `block', `line', `inclusive', `exclusive' or nil.")

(defvar vimpulse-last-operator nil
  "Last repeated operator.
Used by `vimpulse-operator-repeat'.")

(defvar vimpulse-last-motion nil
  "Last repeated motion.
Used by `vimpulse-operator-repeat'.")

(defcustom vimpulse-want-operator-pending-cursor t
  "Whether the cursor changes in Operator-Pending mode, on by default."
  :group 'vimpulse
  :type  'boolean)

(when (featurep 'xemacs)
  ;; XEmacs shows the tag before the modes, so truncate it to a
  ;; constant length to avoid excessive flickering
  (setq vimpulse-operator-state-id "<OP>") ; 4 characters
  ;; XEmacs lacks a horizontal bar cursor option
  (setq vimpulse-want-operator-pending-cursor nil))

(defun vimpulse-set-operator-cursor-type ()
  "Change cursor appearance in Operator-Pending mode."
  (when vimpulse-want-operator-pending-cursor
    (vimpulse-half-height-cursor)))

(defun vimpulse-half-height-cursor ()
  "Change cursor to a half-height box.
\(This is really just a thick horizontal bar.)"
  (unless (featurep 'xemacs)
    (condition-case nil
        (let (height)
          (redisplay)
          (setq height (window-line-height))
          (setq height (+ (nth 0 height) (nth 3 height)))
          ;; Cut cursor height in half
          (setq height (/ height 2))
          (setq cursor-type (cons 'hbar height))
          ;; Ensure the cursor is redisplayed
          (force-window-update (selected-window))
          (redisplay))
      (error nil))))

(defun vimpulse-range
  (&optional no-repeat dont-move-point whole-lines keep-visual
             custom-motion)
  "Read a motion and return a range (BEG END).
In Visual mode, returns the beginning and end of the selection.
This can be used in the `interactive' form of a command:

    (defun foo (beg end)
      (interactive (vimpulse-range))
      ;; Do foo from BEG to END
      )

When this command is called interactively, a motion is read from
the keyboard and the resulting range is stored in BEG and END.
The command then proceeds to do whatever it wants to do on the
text between those buffer positions. The optional arguments allow
for some customization:

NO-REPEAT: don't let \\[viper-repeat] repeat the command.
DONT-MOVE-POINT: don't move to beginning of range in vi state.
WHOLE-LINES: extend range to whole lines.
KEEP-VISUAL: don't disable Visual selection.
CUSTOM-MOTION: predefined motion to use in vi state.

If CUSTOM-MOTION is specified, the command will not read a motion
from the keyboard. This has no effect on Visual behavior."
  (let ((range (list (point) (point)))
        (type-alist '((vimpulse-visual-toggle-normal . inclusive)
                      (vimpulse-visual-toggle-line . line)
                      (vimpulse-visual-toggle-block . block)))
        (type (when whole-lines 'line))
        (oldmsg (current-message))
        viper-ESC-moves-cursor-back)
    (setq vimpulse-this-motion-type nil
          vimpulse-this-count nil
          vimpulse-this-motion nil
          vimpulse-this-operator this-command)
    (cond
     ;; If text is selected, use selection boundaries as range
     ((or vimpulse-visual-mode (region-active-p))
      ;; Extend range to whole lines
      (when (and whole-lines
                 (not (eq 'line vimpulse-visual-mode)))
        (vimpulse-visual-activate 'line)
        (vimpulse-visual-dimensions))
      ;; Determine range and go to beginning
      (setq range (vimpulse-visual-range))
      (setq vimpulse-this-motion-type (vimpulse-motion-type range)
            range (vimpulse-motion-range range))
      (if (eq 'block vimpulse-this-motion-type)
          (vimpulse-visual-block-rotate
           'upper-left (apply 'min range) (apply 'max range))
        (goto-char (apply 'min range)))
      ;; Disable selection
      (setq vimpulse-this-motion 'vimpulse-visual-reselect)
      (unless keep-visual
        (if vimpulse-visual-mode
            (vimpulse-visual-mode -1)
          (vimpulse-deactivate-region))))
     ;; Not in Visual mode: use CUSTOM-MOTION if specified,
     ;; or read motion and return motion range
     (t
      (if custom-motion
          (setq vimpulse-this-motion custom-motion)
        (vimpulse-change-state-to-operator)
        (while (progn
                 (setq vimpulse-this-motion
                       (vimpulse-keypress-parser t))
                 (setq vimpulse-this-count
                       (if vimpulse-this-count
                           (if (numberp (cadr vimpulse-this-motion))
                               (string-to-number
                                (concat (number-to-string
                                         vimpulse-this-count)
                                        (number-to-string
                                         (cadr vimpulse-this-motion))))
                             vimpulse-this-count)
                         (cadr vimpulse-this-motion))
                       vimpulse-this-motion
                       (car vimpulse-this-motion))
                 (when (assq vimpulse-this-motion type-alist)
                   (setq type (cdr (assq vimpulse-this-motion
                                         type-alist))))))
        (message oldmsg)
        ;; Motion reading done: clear echo area
        ;; (message "")
        ;; Return current line motion if operator calls itself
        (if (eq vimpulse-this-operator vimpulse-this-motion)
            (setq vimpulse-this-motion 'vimpulse-line)
          (setq vimpulse-this-motion
                (vimpulse-operator-remapping vimpulse-this-motion))))
      (cond
       ;; Quit if motion reading failed
       ((or (not vimpulse-this-motion)
            (memq vimpulse-this-motion
                  '(viper-nil
                    keyboard-quit))
            (vimpulse-operator-cmd-p vimpulse-this-motion))
        (viper-change-state-to-vi)
        (setq quit-flag t))
       (t
        ;; Multiply operator count and motion count together
        (when (or current-prefix-arg vimpulse-this-count)
          (setq vimpulse-this-count
                (* (prefix-numeric-value current-prefix-arg)
                   (prefix-numeric-value vimpulse-this-count))))
        ;; Determine type to use for type conversion
        (when (and (eq 'inclusive type)
                   (memq (vimpulse-motion-type vimpulse-this-motion)
                         '(line inclusive)))
          (setq type 'exclusive))
        ;; Calculate motion range
        (setq range (vimpulse-make-motion-range
                     vimpulse-this-count vimpulse-this-motion type))
        (setq vimpulse-this-motion-type (vimpulse-motion-type range)
              range (vimpulse-motion-range range))
        ;; Go to beginning of range
        (unless dont-move-point
          (goto-char (apply 'min range))
          (when (and viper-auto-indent
                     (looking-back "^[ \f\t\v]*"))
            (back-to-indentation)))
        (viper-change-state-to-vi)))))
    ;; Set up repeat
    (unless no-repeat
      (setq vimpulse-last-operator vimpulse-this-operator
            vimpulse-last-motion vimpulse-this-motion
            vimpulse-last-motion-type
            (when type vimpulse-this-motion-type))
      (viper-set-destructive-command
       (list 'vimpulse-operator-repeat
             vimpulse-this-count nil viper-use-register nil nil)))
    ;; Return range
    range))

(defun vimpulse-make-motion-range (count motion &optional type refresh)
  "Derive motion range (TYPE BEG END) from MOTION and COUNT.
MOTION can move point or select some text (a text object).
TYPE may specify the motion type for normalizing the resulting range.
If REFRESH is t, this function changes `vimpulse-this-motion-type'
and point."
  ;; Unless REFRESH is t, we create a local binding for
  ;; `vimpulse-this-motion-type' so it's not affected
  (cond
   ((not refresh)
    (let (vimpulse-this-motion-type)
      (save-excursion
        (vimpulse-make-motion-range count motion type t))))
   (t
    (let ((current-prefix-arg count)
          (viper-intermediate-command 'viper-command-argument)
          (viper-current-state 'operator-state)
          (vimpulse-operator-basic-minor-mode t)
          (motion-type (vimpulse-motion-type motion t))
          (already-selection (or vimpulse-visual-mode
                                 (region-active-p)))
          range)
      (setq vimpulse-this-motion-type
            (or type motion-type 'exclusive))
      (viper-move-marker-locally 'viper-com-point (point))
      ;; Enable Transient Mark mode so we can reliably
      ;; detect selection commands
      (unless already-selection
        (vimpulse-transient-mark))
      ;; Execute MOTION
      (condition-case nil
          (if (commandp motion)
              (call-interactively motion)
            (funcall motion count))
        (error nil))
      (cond
       ;; If text has been selected (i.e., it's a text object),
       ;; return the selection
       ((and (or vimpulse-visual-mode (region-active-p))
             (not already-selection))
        (setq range (vimpulse-visual-range))
        (cond
         ((and motion-type (not (eq motion-type (car range))))
          (setcar range motion-type))
         ((and type (not (eq type (car range))))
          (setcar range type)
          (setq range (vimpulse-normalize-motion-range range))))
        ;; Deactivate region (and Transient Mark mode)
        ;; unless they were already activated
        (if vimpulse-visual-mode
            (vimpulse-visual-mode -1)
          (vimpulse-deactivate-region))
        (vimpulse-transient-restore))
       ;; Otherwise, range is defined by `viper-com-point'
       ;; and point (Viper type motion)
       (t
        (setq range (vimpulse-normalize-motion-range
                     (list (or type vimpulse-this-motion-type)
                           (marker-position viper-com-point)
                           (point))))
        (unless already-selection
          (vimpulse-transient-restore))))
      range))))

;; A keypress parser of some kind is unavoidable because we need to
;; know what we are executing beforehand (like when multiplying the
;; counts in "2d3w"). We try to avoid hard-coding where possible by
;; inspecting commands rather than the keys themselves.
(defun vimpulse-keypress-parser (&optional no-remap)
  "Read from keyboard and build a command description.
Returns (CMD COUNT), where COUNT is the numeric prefix argument
of CMD. Both COUNT and CMD may be nil."
  (let ((inhibit-quit t)
        (echo-keystrokes 0.01)
        char digit keys cmd count)
    (while (progn
             ;; Read a keypress, respecting Emacs version,
             ;; and convert it to ASCII representation
             (if (featurep 'xemacs)
                 (setq char (event-to-character
                             (next-command-event) nil t))
               (setq char (read-event))
               (when (symbolp char)
                 (setq char (or (get char 'ascii-character) char))))
             ;; This trick from simple.el's `digit-argument'
             ;; converts keystrokes like C-0 and C-M-1 to digits
             (setq digit (- (logand char ?\177) ?0))
             (if (keymapp cmd)
                 (setq keys (vconcat keys (vector char)))
               (setq keys (vector char)))
             (if no-remap              ; XEmacs doesn't have remapping
                 (setq cmd (key-binding keys t))
               (setq cmd (key-binding keys t t)))
             ;; This `cond' form determines whether
             ;; the reading loop will continue
             (cond
              ;; If calling itself ("cc"), return current command
              ((eq keys (vimpulse-strip-prefix
                         (vconcat (this-command-keys))))
               (setq cmd this-command)
               nil)
              ;; If CMD is a keymap, we need to read more
              ((keymapp cmd)
               t)
              ;; Numeric prefix argument
              ((or (memq cmd '(viper-digit-argument digit-argument))
                   ;; The 0 key runs `viper-beginning-of-line',
                   ;; so ignore it unless preceded by other digits
                   (and (eq 1 (length keys))
                        (not (keymapp cmd))
                        count
                        ;; Probably overkill: only 0 bound this way
                        (memq digit '(0 1 2 3 4 5 6 7 8 9))))
               ;; Store digits in a string, which is easily converted
               ;; to a number afterwards
               (setq count (concat (or count "")
                                   (number-to-string digit)))
               t)
              ;; Catch middle digits like "da2w"
              ((and (not cmd)
                    (< 1 (length keys))
                    (memq digit '(0 1 2 3 4 5 6 7 8 9)))
               (setq count (concat (or count "")
                                   (number-to-string digit)))
               ;; Remove the digit from the key sequence
               ;; so we can see if the previous one goes anywhere
               (setq keys (vimpulse-truncate keys -1))
               (setq cmd (key-binding keys))
               t)
              ;; We might as well accept negative numbers using
              ;; Emacs' C--. Best of both worlds, right?
              ((eq 'negative-argument cmd)
               (unless count
                 (setq count "-")))
              ;; User pressed C-g, so return nil for CMD
              ((eq 'keyboard-quit cmd)
               (setq cmd nil))
              ;; We are done, exit the `while' loop
              (t
               nil))))
    ;; Determine COUNT
    (when (stringp count)
      (if (string= "-" count)
          (setq count nil)
        (setq count (string-to-number count))))
    ;; Return command description
    (list cmd count)))

;;; Repeat an operator/motion combination

;; This is used in `viper-d-com' (read by `viper-repeat').
(defun vimpulse-operator-repeat (arg)
  "Repeat an operator-motion combination.
ARG is a list of the form (COUNT . COM).
COM is discarded."
  (let ((val (viper-P-val arg)))
    (cond
     ((region-active-p)
      (funcall operator (region-beginning) (region-end)))
     (t
      (vimpulse-operator-apply
       vimpulse-last-operator vimpulse-last-motion val
       vimpulse-last-motion-type)))))

(defun vimpulse-operator-apply (operator motion count &optional type)
  "Apply OPERATOR on MOTION. COUNT is the motion count.
TYPE is the motion type."
  (let* ((vimpulse-this-operator operator)
         (vimpulse-this-motion motion)
         (range (vimpulse-make-motion-range count motion type))
         (vimpulse-this-motion-type (vimpulse-motion-type range))
         (range (vimpulse-motion-range range))
         (beg (apply 'min range))
         (end (apply 'max range)))
    (funcall operator beg end)))

(defun vimpulse-region-cmd-p (cmd)
  "Return t if CMD is a region command."
  (let ((spec (car (cdr (interactive-form cmd)))))
    (and (stringp spec)
         (not (not (string-match "r" spec))))))

(defun vimpulse-operator-cmd-p (cmd)
  "Return t if CMD is an operator command."
  (vimpulse-memq-recursive 'vimpulse-range
                           (interactive-form cmd)))

;;; Motion type system

(defun vimpulse-range-p (object)
  "Return t if OBJECT is a pure range (BEG END)."
  (and (listp object)
       (eq 2 (length object))
       (numberp (car object))
       (numberp (cadr object))))

(defun vimpulse-motion-range-p (object)
  "Return t if OBJECT is a motion range (TYPE BEG END)."
  (and (symbolp (car object))
       (vimpulse-range-p (cdr object))))

(defun vimpulse-motion-range (object)
  "Return the range part of OBJECT."
  (cond
   ((vimpulse-motion-range-p object)
    (cdr object))
   ((vimpulse-range-p object)
    object)
   (t
    (list (point) (point)))))

(defun vimpulse-motion-type (object &optional raw)
  "Return motion type of OBJECT.
The type is one of `exclusive', `inclusive', `line' and `block'.
Defaults to `exclusive' unless RAW is specified."
  (let ((type (cond
               ((symbolp object)
                (get object 'motion-type))
               ((vimpulse-motion-range-p object)
                (car object)))))
    (if raw
        type
      (or type 'exclusive))))

;; This implements section 1 of motion.txt (Vim Reference Manual)
(defun vimpulse-normalize-motion-range (range &optional type)
  "Normalize the beginning and end of a motion range (TYPE FROM TO).
Returns the normalized range.

Usually, a motion range should be normalized only once, as
information is lost in the process: an unnormalized motion range
has the form (TYPE FROM TO), while a normalized motion range has
the form (TYPE BEG END).

See also `vimpulse-block-range', `vimpulse-line-range',
`vimpulse-inclusive-range' and `vimpulse-exclusive-range'."
  (let* ((type (or type (vimpulse-motion-type range)))
         (range (vimpulse-motion-range range))
         (from (car range))
         (to   (cadr range)))
    (cond
     ((memq type '(blockwise block))
      (vimpulse-block-range from to))
     ((memq type '(linewise line))
      (vimpulse-line-range from to))
     ((eq 'inclusive type)
      (vimpulse-inclusive-range from to))
     (t
      (vimpulse-exclusive-range from to t)))))

(defun vimpulse-block-range (mark point)
  "Return a blockwise motion range (BLOCK BEG END).
Like `vimpulse-inclusive-range', but for rectangles:
the last column is included."
  (let* ((point (or point (point)))
         (mark  (or mark point))
         (beg (min point mark))
         (end (max point mark))
         (beg-col (save-excursion
                    (goto-char beg)
                    (current-column)))
         (end-col (save-excursion
                    (goto-char end)
                    (current-column))))
    (save-excursion
      (cond
       ((= beg-col end-col)
        (goto-char end)
        (cond
         ((eolp)
          (goto-char beg)
          (if (eolp)
              (list 'block beg end)
            (list 'block (1+ beg) end)))
         (t
          (list 'block beg (1+ end)))))
       ((< beg-col end-col)
        (goto-char end)
        (if (eolp)
            (list 'block beg end)
          (list 'block beg (1+ end))))
       (t
        (goto-char beg)
        (if (eolp)
            (list 'block beg end)
          (list 'block (1+ beg) end)))))))

(defun vimpulse-line-range (mark point)
  "Return a linewise motion range (LINE BEG END)."
  (let* ((point (or point (point)))
         (mark  (or mark point))
         (beg (min mark point))
         (end (max mark point)))
    (list 'line
          (save-excursion
            (goto-char beg)
            (line-beginning-position))
          (save-excursion
            (goto-char end)
            (line-beginning-position 2)))))

(defun vimpulse-inclusive-range (mark point)
  "Return an inclusive motion range (INCLUSIVE BEG END).
That is, the last character is included."
  (let* ((point (or point (point)))
         (mark  (or mark point))
         (beg (min mark point))
         (end (max mark point)))
    (save-excursion
      (goto-char end)
      (unless (or (eobp) (and (eolp) (not (bolp))))
        (setq end (1+ end)))
      (list 'inclusive beg end))))

(defun vimpulse-exclusive-range (mark point &optional normalize)
  "Return an exclusive motion range (EXCLUSIVE BEG END).
However, if NORMALIZE is t and the end of the range is at the
beginning of a line, a different type of range is returned:

  * If the start of the motion is at or before the first
    non-blank in the line, the motion becomes `line' (normalized).

  * Otherwise, the end of the motion is moved to the end of the
    previous line and the motion becomes `inclusive' (normalized).

Thus, this function may return, e.g., (LINE BEG END) instead."
  (let* ((point (or point (point)))
         (mark  (or mark point))
         (beg (min mark point))
         (end (max mark point)))
    (save-excursion
      (cond
       ((and normalize
             (progn
               (goto-char end)
               (bolp)))
        (viper-backward-char-carefully)
        (setq end (max beg (point)))
        (cond
         ((save-excursion
            (goto-char beg)
            (looking-back "^[ \f\t\v]*"))
          (vimpulse-normalize-motion-range (list 'line beg end)))
         (t
          (list 'inclusive beg end))))
       (t
        (list 'exclusive beg end))))))

;;; Operators (yank, delete, change)

(defun vimpulse-yank (beg end)
  "Yank text from BEG to END."
  (interactive (vimpulse-range t t))
  (let ((length (abs (- beg end))))
    (cond
     ((eq 'block vimpulse-this-motion-type)
      (setq killed-rectangle (extract-rectangle beg end))
      ;; Associate the rectangle with the last entry in the kill-ring
      (unless kill-ring
        (copy-region-as-kill beg end))
      (put 'killed-rectangle 'previous-kill (current-kill 0))
      (vimpulse-operator-message "Saved <N>" beg end)
      (vimpulse-visual-block-rotate 'upper-left beg end))
     (t
      (vimpulse-store-in-current-register beg end)
      (copy-region-as-kill beg end)
      (unless (eq 'line vimpulse-this-motion-type)
        (goto-char beg))
      (when (and (eolp) (not (bolp)))
        (backward-char))
      (vimpulse-operator-message "Saved <N>" beg end)))))

(defun vimpulse-delete (beg end &optional dont-save)
  "Delete text from BEG to END.
If DONT-SAVE is t, just delete it."
  (interactive (vimpulse-range))
  (let ((length (if (eq 'line vimpulse-this-motion-type)
                    (count-lines beg end)
                  (abs (- end beg)))))
    (cond
     (dont-save
      (cond
       ((eq 'block vimpulse-this-motion-type)
        (delete-rectangle beg end))
       (t
        (delete-region beg end))))
     ((eq 'block vimpulse-this-motion-type)
      (let ((orig (make-marker)))
        ;; Associate the rectangle with the last entry in the kill-ring
        (viper-move-marker-locally
         'orig (vimpulse-visual-block-position 'upper-left beg end))
        (unless kill-ring
          (copy-region-as-kill beg end))
        (kill-rectangle beg end)
        (put 'killed-rectangle 'previous-kill (current-kill 0))
        (goto-char orig)
        (set-marker orig nil)
        (vimpulse-operator-message "Deleted <N>" beg end)))
     (t
      (vimpulse-store-in-current-register beg end)
      (kill-region beg end)
      (when (and (eolp) (not (bolp)))
        (backward-char))
      (vimpulse-operator-message "Deleted <N>" beg end length)))))

(defun vimpulse-change (beg end &optional dont-save)
  "Change text from BEG to END.
If DONT-SAVE is non-nil, just delete it."
  (interactive (vimpulse-range))
  (cond
   ((eq 'block vimpulse-this-motion-type)
    (vimpulse-delete beg end dont-save)
    (goto-char
     (vimpulse-visual-create-coords
      'block ?i
      (min vimpulse-visual-point vimpulse-visual-mark)
      (1+ (max vimpulse-visual-point vimpulse-visual-mark))))
    (viper-insert nil))
   ((eq viper-intermediate-command 'viper-repeat)
    (if dont-save
        (delete-region beg end)
      (kill-region beg end))
    (when (eq 'line vimpulse-this-motion-type)
      (save-excursion (newline))
      (when viper-auto-indent
        (indent-according-to-mode)))
    (viper-yank-last-insertion))
   ((eq 'line vimpulse-this-motion-type)
    (setq viper-began-as-replace t)
    (if dont-save
        (delete-region beg end)
      (vimpulse-store-in-current-register beg end)
      (kill-region beg end))
    (save-excursion (newline))
    (when viper-auto-indent
      (indent-according-to-mode))
    (viper-change-state-to-insert))
   (t
    (if dont-save
        (delete-region beg end)
      (vimpulse-store-in-current-register beg end)
      (viper-change beg end)))))

(defun vimpulse-operator-message
  (template &optional beg end length type)
  "Echo a message like \"Deleted 2 characters\".
TEMPLATE is a string like \"Deleted <N>\", where <N>
is substituted with the amount of characters or lines.
BEG and END are the range of text. If you specify LENGTH,
they are ignored.

This function respects `viper-change-notification-threshold'."
  (let* ((beg (or beg ((apply 'min (vimpulse-visual-range))) 1))
         (end (or end ((apply 'max (vimpulse-visual-range)) 1)))
         (height (or vimpulse-visual-height 1))
         (width (or vimpulse-visual-width 1))
         (type (or type vimpulse-this-motion-type))
         (length (if (eq 'line type)
                     (or length (count-lines beg end))
                   (or length (abs (- end beg)))))
         (template (replace-regexp-in-string
                    "<N>"
                    (apply 'format
                           (if (eq 'block type)
                               `("%s row%s and %s column%s"
                                 ,height
                                 ,(if (/= 1 (abs height)) "s" "")
                                 ,width
                                 ,(if (/= 1 (abs width)) "s" ""))
                             `(,(if (eq 'line type)
                                    "%s line%s" "%s character%s")
                               ,length
                               ,(if (/= 1 (abs length)) "s" ""))))
                    template)))
    (when (and (< viper-change-notification-threshold length)
               (not (viper-is-in-minibuffer)))
      (message template))))

(defun vimpulse-store-in-register (register start end)
  "Store text from START to END in REGISTER."
  (cond
   ((viper-valid-register register '(Letter))
    (viper-append-to-register
     (downcase register) start end))
   (t
    (copy-to-register register start end))))

(defun vimpulse-store-in-current-register (start end)
  "Store text from START to END in current register, if any.
Resets `viper-use-register'."
  (when viper-use-register
    (vimpulse-store-in-register viper-use-register start end)
    (setq viper-use-register nil)))

(defun vimpulse-read-register (&optional register command)
  "Use COMMAND with REGISTER.
If called interactively, read REGISTER and COMMAND from keyboard."
  (interactive)
  (setq register (or register (read-char)))
  (when (viper-valid-register register)
    (setq command (or command (key-binding (read-key-sequence nil))))
    (when (commandp command)
      (let ((this-command command)
            (viper-use-register register))
        (call-interactively command)))))

;;; Remap non-motion commands to `viper-nil'

(defun vimpulse-operator-remap (from to)
  "Remap FROM to TO in Operator-Pending mode."
  (vimpulse-remap vimpulse-operator-remap-map from to))

(defun vimpulse-operator-remapping (cmd)
  "Return Operator-Pending remapping for CMD."
  (if (featurep 'xemacs)
      (or (cdr (assq cmd vimpulse-operator-remap-alist)) cmd)
    vimpulse-operator-remap-minor-mode
    (or (command-remapping cmd) cmd)))

(vimpulse-operator-remap 'redo 'viper-nil)
(vimpulse-operator-remap 'undo 'viper-nil)
(vimpulse-operator-remap 'viper-Put-back 'viper-nil)
(vimpulse-operator-remap 'viper-delete-backward-char 'viper-nil)
(vimpulse-operator-remap 'viper-delete-char 'viper-nil)
(vimpulse-operator-remap 'viper-insert 'viper-nil)
(vimpulse-operator-remap 'viper-intercept-ESC-key 'viper-nil)
(vimpulse-operator-remap 'viper-line-to-bottom 'viper-nil)
(vimpulse-operator-remap 'viper-line-to-middle 'viper-nil)
(vimpulse-operator-remap 'viper-line-to-top 'viper-nil)
(vimpulse-operator-remap 'viper-put-back 'viper-nil)
(vimpulse-operator-remap 'viper-repeat 'viper-nil)
(vimpulse-operator-remap 'viper-substitute 'viper-nil)

;;; Utility macro for converting region commands to operators

(defmacro vimpulse-convert-to-operator (region-cmd &rest args)
  "Convert a region command to an operator command.
Defines a new command with the name REGION-CMD-operator.
ARGS is passed to `vimpulse-range'."
  (let ((region-cmd (eval region-cmd)))
    `(defun ,(intern (concat (symbol-name region-cmd) "-operator"))
       (beg end)
       ,(format "Operator-wrapper for `%s'.\n\n%s"
                region-cmd (documentation region-cmd t))
       (interactive (vimpulse-range ,@args))
       (,region-cmd beg end))))

;;; Compatibility code allowing old-style Viper motions to work

;; Postpone operator execution by disabling `viper-execute-com'.
;; However, some motions, like f and /, need to update `viper-d-com'
;; with negative count, command-keys, etc., to repeat properly.
(defadvice viper-execute-com (around vimpulse-operator activate)
  "Disable in Operator-Pending mode."
  (cond
   ((eq 'operator-state viper-current-state)
    (setq com ?r)
    ad-do-it
    (unless (eq 'viper-repeat viper-intermediate-command)
      (unless viper-d-com
        (setq viper-d-com (list nil nil nil nil nil nil)))
      (unless (eq vimpulse-this-motion
                  (vimpulse-operator-remapping m-com))
        (setq vimpulse-this-motion (vimpulse-operator-remapping m-com))
        (setcar (nthcdr 2 viper-d-com) com))
      (setq vimpulse-this-count val)
      (setcar (nthcdr 5 viper-d-com)
              (viper-array-to-string
               (if (arrayp viper-this-command-keys)
                   viper-this-command-keys
                 (this-command-keys))))))
   (t
    ad-do-it)))

;; This separates the operator-pending part of a Viper motion from the
;; rest, defining a new command called vimpulse-operator-MOTION
(defmacro vimpulse-operator-map-define
  (viper-motion &optional type &rest body)
  "Define a new command for the Operator-Pending part of VIPER-MOTION.
The new command is named VIMPULSE-OPERATOR-MOTION and has motion
type TYPE. A custom function body may be specified via BODY."
  (declare (indent 2))
  `(let* ((viper-motion ',viper-motion)
          (type ,type)
          (body ',body)
          (motion-name (symbol-name viper-motion))
          (docstring (documentation viper-motion t)))
     (setq type (or type (vimpulse-motion-type viper-motion)))
     (unless (memq type '(inclusive line block))
       (setq type 'exclusive))
     (setq motion-name (replace-regexp-in-string
                        "^viper-\\\|^vimpulse-" "" motion-name))
     (setq motion-name
           (concat "vimpulse-operator-" motion-name))
     (setq motion-name (intern motion-name))
     (eval-after-load 'vimpulse-visual-mode
       `(add-to-list 'vimpulse-movement-cmds ',motion-name))
     (vimpulse-operator-remap viper-motion motion-name)
     (eval `(defun ,motion-name (arg)
              ,(format "Operator-pending %s part of `%s'.\n\n%s"
                       type viper-motion (or docstring ""))
              ,@(if body body
                  `((interactive "P")
                    (let (com com-alist)
                      (setq com-alist
                            '((vimpulse-change . ?c)
                              (vimpulse-delete . ?d)
                              (vimpulse-yank . ?y)))
                      (setq com
                            (or (cdr (assq vimpulse-this-operator
                                           com-alist))
                                ?r))
                      (,viper-motion (if (region-active-p)
                                         arg
                                       (cons arg com)))
                      ,@(unless (eq 'exclusive type)
                          '((viper-backward-char-carefully))))))))
     (put motion-name 'motion-type type)
     `(quote ,motion-name)))

;; d%: when point is before the parenthetical expression,
;; include it in the resulting range
(vimpulse-operator-map-define viper-paren-match 'inclusive
  (interactive "P")
  (let ((orig (point)))
    (viper-paren-match arg)
    (viper-move-marker-locally 'viper-com-point orig)
    (when (integerp arg)
      (setq vimpulse-this-motion-type 'line))))

;; These motions need wrapper functions to repeat correctly
(vimpulse-operator-map-define viper-end-of-Word 'inclusive)
(vimpulse-operator-map-define viper-end-of-word 'inclusive)
(vimpulse-operator-map-define viper-find-char-backward 'inclusive)
(vimpulse-operator-map-define viper-find-char-forward 'inclusive)
(vimpulse-operator-map-define viper-forward-Word 'exclusive)
(vimpulse-operator-map-define viper-forward-char 'inclusive)
(vimpulse-operator-map-define viper-forward-word 'exclusive)
(vimpulse-operator-map-define viper-goto-char-backward 'inclusive)
(vimpulse-operator-map-define viper-goto-char-forward 'inclusive)
(vimpulse-operator-map-define viper-search-backward 'exclusive)
(vimpulse-operator-map-define viper-search-forward 'exclusive)

;; Set up motion types for remaining Viper motions
(put 'vimpulse-goto-first-line 'motion-type 'line)
(put 'viper-backward-Word 'motion-type 'exclusive)
(put 'viper-backward-char 'motion-type 'exclusive)
(put 'viper-backward-paragraph 'motion-type 'exclusive)
(put 'viper-backward-sentence 'motion-type 'exclusive)
(put 'viper-backward-word 'motion-type 'exclusive)
(put 'viper-beginning-of-line 'motion-type 'exclusive)
(put 'viper-forward-paragraph 'motion-type 'exclusive)
(put 'viper-forward-sentence 'motion-type 'exclusive)
(put 'viper-goto-eol 'motion-type 'inclusive)
(put 'viper-goto-line 'motion-type 'line)
(put 'viper-goto-mark 'motion-type 'exclusive)
(put 'viper-goto-mark-and-skip-white 'motion-type 'line)
(put 'viper-next-line 'motion-type 'line)
(put 'viper-previous-line 'motion-type 'line)
(put 'viper-search-Next 'motion-type 'exclusive)
(put 'viper-search-next 'motion-type 'exclusive)
(put 'viper-window-bottom 'motion-type 'line)
(put 'viper-window-middle 'motion-type 'line)
(put 'viper-window-top 'motion-type 'line)

(provide 'vimpulse-operator)

;;;; Text objects support

;; The following code implements support for text objects and commands
;; like diw, daw, ciw, caw. Currently, the most common objects are
;; supported:
;;
;;   - paren-blocks: b B { [ ( < > ) ] }
;;   - sentences: s
;;   - paragraphs: p
;;   - quoted expressions: " and '
;;   - words: w and W
;;
;; Vimpulse's text objects are fairly close to Vim's, and are based on
;; Viper's movement commands. More objects are easily added with
;; `vimpulse-define-text-object'.

(defmacro vimpulse-define-text-object (object args &rest body)
  "Define a text object OBJECT.
ARGS is the argument list, which must contain at least one argument:
the count. It is followed by an optional docstring and optional
keywords:

:keys KEYS      A key or a list of keys to bind the command to.
:map MAP        Keymap to bind :keys in, default
                `vimpulse-operator-basic-map'.
:type TYPE      The object's motion type.

The keywords are followed by the object's body, which must return
a pure range (BEG END) or a motion range (TYPE BEG END). Thus,
a simple example may look somewhat like:

    (vimpulse-define-text-object test (arg)
      \"Test object.\"
      :keys \"t\"
      (list 'exclusive (point) (+ arg (point))))

Here, the count is stored in ARG. Note that the body must be able
to handle a negative value, which specifies reverse direction."
  (declare (indent defun))
  (let ((map vimpulse-operator-basic-map)
        count doc keys keyword type)
    ;; Collect COUNT argument
    (setq args (or args (list 'arg))
          count (car args))
    ;; Collect docstring, if any
    (when (stringp (car body))
      (setq doc  (list (car body))      ; for splicing
            body (cdr body)))
    ;; Collect keywords
    (while (keywordp (setq keyword (car body)))
      (setq body (cdr body))
      (cond
       ((eq :keys keyword)
        (setq keys (vimpulse-unquote (pop body))))
       ((eq :maps keyword)
        (setq maps (vimpulse-unquote (pop body))))
       ((eq :type keyword)
        (setq type (vimpulse-unquote (pop body))))
       (t
        (pop body))))
    ;; Define key bindings
    (unless (keymapp map)
      (setq map (eval map)))
    (unless (listp keys)
      (setq keys (list keys)))
    (dolist (key keys)
      (define-key map key object))
    ;; Set motion type
    (when type
      (put object 'motion-type type)
      (setq type `(',type))) ; for splicing
    ;; Define command
    `(defun ,object ,args
       ,@doc
       (interactive "p")
       (let ((,count (if (numberp ,count) ,count 1))
             range dir)
         (cond
          ((region-active-p)
           (when (< (point) (mark t))
             (setq ,count (- ,count)))
           (when (memq vimpulse-visual-mode '(line block))
             (vimpulse-visual-activate 'normal))
           (when (and vimpulse-visual-mode
                      (not vimpulse-visual-region-expanded))
             (vimpulse-visual-expand-region))
           (setq range (progn ,@body))
           (unless (vimpulse-mark-range range t ,@type)
             ;; Are we stuck (unchanged region)?
             ;; Move forward and try again.
             (viper-forward-char-carefully (if (> 0 ,count) -1 1))
             (setq range (progn ,@body))
             (vimpulse-mark-range range t ,@type)))
          (t
           (setq range (progn ,@body))
           (vimpulse-mark-range range nil ,@type)))))))

(when (fboundp 'font-lock-add-keywords)
  (font-lock-add-keywords
   'emacs-lisp-mode
   '(("(\\(vimpulse-define-text-object\\)\\>[ \f\t\n\r\v]*\\(\\sw+\\)?"
      (1 font-lock-keyword-face)
      (2 font-lock-function-name-face nil t)))))

(defun vimpulse-mark-range (range &optional widen type)
  "Mark RANGE, which has the form (BEG END) or (TYPE BEG END).
If WIDEN is non-nil, expands existing region. If the TYPE
argument is specified, it overrides the type of RANGE."
  (let* ((type  (or type (vimpulse-motion-type range)))
         (range (vimpulse-motion-range range))
         (beg (apply 'min range))
         (end (apply 'max range)))
    (cond
     ((eq 'exclusive type)
      (if vimpulse-visual-mode
          (vimpulse-visual-select beg end widen)
        (vimpulse-set-region beg end widen)))
     (t
      (when vimpulse-visual-mode
        (unless (memq type '(line block))
          (setq type 'normal))
        (unless (eq type vimpulse-visual-mode)
          (vimpulse-visual-activate type)))
      (vimpulse-visual-select beg end widen)))))

;;; Text object range functions

;; Word-like expressions (words, sentences, paragraphs)
(defun vimpulse-object-range
  (count backward-func forward-func &optional type)
  "Return a text object range (TYPE BEG END).
BACKWARD-FUNC moves point to the object's beginning,
FORWARD-FUNC moves to its end. Schematically,

\(vimpulse-object-range <num> <beg-of-object> <end-of-object>)

COUNT is the number of objects. If positive, go forwards and
then backwards; if negative, go backwards and then forwards.

The type of the object (`exclusive', `inclusive' or `line')
may be specified with TYPE. Otherwise, the type is inferred
from the motion types of BACKWARD-FUNC and FORWARD-FUNC."
  (let ((types '(exclusive inclusive line block))
        beg end forward-range backward-range
        vimpulse-this-motion-type)
    (save-excursion
      (setq count (or (if (eq 0 count) 1 count) 1))
      (if (> 0 count)
          (setq backward-range
                (vimpulse-make-motion-range
                 (abs count) backward-func type t)
                forward-range
                (vimpulse-make-motion-range
                 (abs count) forward-func type t))
        (setq forward-range
              (vimpulse-make-motion-range
               (abs count) forward-func type t)
              backward-range
              (vimpulse-make-motion-range
               (abs count) backward-func type t)))
      (setq beg (apply 'min (vimpulse-motion-range backward-range))
            end (apply 'max (vimpulse-motion-range forward-range)))
      (unless type
        (setq type 'exclusive)
        (dolist (elt types)
          (when (or (eq elt (vimpulse-motion-type backward-range))
                    (eq elt (vimpulse-motion-type forward-range)))
            (setq type elt))))
      (list type beg end))))

(defun vimpulse-an-object-range
  (count backward-func forward-func &optional include-newlines regexp)
  "Return a text object range (BEG END) with whitespace.
Unless INCLUDE-NEWLINES is t, whitespace inclusion is restricted
to the line(s) the object is on. REGEXP is a regular expression
for matching whitespace; the default is \"[ \\f\\t\\n\\r\\v]+\".
See `vimpulse-object-range' for more details."
  (let (range beg end line-beg line-end mark-active-p)
    (save-excursion
      (setq count (or (if (eq 0 count) 1 count) 1))
      (setq regexp (or regexp "[ \f\t\n\r\v]+"))
      (setq range (vimpulse-motion-range
                   (vimpulse-object-range
                    count backward-func forward-func)))
      ;; Let `end' be the boundary furthest from point,
      ;; based on the direction we are going
      (if (> 0 count)
          (setq beg (cadr range)
                end (car range))
        (setq beg (car range)
              end (cadr range)))
      ;; If INCLUDE-NEWLINES is nil, never move past
      ;; the line boundaries of the text object
      (unless include-newlines
        (setq line-beg (line-beginning-position)
              line-end (line-end-position))
        (when (< (max (* count line-beg) (* count line-end))
                 (* count beg))
          (setq count (- count))
          (setq range (vimpulse-motion-range
                       (vimpulse-object-range
                        count backward-func forward-func)))
          (if (> 0 count)
              (setq beg (cadr range)
                    end (car range))
            (setq beg (car range)
                  end (cadr range))))
        (setq line-beg (save-excursion
                         (goto-char (min beg end))
                         (line-beginning-position))
              line-end (save-excursion
                         (goto-char (max beg end))
                         (line-end-position))))
      ;; Generally only include whitespace at one side (but see below).
      ;; If we are before the object, include leading whitespace;
      ;; if we are inside the object, include trailing whitespace.
      ;; If trailing whitespace inclusion fails, include leading.
      (setq count (if (> 0 count) -1 1))
      (when (or (< (* count (point)) (* count beg))
                (eq end (setq end (save-excursion
                                    (goto-char end)
                                    (vimpulse-skip-regexp
                                     regexp count line-beg line-end)))))
        (setq beg (save-excursion
                    (goto-char beg)
                    (if (and (not include-newlines)
                             (looking-back "^[ \t]*"))
                        beg
                      (vimpulse-skip-regexp
                       regexp (- count) line-beg line-end))))
        ;; Before/after adjustment for whole lines: if the object is
        ;; followed by a blank line, include that as trailing
        ;; whitespace and subtract a line from the leading whitespace
        (when include-newlines
          (goto-char end)
          (forward-line count)
          (when (looking-at "[ \t]*$")
            (setq end (line-beginning-position))
            (goto-char beg)
            (when (looking-at "[ \t]*$")
              (forward-line count)
              (setq beg (line-beginning-position))))))
      ;; Return the range
      (list (min beg end) (max beg end)))))

(defun vimpulse-inner-object-range
  (count backward-func forward-func)
  "Return a text object range (BEG END) including point.
If point is outside the object, it is included in the range.
To include whitespace, use `vimpulse-an-object-range'.
See `vimpulse-object-range' for more details."
  (let (range beg end line-beg line-end)
    (setq count (or (if (eq 0 count) 1 count) 1))
    (setq range (vimpulse-motion-range
                 (vimpulse-object-range
                  count backward-func forward-func)))
    (setq beg (car range)
          end (cadr range))
    (setq line-beg (line-beginning-position)
          line-end (line-end-position))
    (when (< (max (* count line-beg) (* count line-end))
             (min (* count beg) (* count end)))
      (setq count (- count))
      (setq range (vimpulse-motion-range
                   (vimpulse-object-range
                    count backward-func forward-func))
            beg (car range)
            end (cadr range)))
    ;; Return the range, including point
    (list (min beg (point)) (max end (point)))))

;; Parenthetical expressions
(defun vimpulse-paren-range (count &optional open close include-parentheses)
  "Return a parenthetical expression range (BEG END).
The type of parentheses may be specified with OPEN and CLOSE,
which must be characters. INCLUDE-PARENTHESES specifies
whether to include the parentheses in the range."
  (let ((beg (point)) (end (point))
        line-beg line-end)
    (setq count (if (eq 0 count) 1 (abs count)))
    (save-excursion
      (setq open  (if (characterp open)
                      (regexp-quote (string open)) "")
            close (if (characterp close)
                      (regexp-quote (string close)) ""))
      (when (and (not (string= "" open))
                 (looking-at open))
        (forward-char))
      ;; Find opening and closing paren with
      ;; Emacs' S-exp facilities
      (while (progn
               (vimpulse-backward-up-list 1)
               (not (when (looking-at open)
                      (when (save-excursion
                              (forward-sexp)
                              (when (looking-back close)
                                (setq end (point))))
                        (if (<= 0 count)
                            (setq beg (point))
                          (setq count (1- count)) nil))))))
      (if include-parentheses
          (list beg end)
        (setq beg (prog1 (min (1+ beg) end)
                    (setq end (max (1- end) beg))))
        ;; Multi-line inner range: select whole lines
        (if (>= 1 (count-lines beg end))
            (list beg end)
          (goto-char beg)
          (when (looking-at "[ \f\t\n\r\v]*$")
            (forward-line)
            ;; Include indentation?
            (if (and viper-auto-indent
                     (not (eq 'vimpulse-delete
                              vimpulse-this-operator)))
                (back-to-indentation)
              (beginning-of-line))
            (setq beg (point)))
          (goto-char end)
          (when (and (looking-back "^[ \f\t\n\r\v]*")
                     (not (eq 'vimpulse-delete
                              vimpulse-this-operator)))
            (setq end (line-end-position 0))
            (goto-char end))
          (list (min beg end) (max beg end)))))))

;; Quoted expressions
(defun vimpulse-quote-range (count &optional quote include-quotes)
  "Return a quoted expression range (BEG END).
QUOTE is a quote character (default ?\\\"). INCLUDE-QUOTES
specifies whether to include the quote marks in the range."
  (let ((beg (point)) (end (point))
        regexp)
    (save-excursion
      (setq count (if (eq 0 count) 1 (abs count)))
      (setq quote (or quote ?\"))
      (setq quote (if (characterp quote)
                      (regexp-quote (string quote)) "")
            regexp (concat "\\([^\\\\]\\|^\\)" quote))
      (when (and (not (string= "" quote))
                 (looking-at quote))
        (forward-char))
      ;; Search forward for a closing quote
      (while (and (< 0 count)
                  (re-search-forward regexp nil t))
        (setq count (1- count))
        (setq end (point))
        ;; Find the matching opening quote
        (condition-case nil
            (progn
              (setq beg (scan-sexps end -1))
              ;; Emacs' S-exp logic doesn't work in text mode
              (save-excursion
                (goto-char beg)
                (unless (looking-at quote)
                  (re-search-backward regexp)
                  (unless (looking-at quote)
                    (forward-char))
                  (setq beg (point)))))
          ;; Finding the opening quote failed. Maybe we're already at
          ;; the opening quote and should look for the closing instead?
          (error (condition-case nil
                     (progn
                       (viper-backward-char-carefully)
                       (setq beg (point))
                       (setq end (scan-sexps beg 1))
                       (unless (looking-back quote)
                         (re-search-forward regexp)
                         (unless (looking-back quote)
                           (backward-char))
                         (setq end (point))))
                   (error (setq end beg))))))
      (if include-quotes
          (list beg end)
        (list (min (1+ beg) end) (max (1- end) beg))))))

;;; Text object definitions

(vimpulse-define-text-object vimpulse-line (arg)
  "Select ARG lines."
  :type 'line
  (setq arg (1- arg))
  (vimpulse-line-range
   (point)
   (save-excursion
     (when (< 0 arg)
       (viper-next-line-carefully arg))
     (point))))

(vimpulse-define-text-object vimpulse-a-word (arg)
  "Select a word."
  :keys "aw"
  (vimpulse-an-object-range
   arg
   (lambda (arg)
     (vimpulse-limit (line-beginning-position) (line-end-position)
       (viper-backward-word (cons arg ?r))))
   (lambda (arg)
     (vimpulse-limit (line-beginning-position) (line-end-position)
       (viper-end-of-word (cons arg ?r))))))

(vimpulse-define-text-object vimpulse-inner-word (arg)
  "Select inner word."
  :keys "iw"
  (vimpulse-inner-object-range
   arg
   (lambda (arg)
     (vimpulse-limit (line-beginning-position) (line-end-position)
       (viper-backward-word (cons arg ?r))))
   (lambda (arg)
     (vimpulse-limit (line-beginning-position) (line-end-position)
       (viper-end-of-word (cons arg ?r))))))

(vimpulse-define-text-object vimpulse-a-Word (arg)
  "Select a Word."
  :keys "aW"
  (vimpulse-an-object-range
   arg
   (lambda (arg)
     (vimpulse-limit (line-beginning-position) (line-end-position)
       (viper-backward-Word (cons arg ?r))))
   (lambda (arg)
     (vimpulse-limit (line-beginning-position) (line-end-position)
       (viper-end-of-Word (cons arg ?r))))))

(vimpulse-define-text-object vimpulse-inner-Word (arg)
  "Select inner Word."
  :keys "iW"
  (vimpulse-inner-object-range
   arg
   (lambda (arg)
     (vimpulse-limit (line-beginning-position) (line-end-position)
       (viper-backward-Word (cons arg ?r))))
   (lambda (arg)
     (vimpulse-limit (line-beginning-position) (line-end-position)
       (viper-end-of-Word (cons arg ?r))))))

(vimpulse-define-text-object vimpulse-a-sentence (arg)
  "Select a sentence."
  :keys "as"
  (vimpulse-an-object-range
   arg
   (lambda (arg)
     (viper-backward-sentence arg)
     (vimpulse-skip-regexp "[ \f\t\n\r\v]+" 1))
   (lambda (arg)
     (viper-forward-sentence arg)
     (vimpulse-skip-regexp "[ \f\t\n\r\v]+" -1))))

(vimpulse-define-text-object vimpulse-inner-sentence (arg)
  "Select inner sentence."
  :keys "is"
  (vimpulse-inner-object-range
   arg
   (lambda (arg)
     (viper-backward-sentence arg)
     (vimpulse-skip-regexp "[ \f\t\n\r\v]+" 1))
   (lambda (arg)
     (viper-forward-sentence arg)
     (vimpulse-skip-regexp "[ \f\t\n\r\v]+" -1))))

(vimpulse-define-text-object vimpulse-a-paragraph (arg)
  "Select a paragraph."
  :keys "ap"
  (vimpulse-an-object-range
   arg
   (lambda (arg)
     (vimpulse-skip-regexp "[ \f\t\n\r\v]+" -1)
     (viper-backward-paragraph arg)
     (vimpulse-skip-regexp "[ \f\t\n\r\v]+" 1))
   (lambda (arg)
     (vimpulse-skip-regexp "[ \f\t\n\r\v]+" 1)
     (viper-forward-paragraph arg)
     (vimpulse-skip-regexp "[ \f\t\n\r\v]+" -1)) t))

(vimpulse-define-text-object vimpulse-inner-paragraph (arg)
  "Select inner paragraph."
  :keys "ip"
  (vimpulse-inner-object-range
   arg
   (lambda (arg)
     (vimpulse-skip-regexp "[ \f\t\n\r\v]+" -1)
     (viper-backward-paragraph arg)
     (vimpulse-skip-regexp "[ \f\t\n\r\v]+" 1))
   (lambda (arg)
     (vimpulse-skip-regexp "[ \f\t\n\r\v]+" 1)
     (viper-forward-paragraph arg)
     (vimpulse-skip-regexp "[ \f\t\n\r\v]+" -1))))

(vimpulse-define-text-object vimpulse-a-paren (arg)
  "Select a parenthesis."
  :keys '("ab" "a(" "a)")
  (vimpulse-paren-range arg ?\( nil t))

(vimpulse-define-text-object vimpulse-inner-paren (arg)
  "Select inner parenthesis."
  :keys '("ib" "i(" "i)")
  (vimpulse-paren-range arg ?\())

(vimpulse-define-text-object vimpulse-a-bracket (arg)
  "Select a bracket parenthesis."
  :keys '("a[" "a]")
  (vimpulse-paren-range arg ?\[ nil t))

(vimpulse-define-text-object vimpulse-inner-bracket (arg)
  "Select inner bracket parenthesis."
  :keys '("i[" "i]")
  (vimpulse-paren-range arg ?\[))

(vimpulse-define-text-object vimpulse-a-curly (arg)
  "Select a curly parenthesis."
  :keys '("aB" "a{" "a}")
  (vimpulse-paren-range arg ?{ nil t))

(vimpulse-define-text-object vimpulse-inner-curly (arg)
  "Select inner curly parenthesis."
  :keys '("iB" "i{" "i}")
  (vimpulse-paren-range arg ?{))

(vimpulse-define-text-object vimpulse-an-angle (arg)
  "Select an angle bracket."
  :keys '("a<" "a>")
  (vimpulse-paren-range arg ?< nil t))

(vimpulse-define-text-object vimpulse-inner-angle (arg)
  "Select inner angle bracket."
  :keys '("i<" "i>")
  (vimpulse-paren-range arg ?<))

(vimpulse-define-text-object vimpulse-a-single-quote (arg)
  "Select a single quoted expression."
  :keys "a'"
  (vimpulse-quote-range arg ?' t))

(vimpulse-define-text-object vimpulse-inner-single-quote (arg)
  "Select inner single quoted expression."
  :keys "i'"
  (vimpulse-quote-range arg ?'))

(vimpulse-define-text-object vimpulse-a-double-quote (arg)
  "Select a double quoted expression."
  :keys "a\""
  (vimpulse-quote-range arg ?\" t))

(vimpulse-define-text-object vimpulse-inner-double-quote (arg)
  "Select inner double quoted expression."
  :keys "i\""
  (vimpulse-quote-range arg ?\"))

(provide 'vimpulse-text-object-system)

;;;; Visual mode

;; Visual mode is defined as another Viper state, just like vi state,
;; Insert state, Replace state etc. It inherits keybindings from
;; vi state (movement), but defines some bindings of its own
;; on top of that.
;;
;; Text selection in Emacs and Vim differs subtly by that in Vim, the
;; character under the cursor is always included in the selection,
;; while Emacs' region excludes it when point follows mark. Vimpulse
;; solves the problem by "translating" a Visual selection to the
;; equivalent Emacs region when a command is about to be executed.
;; Likewise, a Line selection is translated to an Emacs region of
;; whole lines.
;;
;; This is pretty transparent, except that we don't wish to do any
;; translating when the user is just moving around in the buffer.
;; To that end, the variable `vimpulse-movement-cmds' lists all of
;; Viper's movement commands, so that translation can be postponed
;; until the user executes a non-movement command.
;;
;; Block selections are rectangle compatible. This means Emacs'
;; rectangular commands are applicable on the selection, and you can
;; write your own utilities using the rect.el library. Alternatively,
;; use the `vimpulse-apply-on-block' function.

(vimpulse-define-state visual
  "Visual mode is a flexible and easy way to select text.
To use Visual mode, press v in vi (command) mode. Then use the
motion commands to expand the selection. Press d to delete, c to
change, r to replace, or y to copy. You can use p to paste.
For Line selection, press V instead of v; then you can copy and
paste whole lines. For Block selection, press C-v; now you can
copy and paste the selected rectangle. In Block selection, you
may use I or A to insert or append text before or after the
selection on each line."
  :id "<VIS> "
  :basic-minor-mode 'vimpulse-visual-mode
  :enable '((vimpulse-visual-mode (or vimpulse-visual-mode t))
            (vimpulse-operator-remap-minor-mode nil)
            operator-state
            vi-state)
  (cond
   ((eq 'visual-state new-state)
    (unless (memq vimpulse-visual-mode '(normal line block))
      (vimpulse-visual-mode 1)))
   (t
    (vimpulse-visual-mode -1))))

(defgroup vimpulse-visual nil
  "Visual mode for Viper."
  :prefix "vimpulse-visual-"
  :group  'vimpulse)

;; Visual mode consists of three "submodes": characterwise, linewise
;; and blockwise selection. We implement this by setting the mode
;; variable `vimpulse-visual-mode' to either `normal', `line' or
;; `block'.
(define-minor-mode vimpulse-visual-mode
  "Toggles Visual mode in Viper."
  :initial-value nil
  :keymap vimpulse-visual-basic-map
  :global nil
  :group 'vimpulse-visual
  (cond
   (vimpulse-visual-mode
    (unless (memq vimpulse-visual-mode '(normal line block))
      (vimpulse-visual-activate 'normal)))
   (t
    ;; This is executed when we do (vimpulse-visual-mode -1).
    ;; It must run without error even if Visual mode is not active.
    (vimpulse-visual-highlight -1)
    ;; Clean up local variables
    (dolist (var vimpulse-visual-local-vars)
      (when (assq var vimpulse-visual-vars-alist)
        (set var (cdr (assq var vimpulse-visual-vars-alist))))
      (when (memq var vimpulse-visual-global-vars)
        (kill-local-variable var)))
    ;; Deactivate mark
    (when vimpulse-visual-vars-alist
      (vimpulse-deactivate-mark t))
    (vimpulse-transient-restore)
    (kill-local-variable 'vimpulse-visual-vars-alist)
    (kill-local-variable 'vimpulse-visual-global-vars)
    ;; If Viper state is not already changed,
    ;; change it to vi (command) state
    (when (eq viper-current-state 'visual-state)
      (cond
       ((eq 'emacs-state vimpulse-visual-previous-state)
        (viper-change-state-to-emacs))
       (t
        (viper-change-state-to-vi))))
    (kill-local-variable 'vimpulse-visual-previous-state))))

(defvar vimpulse-visual-remap-alist nil
  "Association list of command remappings in Visual mode.")

(put 'vimpulse-visual-basic-map
     'remap-alist 'vimpulse-visual-remap-alist)

(viper-deflocalvar vimpulse-visual-mode nil
  "Current Visual mode: may be nil, `normal', `line' or `block'.")

(defcustom vimpulse-visual-block-untabify nil
  "Whether Block mode may change tabs to spaces for fine movement.
Off by default."
  :type  'boolean
  :group 'vimpulse-visual)

(viper-deflocalvar vimpulse-visual-global-vars nil
  "List of variables which were global.")

(viper-deflocalvar vimpulse-visual-local-vars
  '(cua-mode
    mark-active
    transient-mark-mode
    zmacs-regions
    vimpulse-visual-region-expanded)
  "System variables which are reset for each Visual session.")

(viper-deflocalvar vimpulse-visual-vars-alist nil
  "Alist of old variable values.")

(viper-deflocalvar vimpulse-visual-last nil
  "Last active Visual mode.
May be `normal', `line', `block' or nil.")

(viper-deflocalvar vimpulse-visual-previous-state 'viper-state
  "Previous state before enabling Visual mode.
This lets us revert to Emacs state in non-vi buffers.")

(viper-deflocalvar vimpulse-visual-region-expanded nil
  "Whether region is expanded to the Visual selection.")

(viper-deflocalvar vimpulse-visual-point nil
  "Last expanded `point' in Visual mode.")

(viper-deflocalvar vimpulse-visual-mark nil
  "Last expanded `mark' in Visual mode.")

(defvar vimpulse-visual-height nil
  "Height of last Visual selection.")

(defvar vimpulse-visual-width nil
  "Width of last Visual selection.")

(viper-deflocalvar vimpulse-visual-overlay nil
  "Overlay for Visual selection.
In XEmacs, this is an extent.")

(viper-deflocalvar vimpulse-visual-block-overlays nil
  "Overlays for Visual Block selection.")

(viper-deflocalvar vimpulse-visual-whitespace-overlay nil
  "Overlay encompassing text inserted into the buffer
to make Block selection at least one column wide.")

;; Defined in rect.el
(defvar killed-rectangle nil)

(viper-deflocalvar vimpulse-undo-needs-adjust nil
  "If true, several commands in the undo-list should be connected.")

(defconst vimpulse-buffer-undo-list-mark 'vimpulse
  "Everything up to this mark is united in the undo-list.")

;; This variable holds the point and column of the first line
;; as well as the number of lines in the region
(defvar vimpulse-visual-insert-coords nil
  "List with (I-COM UL-POS COL NLINES), where
I-COM is the insert command (?i, ?a, ?I or ?A),
UL-POS is the position of the upper left corner of the region,
COL is the column of insertion, and
NLINES is the number of lines in the region.")

(defun vimpulse-visual-remap (from to)
  "Remap FROM to TO in Visual mode."
  (vimpulse-remap vimpulse-visual-basic-map from to))

(defun vimpulse-filter-undos (undo-list)
  "Filters all `nil' marks from `undo-list' until the first
occurrence of `vimpulse-buffer-undo-list-mark'."
  (cond
   ((null undo-list)
    nil)
   ((eq (car undo-list) 'vimpulse)
    (cdr undo-list))
   ((null (car undo-list))
    (vimpulse-filter-undos (cdr undo-list)))
   (t
    (cons (car undo-list)
          (vimpulse-filter-undos (cdr undo-list))))))

(defun vimpulse-connect-undos ()
  "Connects all undo-steps from `buffer-undo-list' up to the
first occurrence of `vimpulse-buffer-undo-list-mark'."
  (when (and vimpulse-undo-needs-adjust
             (listp buffer-undo-list))
    (setq buffer-undo-list
          (vimpulse-filter-undos buffer-undo-list)))
  (setq vimpulse-undo-needs-adjust nil))

(defun vimpulse-push-buffer-undo-list-mark ()
  (setq vimpulse-undo-needs-adjust t)
  (push vimpulse-buffer-undo-list-mark buffer-undo-list))

;;; Activation

(defun vimpulse-visual-activate (&optional mode)
  "Activate Visual mode. MODE is `normal', `line' or `block'.
May also be used to change the Visual mode."
  (unless (memq vimpulse-visual-mode '(normal line block))
    ;; We are activating Visual mode for the first time
    (kill-local-variable 'vimpulse-visual-vars-alist)
    (kill-local-variable 'vimpulse-visual-global-vars)
    (setq vimpulse-visual-previous-state viper-current-state)
    ;; Make global variables buffer-local
    (setq vimpulse-visual-vars-alist nil)
    (vimpulse-visual-block-cleanup-whitespace)
    (dolist (var vimpulse-visual-local-vars)
      (when (boundp var)
        ;; Remember old value
        (add-to-list 'vimpulse-visual-vars-alist
                     (cons var (eval var))))
      (unless (assoc var (buffer-local-variables))
        (make-local-variable var)
        (add-to-list 'vimpulse-visual-global-vars var)))
    ;; Re-add hooks in case they were cleared
    (add-hook 'pre-command-hook 'vimpulse-visual-pre-command)
    (add-hook 'post-command-hook 'vimpulse-visual-post-command)
    (if (featurep 'xemacs)
        (add-hook 'zmacs-deactivate-region-hook
                  'vimpulse-visual-deactivate-hook)
      (add-hook 'deactivate-mark-hook 'vimpulse-visual-deactivate-hook))
    ;; Activate mark at point
    (cond
     ((eq 'block mode)
      (set-mark (point))
      (vimpulse-deactivate-mark t)     ; `set-mark' activates the mark
      (vimpulse-transient-mark -1))
     (t
      (vimpulse-transient-mark 1)
      ;; Convert active Emacs region to Visual selection, if any
      (cond
       ((region-active-p)
        (vimpulse-visual-contract-region
         (not viper-ESC-moves-cursor-back)))
       (t
        (vimpulse-activate-mark (point))))
      (vimpulse-visual-highlight))))
  ;; Set the Visual mode
  (setq mode (or mode 'normal))
  (setq vimpulse-visual-mode mode
        vimpulse-visual-last mode)
  (viper-change-state 'visual-state)
  (viper-restore-cursor-type)           ; use vi cursor
  ;; Reactivate mark
  (cond
   ((eq 'block mode)
    (vimpulse-deactivate-mark t)
    (vimpulse-transient-mark -1))
   (t
    (vimpulse-transient-mark 1)
    (vimpulse-activate-mark))))

(defun vimpulse-visual-toggle (mode)
  "Enable Visual MODE if this is not the current mode.
Otherwise disable Visual mode."
  (if (eq vimpulse-visual-mode mode)
      (vimpulse-visual-mode -1)
    (vimpulse-visual-activate mode)))

(defun vimpulse-visual-activate-normal ()
  "Enable Visual selection."
  (interactive)
  (let (message-log-max)
    (vimpulse-visual-activate 'normal)
    (message "-- VISUAL --")))

(defun vimpulse-visual-activate-line ()
  "Enable Visual Line selection."
  (interactive)
  (let (message-log-max)
    (vimpulse-visual-activate 'line)
    (message "-- VISUAL LINE --")))

(defun vimpulse-visual-activate-block ()
  "Enable Visual Block selection."
  (interactive)
  (let (message-log-max)
    (vimpulse-visual-activate 'block)
    (message "-- VISUAL BLOCK --")))

(defun vimpulse-visual-toggle-normal ()
  "Toggle Visual selection."
  (interactive)
  (let (message-log-max)
    (vimpulse-visual-toggle 'normal)
    (when vimpulse-visual-mode
      (message "-- VISUAL --"))))

(defun vimpulse-visual-toggle-line ()
  "Toggle Visual Line selection."
  (interactive)
  (let (message-log-max)
    (vimpulse-visual-toggle 'line)
    (when vimpulse-visual-mode
      (message "-- VISUAL LINE --"))))

(defun vimpulse-visual-toggle-block ()
  "Toggle Visual Block selection."
  (interactive)
  (let (message-log-max)
    (vimpulse-visual-toggle 'block)
    (when vimpulse-visual-mode
      (message "-- VISUAL BLOCK --"))))

;;; Visualization

(defun vimpulse-deactivate-mark (&optional now)
  "Don't deactivate mark in Visual mode."
  (cond
   ((and vimpulse-visual-mode
         (not (eq 'block vimpulse-visual-mode)))
    nil)
   (t
    (vimpulse-deactivate-region now))))

(fset 'viper-deactivate-mark 'vimpulse-deactivate-mark)
(fset 'vimpulse-activate-mark 'vimpulse-activate-region)

(defun vimpulse-transient-mark (&optional arg)
  "Enable Transient Mark mode (and Cua mode) if not already enabled.
Enable forcefully with positive ARG. Disable with negative ARG."
  (setq deactivate-mark nil)
  (and (boundp 'mark-active)
       (setq mark-active (region-active-p)))
  (let (deactivate-mark)
    (cond
     ;; Disable Transient Mark/Cua
     ((and (integerp arg) (> 1 arg))
      (and (fboundp 'cua-mode)
           cua-mode
           (cua-mode -1))
      (and (fboundp 'transient-mark-mode)
           transient-mark-mode
           (transient-mark-mode -1))
      (and (boundp 'zmacs-regions)
           (setq zmacs-regions nil)))
     ;; Enable Transient Mark/Cua
     (t
      (unless vimpulse-visual-vars-alist
        (when (boundp 'transient-mark-mode)
          (add-to-list 'vimpulse-visual-vars-alist
                       (cons 'transient-mark-mode
                             transient-mark-mode)))
        (when (boundp 'cua-mode)
          (add-to-list 'vimpulse-visual-vars-alist
                       (cons 'cua-mode cua-mode))))
      (cond
       ((and (fboundp 'cua-mode)
             (vimpulse-visual-before (eq cua-mode t))
             (or (not cua-mode) (numberp arg)))
        (cua-mode 1))
       ((and (fboundp 'transient-mark-mode)
             (or (not transient-mark-mode) (numberp arg)))
        (transient-mark-mode 1))
       ((and (boundp 'zmacs-regions)
             (or (not zmacs-regions) (numberp arg)))
        (setq zmacs-regions t)))))))

(defun vimpulse-transient-restore ()
  "Restore Transient Mark mode to what is was before Visual mode.
 Also restores Cua mode."
  (when vimpulse-visual-vars-alist
    (when (boundp 'transient-mark-mode)
      (if (vimpulse-visual-before transient-mark-mode)
          (transient-mark-mode 1)
        (transient-mark-mode -1)))
    (when (boundp 'cua-mode)
      (if (vimpulse-visual-before cua-mode)
          (cua-mode 1)
        (cua-mode -1)))
    (when (boundp 'zmacs-regions)
      (let ((oldval (vimpulse-visual-before zmacs-regions)))
        (setq zmacs-regions oldval)))))

(defmacro vimpulse-visual-before (&rest body)
  "Evaluate BODY with original system values from before Visual mode.
This is based on `vimpulse-visual-vars-alist'."
  `(let ,(mapcar (lambda (elt)
                   `(,(car elt) (quote ,(cdr elt))))
                 vimpulse-visual-vars-alist)
     ,@body))

(defun vimpulse-visual-beginning (&optional mode force)
  "Return beginning of Visual selection.
See `vimpulse-visual-range'."
  (apply 'min (vimpulse-motion-range
               (vimpulse-visual-range mode force))))

(defun vimpulse-visual-end (&optional mode force)
  "Return end of Visual selection.
See `vimpulse-visual-range'."
  (apply 'max (vimpulse-motion-range
               (vimpulse-visual-range mode force))))

(defun vimpulse-visual-range (&optional mode force)
  "Return a Visual motion range (TYPE BEG END).
TYPE is the Visual mode.

The range depends on `point', `mark' and `vimpulse-visual-mode'.
The Visual mode may be specified explicitly with MODE, which must
be one of `normal', `line' and `block'.

In Normal mode, returns region plus one character.
In Line mode, returns region as whole lines.
In Block mode, returns rectangle plus one column.

If the Visual selection is already translated to Emacs' region,
returns the region as-is. This can be overridden with FORCE.

See also `vimpulse-visual-beginning' and `vimpulse-visual-end'."
  (let ((mark  (or (mark t) 1))
        (point (point)))
    (setq mode (or mode vimpulse-visual-mode))
    (unless (memq mode '(line block))
      (setq mode (if vimpulse-visual-mode 'inclusive 'exclusive)))
    (cond
     ((and (not force)
           (or (not vimpulse-visual-mode)
               vimpulse-visual-region-expanded))
      (list mode (min mark point) (max mark point)))
     ((eq 'block mode)
      (vimpulse-block-range mark point))
     ((eq 'line mode)
      (vimpulse-line-range mark point))
     (t
      (vimpulse-inclusive-range mark point)))))

(defun vimpulse-visual-select (beg end &optional widen)
  "Visually select text inclusively from BEG to END.
Return nil if selection is unchanged. If WIDEN is non-nil, only
modify selection if it does not already encompass BEG and END.

Under the hood, this function changes Emacs' `point' and `mark'.
The boundaries of the Visual selection are deduced from these and
the current Visual mode via `vimpulse-visual-beginning' and
`vimpulse-visual-end'."
  (cond
   ;; In Visual mode, protect the value of `mark-active'
   (vimpulse-visual-mode
    (let (mark-active)
      (vimpulse-set-region
       (min beg end)
       (if vimpulse-visual-region-expanded
           (max beg end)
         (max (min beg end) (1- (max beg end))))
       widen)))
   (t
    (vimpulse-set-region
     (min beg end) (max beg end) widen))))

(defun vimpulse-visual-expand-region
  (&optional mode no-trailing-newline)
  "Expand Emacs region to Visual selection.
If NO-TRAILING-NEWLINE is t and selection ends with a newline,
exclude that newline from the region."
  (let ((range (vimpulse-visual-range mode))
        mark-active)
    (when no-trailing-newline
      (save-excursion
        (goto-char (apply 'max (vimpulse-motion-range range)))
        (and (bolp) (not (bobp))
             (setq range
                   (list (vimpulse-motion-type range)
                         (apply 'min (vimpulse-motion-range range))
                         (max (apply 'min
                                     (vimpulse-motion-range range))
                              (1- (point))))))))
    (setq vimpulse-visual-region-expanded t)
    (vimpulse-mark-range range)))

(defun vimpulse-visual-contract-region (&optional keep-point)
  "Opposite of `vimpulse-visual-expand-region'.
I.e., the resulting Visual selection is equivalent to the former
Emacs region. If KEEP-POINT is t, does not move point.
Return nil if selection is unchanged."
  (let ((opoint (point)) (omark (mark t)))
    (setq vimpulse-visual-region-expanded nil)
    (vimpulse-visual-select (region-beginning) (region-end))
    (when keep-point (goto-char opoint))
    (not (and (= opoint (point))
              (= omark  (mark t))))))

(defun vimpulse-visual-restore ()
  "Restore previous selection.
This selects a specific range of text in the buffer.
See also `vimpulse-visual-reselect'."
  (interactive)
  (setq vimpulse-visual-region-expanded nil)
  (let ((last vimpulse-visual-last))
    (cond
     ;; If no previous selection, try a quick C-x C-x
     ((or (not vimpulse-visual-point)
          (not vimpulse-visual-mark))
      (vimpulse-activate-mark nil)
      (vimpulse-visual-mode 1))
     (t
      (unless vimpulse-visual-mode
        (cond
         ((eq 'line last)
          (vimpulse-visual-activate-line))
         ((eq 'block last)
          (vimpulse-visual-activate-block))
         (t                             ; normal
          (vimpulse-visual-activate-normal))))
      (set-mark vimpulse-visual-mark)
      (goto-char vimpulse-visual-point)
      (vimpulse-visual-contract-region)
      (vimpulse-visual-highlight)))))

(defun vimpulse-visual-reselect (&optional mode height width pos)
  "Create a Visual MODE selection of dimensions HEIGHT and WIDTH.
When called interactively, uses dimensions of previous selection.
If specified, selects about POS; otherwise selects about point.
See also `vimpulse-visual-restore'."
  (interactive)
  (when pos
    (goto-char pos))
  (setq mode (or mode vimpulse-visual-mode vimpulse-visual-last)
        height (or height vimpulse-visual-height 1)
        width (or width vimpulse-visual-width 1))
  (unless vimpulse-visual-mode
    (vimpulse-visual-activate mode))
  (cond
   ((eq 'block mode)
    (viper-next-line-carefully (1- height))
    (setq width (+ (1- width) (current-column)))
    (vimpulse-move-to-column width)
    (setq height (count-lines (vimpulse-visual-beginning mode)
                              (vimpulse-visual-end mode)))
    (while (and (not (eq width (current-column)))
                (< 1 height))
      (viper-next-line-carefully -1)
      (setq height (1- height))
      (move-to-column width)))
   ((eq 'line mode)
    (viper-next-line-carefully (1- height)))
   (t                                   ; normal
    (viper-forward-char-carefully (1- width)))))

(defun vimpulse-visual-markers (&optional point mark)
  "Refresh `vimpulse-visual-point' and `vimpulse-visual-mark'."
  (setq mark  (vimpulse-visual-beginning 'normal)
        point (vimpulse-visual-end 'normal))
  (when (< (point) (mark t))
    (setq mark (prog1 point
                 (setq point mark))))
  (viper-move-marker-locally 'vimpulse-visual-point point)
  (viper-move-marker-locally 'vimpulse-visual-mark  mark)
  (set-marker-insertion-type vimpulse-visual-point
                             (<= point mark))
  (set-marker-insertion-type vimpulse-visual-mark
                             (> point mark)))

(defun vimpulse-visual-dimensions (&optional beg end mode)
  "Refresh `vimpulse-visual-height' and `vimpulse-visual-width'."
  (vimpulse-visual-markers beg end)
  (setq mode (or mode vimpulse-visual-mode)
        beg (or beg (vimpulse-visual-beginning mode))
        end (or end (vimpulse-visual-end mode)))
  (cond
   ((eq 'block mode)
    (setq vimpulse-visual-height
          (count-lines beg
                       (save-excursion
                         (goto-char end)
                         (if (bolp)
                             (1+ end)
                           end)))
          vimpulse-visual-width (abs (- (save-excursion
                                          (goto-char end)
                                          (current-column))
                                        (save-excursion
                                          (goto-char beg)
                                          (current-column))))))
   ((eq 'line mode)
    (setq vimpulse-visual-height (count-lines beg end)
          vimpulse-visual-width nil))
   (t
    (setq vimpulse-visual-height nil
          vimpulse-visual-width (abs (- end beg))))))

(defun vimpulse-visual-highlight (&optional arg)
  "Highlight Visual selection, depending on region and Visual mode.
With negative ARG, removes highlighting."
  (cond
   ((and (numberp arg) (> 1 arg))
    (when (viper-overlay-live-p vimpulse-visual-overlay)
      (vimpulse-delete-overlay vimpulse-visual-overlay))
    (mapcar 'vimpulse-delete-overlay vimpulse-visual-block-overlays)
    (setq vimpulse-visual-block-overlays nil)
    ;; Clean up unreferenced overlays
    (dolist (overlay (vimpulse-overlays-at (point)))
      (when (eq (vimpulse-region-face) (viper-overlay-get overlay 'face))
        (vimpulse-delete-overlay overlay))))
   ((eq 'block vimpulse-visual-mode)
    ;; Remove any normal/line highlighting
    (when (viper-overlay-live-p vimpulse-visual-overlay)
      (vimpulse-delete-overlay vimpulse-visual-overlay))
    ;; Block highlighting isn't perfect
    (condition-case nil
        (vimpulse-visual-highlight-block
         (vimpulse-visual-beginning)
         (vimpulse-visual-end))
      (error nil)))
   (vimpulse-visual-mode                ; normal or line
    (let ((beg (vimpulse-visual-beginning))
          (end (vimpulse-visual-end)))
      ;; Remove any block highlighting
      (mapcar 'vimpulse-delete-overlay vimpulse-visual-block-overlays)
      (setq vimpulse-visual-block-overlays nil)
      ;; Reuse overlay if possible
      (if (viper-overlay-live-p vimpulse-visual-overlay)
          (viper-move-overlay vimpulse-visual-overlay beg end)
        (setq vimpulse-visual-overlay
              (vimpulse-make-overlay beg end nil t))
        (viper-overlay-put vimpulse-visual-overlay
                           'face (vimpulse-region-face))
        (viper-overlay-put vimpulse-visual-overlay
                           'priority 99))))))

(defun vimpulse-visual-highlight-block (beg end)
  "Highlight rectangular region from BEG to END.
We do this by putting an overlay on each line within the
rectangle. Each overlay extends across all the columns of the
rectangle. We try to reuse overlays where possible because this
is more efficient and results in less flicker.

Adapted from: `rm-highlight-rectangle' in rect-mark.el."
  (let ((opoint (point))                ; remember point
        (omark  (mark t))               ; remember mark
        (old vimpulse-visual-block-overlays)
        beg-col end-col new nlines overlay window-beg window-end)
    ;; Calculate the rectangular region represented by BEG and END,
    ;; but put BEG in the north-west corner and END in the south-east
    ;; corner if not already there
    (save-excursion
      (setq beg-col (save-excursion (goto-char beg)
                                    (current-column))
            end-col (save-excursion (goto-char end)
                                    (current-column)))
      (when (>= beg-col end-col)
        (if (= beg-col end-col)
            (setq end-col (1+ end-col))
          (setq beg-col (prog1 end-col
                          (setq end-col beg-col))))
        (setq beg (save-excursion (goto-char beg)
                                  (vimpulse-move-to-column beg-col)
                                  (point))
              end (save-excursion (goto-char end)
                                  (vimpulse-move-to-column end-col 1)
                                  (point))))
      ;; Force a redisplay so we can do reliable
      ;; windows BEG/END calculations
      (sit-for 0)
      (setq window-beg (max (window-start) beg)
            window-end (min (window-end) (1+ end))
            nlines (count-lines window-beg
                                (min window-end (point-max))))
      ;; Iterate over those lines of the rectangle which are
      ;; visible in the currently selected window
      (goto-char window-beg)
      (dotimes (i nlines)
        (let (row-beg row-end bstring astring)
          ;; Beginning of row
          (vimpulse-move-to-column beg-col)
          (when (> beg-col (current-column))
            ;; Prepend overlay with virtual spaces if we are unable to
            ;; move directly to the first column
            (setq bstring
                  (propertize
                   (make-string
                    (- beg-col (current-column)) ?\ )
                   'face
                   (or (get-text-property (1- (point)) 'face)
                       'default))))
          (setq row-beg (point))
          ;; End of row
          (vimpulse-move-to-column end-col)
          (when (> end-col (current-column))
            ;; Append overlay with virtual spaces if we are unable to
            ;; move directly to the last column
            (setq astring
                  (propertize
                   (make-string
                    (if (= row-beg (point))
                        (- end-col beg-col)
                      (- end-col (current-column)))
                    ?\ ) 'face (vimpulse-region-face)))
            ;; Place cursor on one of the virtual spaces
            ;; (only works in GNU Emacs)
            (if (= row-beg opoint)
                (put-text-property
                 0 (min (length astring) 1)
                 'cursor t astring)
              (put-text-property
               (max 0 (1- (length astring))) (length astring)
               'cursor t astring)))
          (setq row-end (min (point) (line-end-position)))
          ;; XEmacs bug: zero-length extents display
          ;; end-glyph before start-glyph
          (and (featurep 'xemacs)
               bstring astring
               (= row-beg row-end)
               (setq bstring (prog1 astring
                               (setq astring bstring))))
          ;; Trim old leading overlays
          (while (and old
                      (setq overlay (car old))
                      (< (viper-overlay-start overlay) row-beg)
                      (/= (viper-overlay-end overlay) row-end))
            (vimpulse-delete-overlay overlay)
            (setq old (cdr old)))
          ;; Reuse an overlay if possible, otherwise create one
          (cond
           ((and old (setq overlay (car old))
                 (or (= (viper-overlay-start overlay) row-beg)
                     (= (viper-overlay-end overlay) row-end)))
            (viper-move-overlay overlay row-beg row-end)
            (vimpulse-overlay-before-string overlay bstring)
            (vimpulse-overlay-after-string overlay astring)
            (setq new (cons overlay new)
                  old (cdr old)))
           (t
            (setq overlay (vimpulse-make-overlay row-beg row-end))
            (vimpulse-overlay-before-string overlay bstring)
            (vimpulse-overlay-after-string overlay astring)
            (viper-overlay-put overlay 'face (vimpulse-region-face))
            (viper-overlay-put overlay 'priority 99)
            (setq new (cons overlay new)))))
        (forward-line 1))
      ;; Trim old trailing overlays
      (mapcar 'vimpulse-delete-overlay old)
      (setq vimpulse-visual-block-overlays (nreverse new)))))

(defun vimpulse-visual-pre-command ()
  "Run before each command in Visual mode."
  (when vimpulse-visual-mode
    ;; Refresh Visual restore markers and marks
    (vimpulse-visual-dimensions)
    (cond
     ;; Movement command: don't expand region
     ((vimpulse-movement-cmd-p this-command)
      (setq vimpulse-visual-region-expanded nil))
     (t
      ;; Add whitespace if necessary for making a rectangle
      (and (eq 'block vimpulse-visual-mode)
           (vimpulse-visual-block-add-whitespace))
      (vimpulse-visual-expand-region
       ;; If in Line mode, don't include trailing newline
       ;; unless the command has real need of it
       nil (and (eq 'line vimpulse-visual-mode)
                (not (vimpulse-needs-newline-p this-command))))))))

(defun vimpulse-visual-post-command ()
  "Run after each command in Visual mode."
  (cond
   (vimpulse-visual-mode
    ;; Quitting: exit to vi (command) mode
    (cond
     (quit-flag                         ; C-g
      (vimpulse-visual-mode -1))
     ((eq 'keyboard-quit this-command)
      (vimpulse-visual-mode -1))
     ((and (not (region-active-p))
           (not (eq 'block vimpulse-visual-mode)))
      (vimpulse-visual-mode -1))
     ;; Region was expanded, so contract it
     (vimpulse-visual-region-expanded
      (when (eq 'block vimpulse-visual-mode)
        (vimpulse-visual-block-cleanup-whitespace))
      (if (eq 'line vimpulse-visual-mode)
          (vimpulse-visual-restore)
        (vimpulse-visual-contract-region))
      (vimpulse-visual-highlight))
     (t
      (vimpulse-visual-highlight))))
   ;; Not in the Visual state, but maybe the mark
   ;; was activated in vi (command) state?
   ((and (region-active-p)
         (eq 'vi-state viper-current-state)
         (if (boundp 'deactivate-mark) (not deactivate-mark) t))
    (vimpulse-visual-mode 1))))

(defun vimpulse-visual-deactivate-hook ()
  "Hook run when mark is deactivated in Visual mode."
  (when vimpulse-visual-mode
    (and (not (region-active-p))
         (not (vimpulse-movement-cmd-p this-command))
         (vimpulse-visual-mode -1))))

(add-hook 'pre-command-hook 'vimpulse-visual-pre-command)
(add-hook 'post-command-hook 'vimpulse-visual-post-command)
(if (featurep 'xemacs)
    (add-hook 'zmacs-deactivate-region-hook
              'vimpulse-visual-deactivate-hook)
  (add-hook 'deactivate-mark-hook 'vimpulse-visual-deactivate-hook))

;; Advise viper-intercept-ESC-key to exit Visual mode with ESC
(defadvice viper-intercept-ESC-key
  (around vimpulse-ESC-exit-visual-mode activate)
  "Exit Visual mode with ESC."
  (let ((viper-ESC-moves-cursor-back (not (region-active-p)))
        deactivate-mark)
    (if (and vimpulse-visual-mode
             (not (input-pending-p)))
        (vimpulse-visual-mode -1)
      ad-do-it)))

(defadvice viper-Put-back (around vimpulse-visual activate)
  "Delete selection before pasting in Visual mode."
  (let (inserted-text replaced-text mode)
    (setq yank-window-start (window-start))
    (cond
     (vimpulse-visual-mode
      (setq mode vimpulse-visual-mode)
      (unless (eq 'block mode)
        ;; Add replaced text to the kill-ring before the current kill
        (setq inserted-text (current-kill 0))
        (setq replaced-text
              (buffer-substring (region-beginning) (region-end)))
        (kill-new replaced-text t)
        (kill-new inserted-text))
      (vimpulse-delete (region-beginning) (region-end) t)
      (when (and (eq 'normal mode)
                 (not (bolp))
                 (viper-end-with-a-newline-p inserted-text))
        (newline))
      (when (and (eq 'line mode)
                 (not (viper-end-with-a-newline-p inserted-text)))
        (save-excursion (newline))))
     ((region-active-p)
      (delete-region (region-beginning) (region-end))))
    (if (and killed-rectangle
             kill-ring
             (eq (current-kill 0)
                 (get 'killed-rectangle 'previous-kill)))
        (save-excursion
          (yank-rectangle))
      ad-do-it)))

(defadvice viper-put-back (around vimpulse-visual activate)
  "Delete selection before pasting in Visual mode."
  (setq yank-window-start (window-start))
  (cond
   (vimpulse-visual-mode
    (viper-Put-back arg))
   ((region-active-p)
    (viper-Put-back arg))
   ((and killed-rectangle
         kill-ring
         (eq (current-kill 0)
             (get 'killed-rectangle 'previous-kill)))
    (unless (eolp)
      (viper-forward-char-carefully))
    (save-excursion
      (yank-rectangle)))
   (t
    ad-do-it)))

;; Viper's larger movement commands use the mark to store the previous
;; position, which is fine and useful when the mark isn't active. When
;; it is, however, it has the effect of remaking the region.
(defadvice push-mark (around vimpulse-visual-mode activate)
  (unless (and vimpulse-visual-mode
               ;; Note: if you really need to call `push-mark'
               ;; in proximity with these commands (e.g., in a hook),
               ;; do (let (this-command) (push-mark)).
               (memq this-command
                     '(vimpulse-goto-first-line
                       vimpulse-goto-line
                       viper-backward-paragraph
                       viper-backward-sentence
                       viper-forward-paragraph
                       viper-forward-sentence
                       viper-goto-line
                       viper-window-bottom
                       viper-window-middle
                       viper-window-top)))
    ad-do-it))

;; Block selection disables Transient Mark mode
(defadvice deactivate-mark (after vimpulse-visual activate)
  "Deactivate Visual Block mode."
  (when (eq 'block vimpulse-visual-mode)
    (vimpulse-visual-mode -1)))

(defmacro vimpulse-visual-mouse-advice (cmd)
  "Advise mouse command CMD to enable Visual mode."
  `(defadvice ,cmd (around vimpulse-visual activate)
     "Enable Visual mode in vi (command) state."
     (let ((w (posn-window (event-start (ad-get-arg 0)))))
       (cond
        ;; If Visual mode is enabled in the window clicked in,
        ;; adjust region afterwards
        ((with-selected-window w
           vimpulse-visual-mode)
         (vimpulse-visual-highlight -1)
         ad-do-it
         (when (eq w (selected-window))
           (vimpulse-visual-contract-region t)
           (vimpulse-visual-highlight)))
        ;; Otherwise, if in vi (command) state, enable Visual mode
        ((with-selected-window w
           (eq 'vi-state viper-current-state))
         ad-do-it
         (when (eq w (selected-window))
           (cond
            (vimpulse-visual-mode
             (vimpulse-visual-contract-region t))
            ((region-active-p)
             (vimpulse-visual-mode 1)
             (setq vimpulse-visual-region-expanded nil)
             (vimpulse-visual-contract-region t)))))
        (t
         ad-do-it)))))

(vimpulse-visual-mouse-advice mouse-drag-region)
(vimpulse-visual-mouse-advice mouse-save-then-kill)

(defadvice mouse-show-mark (before vimpulse-visual activate)
  "Refresh highlighting of Visual selection."
  (when vimpulse-visual-mode
    (vimpulse-visual-highlight)))

;;; Lists

(defvar vimpulse-movement-cmds
  '(backward-char backward-list backward-paragraph backward-sentence
    backward-sexp backward-up-list backward-word beginning-of-buffer
    beginning-of-defun beginning-of-line beginning-of-visual-line
    cua-cancel down-list end-of-buffer end-of-defun end-of-line
    end-of-visual-line exchange-point-and-mark forward-char
    forward-list forward-paragraph forward-sentence forward-sexp
    forward-word keyboard-quit mouse-drag-region mouse-save-then-kill
    mouse-set-point mouse-set-region move-beginning-of-line
    move-end-of-line next-line previous-line scroll-down scroll-up
    undo universal-argument up-list vimpulse-end-of-previous-word
    vimpulse-goto-definition vimpulse-goto-first-line
    vimpulse-goto-line vimpulse-visual-block-rotate
    vimpulse-visual-exchange-corners vimpulse-visual-reselect
    vimpulse-visual-restore vimpulse-visual-toggle-block
    vimpulse-visual-toggle-line vimpulse-visual-toggle-normal
    viper-backward-Word viper-backward-char viper-backward-paragraph
    viper-backward-sentence viper-backward-word
    viper-beginning-of-line viper-end-of-Word viper-end-of-word
    viper-exec-mapped-kbd-macro viper-find-char-backward
    viper-find-char-forward viper-forward-Word viper-forward-char
    viper-forward-paragraph viper-forward-sentence viper-forward-word
    viper-goto-char-backward viper-goto-char-forward viper-goto-eol
    viper-goto-line viper-insert viper-intercept-ESC-key
    viper-line-to-bottom viper-line-to-middle viper-line-to-top
    viper-next-line viper-paren-match viper-previous-line
    viper-search-Next viper-search-backward viper-search-forward
    viper-search-next viper-window-bottom viper-window-middle
    viper-window-top)
  "List of commands that move point.
If listed here, the region is not expanded to the
Visual selection before the command is executed.")

(defvar vimpulse-newline-cmds
  '(cua-copy-region cua-cut-region cua-delete-region delete-region
    exchange-point-and-mark execute-extended-command kill-region
    kill-ring-save viper-put-back viper-Put-back
    vimpulse-visual-exchange-corners)
  "Non-operator commands needing trailing newline in Visual Line mode.
In most cases, it's more useful not to include this newline in
the region acted on.")

(defun vimpulse-movement-cmd-p (command)
  "Whether COMMAND is a \"movement\" command.
That is, whether it is listed in `vimpulse-movement-cmds'."
  ;; We use `member' rather than `memq' to allow lambdas
  (member command vimpulse-movement-cmds))

(defun vimpulse-needs-newline-p (command)
  "Whether COMMAND needs trailing newline in Visual Line mode.
In most cases (say, when wrapping the selection in a skeleton),
it is more useful to exclude the last newline from the region."
  (or (member command vimpulse-newline-cmds)
      (vimpulse-operator-cmd-p command)))

;;; Ex

(defun vimpulse-visual-ex (arg)
  "Call `viper-ex' on region."
  (interactive "p")
  (viper-ex arg))

;;; Insert/append

(defun vimpulse-visual-insert (beg end &optional arg)
  "Enter Insert state at beginning of Visual selection."
  (interactive "r\nP")
  (let (deactivate-mark)
    (cond
     ((eq 'block vimpulse-visual-mode)
      (vimpulse-visual-block-rotate 'upper-left beg end)
      (setq beg (vimpulse-visual-beginning)
            end (vimpulse-visual-end))
      (vimpulse-visual-mode -1)
      (goto-char
       (vimpulse-visual-create-coords 'block ?i beg end))
      (viper-insert arg))
     (t
      (vimpulse-visual-mode -1)
      (push-mark end t t)
      (goto-char beg)
      (viper-insert arg))
     (t
      (error "Not in Visual mode")))))

(defun vimpulse-visual-append (beg end &optional arg)
  "Enter Insert state at end of Visual selection."
  (interactive "r\nP")
  (let (deactivate-mark)
    (cond
     ((eq 'block vimpulse-visual-mode)
      (vimpulse-visual-block-rotate 'upper-left beg end)
      (setq beg (vimpulse-visual-beginning)
            end (vimpulse-visual-end))
      (setq vimpulse-visual-whitespace-overlay nil)
      (vimpulse-visual-mode -1)
      (goto-char
       (vimpulse-visual-create-coords 'block ?a beg end))
      (viper-append arg))
     (t
      (vimpulse-visual-mode -1)
      (push-mark beg t t)
      (goto-char end)
      (viper-insert arg))
     (t
      (error "Not in Visual mode")))))

;;; Block selection

(defun vimpulse-apply-on-block (func &optional beg end &rest args)
  "Call FUNC for each line of Visual Block selection.
The selection may be specified explicitly with BEG and END.
FUNC must take at least two arguments, the beginning and end of
each line. Extra arguments to FUNC may be passed via ARGS."
  (let (beg-col end-col)
    (save-excursion
      (setq beg (or beg (vimpulse-visual-beginning))
            end (or end (vimpulse-visual-end)))
      ;; Ensure BEG < END
      (setq beg (prog1 (min beg end)
                  (setq end (max beg end))))
      ;; Calculate columns
      (goto-char end)
      (setq end-col (current-column))
      (goto-char beg)
      (setq beg-col (current-column))
      ;; Ensure BEG-COL < END-COL
      (when (< end-col beg-col)
        (setq beg-col (prog1 end-col
                        (setq end-col beg-col)))
        (setq end (save-excursion
                    (goto-char end)
                    (move-to-column end-col)
                    (point))))
      ;; Apply FUNC on each line
      (while (< (point) end)
        (apply func
               (save-excursion
                 (move-to-column beg-col)
                 (point))
               (save-excursion
                 (move-to-column end-col)
                 (point))
               args)
        (forward-line 1)))))

(defun vimpulse-visual-block-position (corner &optional beg end)
  "Return position of Visual Block CORNER.
CORNER may be one of `upper-left', `upper-right', `lower-left'
and `lower-right', or a clockwise number from 0 to 3:

        0---1        upper-left +---+ upper-right
        |   |                   |   |
        3---2        lower-left +---+ lower-right

The rectangle is defined by mark and point, or BEG and END
if specified. The CORNER values `upper', `left', `lower'
and `right' return one of the defining corners.

        upper P---+                    +---M upper
         left |   | lower        lower |   | right
              +---M right         left P---+

Corners 0 and 3 are returned by their left side, corners 1 and 2
by their right side. To place point in one of the corners, use
`vimpulse-visual-block-rotate'.

To go the other way, use `vimpulse-visual-block-corner'."
  (save-excursion
    (setq beg (or beg (vimpulse-visual-beginning 'block))
          end (or end (vimpulse-visual-end 'block)))
    (when (> beg end) (setq beg (prog1 end (setq end beg))))
    (let ((beg-col (progn (goto-char beg)
                          (current-column)))
          (end-col (progn (goto-char end)
                          (current-column)))
          (upper beg) (left beg) (lower end) (right end)
          (upper-left 0) (upper-right 1)
          (lower-left 3) (lower-right 2))
      (when (> beg-col end-col)
        (setq beg-col (prog1 end-col
                        (setq end-col beg-col)))
        (setq left (prog1 right
                     (setq right left))))
      (if (memq corner '(upper left lower right))
          (eval corner)
        (setq corner (mod (eval corner) 4))
        (if (memq corner '(0 1))
            (goto-char beg)
          (goto-char end))
        (if (memq corner '(0 3))
            (vimpulse-move-to-column beg-col)
          (vimpulse-move-to-column end-col))
        (point)))))

(defun vimpulse-visual-block-corner (&optional symbolic pos)
  "Return the current Visual Block corner as a number from 0 to 3.
Corners are numbered clockwise, starting with the upper-left corner.
Return as one of `upper-left', `upper-right', `lower-left' and
`lower-right' if SYMBOLIC is non-nil.

        0---1        upper-left +---+ upper-right
        |   |                   |   |
        3---2        lower-left +---+ lower-right

Specify POS to compare that position, rather than point,
against the corners. The result can be passed to functions
like `vimpulse-visual-block-position' and
`vimpulse-visual-block-rotate'."
  (let ((upper-left 0)
        (upper-right 1)
        (lower-left 3)
        (lower-right 2)
        corner)
    (setq pos (or pos (point)))
    (or (dolist (i '(upper-left lower-left) corner)
          (when (eq pos (vimpulse-visual-block-position i))
            (setq corner i)))
        (progn
          (unless vimpulse-visual-region-expanded
            (setq pos (1+ pos)))
          (dolist (i '(upper-right lower-right) corner)
            (when (eq pos (vimpulse-visual-block-position i))
              (setq corner i)))))
    (if symbolic
        corner
      (eval corner))))

(defun vimpulse-visual-block-rotate (corner &optional beg end)
  "In Visual Block selection, rotate point and mark clockwise.
When called non-interactively, CORNER specifies the corner to
place point in; mark is placed in the opposite corner.

        0---1        upper-left +---+ upper-right
        |   |                   |   |
        3---2        lower-left +---+ lower-right

Corners are numbered clockwise from 0. For better readability,
you may use the symbolic values `upper-left', `upper-right',
`lower-left' and `lower-right'.

This function updates `vimpulse-visual-point' and
`vimpulse-visual-mark' so that \\[vimpulse-visual-restore]
restores the selection with the same rotation."
  (interactive
   (list (if (> 0 (prefix-numeric-value current-prefix-arg))
             (1- (vimpulse-visual-block-corner))
           (1+ (vimpulse-visual-block-corner)))))
  (let ((upper-left 0) (upper-right 1) (lower-left 3) (lower-right 2)
        newmark newpoint newmark-marker newpoint-marker mark-active)
    (setq corner (mod (eval corner) 4))
    (setq newpoint (vimpulse-visual-block-position corner beg end))
    (setq newmark (vimpulse-visual-block-position
                   (mod (+ 2 corner) 4) beg end))
    (if (memq corner '(0 3))
        (setq newmark-marker (1- newmark)
              newpoint-marker newpoint)
      (setq newpoint-marker (1- newpoint)
            newmark-marker newmark))
    (unless vimpulse-visual-region-expanded
      (setq newpoint newpoint-marker
            newmark  newmark-marker))
    (set-mark newmark)
    (goto-char newpoint)
    (vimpulse-visual-dimensions beg end 'block)))

(defun vimpulse-visual-exchange-corners ()
  "Rearrange corners in Visual Block mode.

        M---+          +---M
        |   |    =>    |   |
        +---P          P---+

For example, if mark is in the upper left corner and point
in the lower right (see fig.), this function puts mark in
the upper right corner and point in the lower left."
  (interactive)
  (cond
   ((memq vimpulse-visual-mode '(normal line))
    (exchange-point-and-mark))
   ((eq 'block vimpulse-visual-mode)
    (let ((mark-col (save-excursion
                      (goto-char (mark t))
                      (forward-char)
                      (1- (current-column))))
          (point-col (current-column)))
      (set-mark (save-excursion
                  (goto-char (mark t))
                  (vimpulse-move-to-column
                   point-col (< (current-column) point-col))
                  (point)))
      (vimpulse-move-to-column
       mark-col (< (current-column) mark-col))
      (and (eolp) (not (bolp)) (backward-char))))
   (t
    (error "Not in Visual mode"))))

;; Insert whitespace into buffer to handle zero-width rectangles.
;; This isn't ideal and should be replaced with something else.
(defun vimpulse-visual-block-add-whitespace ()
  "Ensure rectangle is at least one column wide.
If the Block selection starts and ends on blank lines, the
resulting rectangle has width zero even if intermediate lines
contain characters. This function inserts a space after mark
so that a one-column rectangle can be made. The position of the
space is stored in `vimpulse-visual-whitespace-overlay' so it can be
removed afterwards with `vimpulse-visual-block-cleanup-whitespace'."
  (save-excursion
    (when (and (eq 'block vimpulse-visual-mode)
               (/= (vimpulse-visual-beginning)
                   (vimpulse-visual-end))
               (save-excursion
                 (goto-char (vimpulse-visual-beginning))
                 (and (bolp) (eolp)))
               (save-excursion
                 (goto-char (vimpulse-visual-end))
                 (and (bolp) (eolp))))
      (goto-char (mark t))
      (insert " ")
      (setq vimpulse-visual-whitespace-overlay
            (vimpulse-make-overlay (mark t) (1+ (mark t))
                                   nil t nil)))))

(defun vimpulse-visual-block-cleanup-whitespace ()
  "Clean up whitespace inserted by `vimpulse-visual-block-add-whitespace'."
  (when (viper-overlay-live-p vimpulse-visual-whitespace-overlay)
    (when (= 1 (- (viper-overlay-end
                   vimpulse-visual-whitespace-overlay)
                  (viper-overlay-start
                   vimpulse-visual-whitespace-overlay)))
      (delete-region
       (viper-overlay-start vimpulse-visual-whitespace-overlay)
       (viper-overlay-end   vimpulse-visual-whitespace-overlay)))
    (vimpulse-delete-overlay vimpulse-visual-whitespace-overlay)
    (setq vimpulse-visual-whitespace-overlay nil)))

(defun vimpulse-visual-create-coords
  (mode i-com upper-left lower-right)
  "Update the list of block insert coordinates with current rectangle.
I-COM should be ?c, ?i, ?a, ?I or ?A; the column for the
insertion will be chosen according to this command.
Returns the insertion point."
  (setq vimpulse-visual-insert-coords nil)
  (let ((nlines (count-lines upper-left lower-right))
        (col 0)) ; for ?I and ?A, trivial -- column is 0
    (when (or (eq i-com ?a) (eq i-com ?i) (eq i-com ?c))
      ;; For ?i and ?a, choose the left (the right) rectangle column
      (let ((beg-col (save-excursion
                       (goto-char upper-left)
                       (current-column)))
            (end-col (save-excursion
                       (goto-char lower-right)
                       (current-column))))
        ;; Decide if we use the left or right column
        (setq col (max 0 (if (or (eq i-com ?i) (eq i-com ?c))
                             beg-col
                           (1- end-col))))))
    ;; Save the information
    (setq vimpulse-visual-insert-coords
          (list mode i-com upper-left col nlines))
    (save-excursion
      (goto-char upper-left)
      (vimpulse-move-to-column col)
      (point))))

;; Redefinitions of Viper functions to handle Visual block selection,
;; that is, the "update all lines when we hit ESC" part.
;; This function is not in viper-functions-redefinitions.el
;; because its code is closely related to Visual mode.
(defun vimpulse-exit-insert-state ()
  (interactive)
  (viper-change-state-to-vi)
  (when vimpulse-visual-insert-coords
    ;; Get the saved info about the Visual selection
    (let ((mode   (nth 0 vimpulse-visual-insert-coords))
          (i-com  (nth 1 vimpulse-visual-insert-coords))
          (pos    (nth 2 vimpulse-visual-insert-coords))
          (col    (nth 3 vimpulse-visual-insert-coords))
          (nlines (nth 4 vimpulse-visual-insert-coords)))
      (goto-char pos)
      (save-excursion
        (dotimes (i (1- nlines))
          (forward-line 1)
          (let ((cur-col (vimpulse-move-to-column col)))
            ;; If we are in Block mode, this line, but do not hit the
            ;; correct column, we check if we should convert tabs
            ;; and/or append spaces
            (if (and (eq mode 'block)
                     (or (/= col cur-col) ; wrong column or
                         (eolp)))         ; end of line
                (cond ((< col cur-col)    ; we are inside a tab
                       (move-to-column (1+ col) t) ; convert to spaces
                       (move-to-column col t) ; this is needed for ?a
                       (viper-repeat nil))
                      ((and (>= col cur-col) ; we are behind the end
                            (eq i-com ?a))   ; and I-COM is ?a
                       (move-to-column (1+ col) t) ; append spaces
                       (viper-repeat nil)))
              (viper-repeat nil)))))
      (setq vimpulse-visual-insert-coords nil)))
  ;; Update undo-list
  (vimpulse-connect-undos))

(fset 'viper-exit-insert-state 'vimpulse-exit-insert-state)

;;; Key bindings

(define-key viper-vi-basic-map "v" 'vimpulse-visual-toggle-normal)
(define-key viper-vi-basic-map "V" 'vimpulse-visual-toggle-line)
(define-key viper-vi-basic-map "\C-v" 'vimpulse-visual-toggle-block)
(define-key viper-vi-basic-map "\C-p" 'yank-rectangle)
(define-key viper-vi-basic-map "gv" 'vimpulse-visual-restore)

(define-key vimpulse-visual-basic-map "v" 'vimpulse-visual-toggle-normal)
(define-key vimpulse-visual-basic-map "V" 'vimpulse-visual-toggle-line)
(define-key vimpulse-visual-basic-map "\C-v" 'vimpulse-visual-toggle-block)
(define-key vimpulse-visual-basic-map "x" 'vimpulse-delete)
(define-key vimpulse-visual-basic-map "D" 'vimpulse-delete)
(define-key vimpulse-visual-basic-map "Y" 'vimpulse-yank)
(define-key vimpulse-visual-basic-map "R" 'vimpulse-change)
(define-key vimpulse-visual-basic-map "C" 'vimpulse-change)
(define-key vimpulse-visual-basic-map "s" 'vimpulse-change)
(define-key vimpulse-visual-basic-map "S" 'vimpulse-change)
(define-key vimpulse-visual-basic-map "o" 'exchange-point-and-mark)
(define-key vimpulse-visual-basic-map "O" 'vimpulse-visual-exchange-corners)
(define-key vimpulse-visual-basic-map "I" 'vimpulse-visual-insert)
(define-key vimpulse-visual-basic-map "A" 'vimpulse-visual-append)
(define-key vimpulse-visual-basic-map "U" 'vimpulse-upcase)
(define-key vimpulse-visual-basic-map "u" 'vimpulse-downcase)
(define-key vimpulse-visual-basic-map ":" 'vimpulse-visual-ex)
;; Keys that have no effect in Visual mode
(vimpulse-visual-remap 'viper-repeat 'viper-nil)

(provide 'vimpulse-visual-mode)

;;;; This code integrates Viper with the outside world

;;; Add vi navigation to help buffers

;; Apropos
(defcustom vimpulse-want-vi-keys-in-apropos t
  "Whether to use vi keys in Apropos mode, on by default."
  :group 'vimpulse
  :type  'boolean)

(eval-after-load 'apropos
  '(when vimpulse-want-vi-keys-in-apropos
     (add-to-list 'viper-vi-state-mode-list 'apropos-mode)
     (let ((map apropos-mode-map))
       (vimpulse-add-core-movement-cmds map)
       (vimpulse-inhibit-destructive-cmds map)
       (viper-modify-major-mode 'apropos-mode 'vi-state map))))

;; Buffer-menu
(defcustom vimpulse-want-vi-keys-in-buffmenu t
  "Whether to use vi keys in Buffer menu, on by default."
  :group 'vimpulse
  :type  'boolean)

(eval-after-load "buff-menu"
  '(when vimpulse-want-vi-keys-in-buffmenu
     (setq viper-emacs-state-mode-list
           (delq 'Buffer-menu-mode viper-emacs-state-mode-list))
     (add-to-list 'viper-vi-state-mode-list 'Buffer-menu-mode)
     (let ((map Buffer-menu-mode-map))
       (vimpulse-add-core-movement-cmds map)
       (vimpulse-inhibit-destructive-cmds map)
       (viper-modify-major-mode 'Buffer-menu-mode 'vi-state map))))

;; Dired
(defcustom vimpulse-want-vi-keys-in-dired t
  "Whether to use vi keys in Dired mode, on by default."
  :group 'vimpulse
  :type  'boolean)

(eval-after-load 'dired
  '(when vimpulse-want-vi-keys-in-dired
     (setq viper-emacs-state-mode-list
           (delq 'dired-mode viper-emacs-state-mode-list))
     (add-to-list 'viper-vi-state-mode-list 'dired-mode)
     (let ((map dired-mode-map))
       (vimpulse-add-core-movement-cmds map)
       (vimpulse-inhibit-destructive-cmds map)
       (add-to-list 'ex-token-alist '("e" (epa-dired-do-encrypt)))
       (add-to-list 'ex-token-alist '("s" (epa-dired-do-sign)))
       (add-to-list 'ex-token-alist '("v" (epa-dired-do-verify)))
       (add-to-list 'ex-token-alist '("d" (epa-dired-do-decrypt)))
       (viper-modify-major-mode 'dired-mode 'vi-state map))))

;; Info
(defcustom vimpulse-want-vi-keys-in-Info t
  "Whether to use vi keys in Info mode, on by default."
  :group 'vimpulse
  :type  'boolean)

(eval-after-load 'info
  '(when vimpulse-want-vi-keys-in-Info
     (setq viper-emacs-state-mode-list
           (delq 'Info-mode viper-emacs-state-mode-list))
     (add-to-list 'viper-vi-state-mode-list 'Info-mode)
     (let ((map Info-mode-map))
       (vimpulse-add-core-movement-cmds map)
       (vimpulse-inhibit-destructive-cmds map)
       (define-key map "\C-t" 'Info-history-back) ; l
       (define-key map "\C-o" 'Info-history-back)
       (define-key map "\M-h" 'Info-help) ; h
       (define-key map " " 'Info-scroll-up)
       (define-key map "\C-]" 'Info-follow-nearest-node)
       (define-key map [backspace] 'Info-scroll-down)
       (viper-modify-major-mode 'Info-mode 'vi-state map))))

;; Help
(defcustom vimpulse-want-vi-keys-in-help t
  "Whether to use vi keys in Help mode, on by default."
  :group 'vimpulse
  :type  'boolean)

(eval-after-load 'help-mode
  '(when vimpulse-want-vi-keys-in-help
     (setq viper-emacs-state-mode-list
           (delq 'help-mode viper-emacs-state-mode-list))
     (add-to-list 'viper-vi-state-mode-list 'help-mode)
     (let ((map help-mode-map))
       (vimpulse-add-core-movement-cmds map)
       (vimpulse-inhibit-destructive-cmds map)
       (viper-modify-major-mode 'help-mode 'vi-state map))))

;;; ElDoc

(eval-after-load 'eldoc
  '(apply 'eldoc-add-command
          (append vimpulse-viper-movement-cmds
                  vimpulse-core-movement-cmds)))

;;; Folding

;; Almost all of this code is taken from extended-viper
;; coded by Brad Beveridge (bradbev at gmail.com)
;; - I changed the prefix of the custom functions to `vimpulse'
;;   to avoid multiple prefixes
(defcustom vimpulse-fold-level 0
  "Default fold level."
  :type  'boolean
  :group 'vimpulse)

(defun vimpulse-hs-Open ()
  (interactive)
  (hs-show-block)
  (hs-hide-level -1))

(eval-after-load 'hideshow
  '(add-hook 'hs-minor-mode-hook
             (lambda ()
               (call-interactively 'hs-hide-all)
               (define-key viper-vi-basic-map "za"
                 (lambda ()
                   (interactive)
                   (hs-toggle-hiding)
                   (hs-hide-level vimpulse-fold-level)))
               (define-key viper-vi-basic-map "za" 'hs-toggle-hiding)
               (define-key viper-vi-basic-map "zm" 'hs-hide-all)
               (define-key viper-vi-basic-map "zr" 'hs-show-all)
               (define-key viper-vi-basic-map "zo" 'hs-show-block)
               (define-key viper-vi-basic-map "zc" 'hs-hide-block))))

;; Load reveal.el if available
(unless (featurep 'reveal)
  (condition-case nil
      (require 'reveal)
    (error nil)))
(when (fboundp 'global-reveal-mode)
  (global-reveal-mode 1))

(provide 'vimpulse-compatibility)

;;;; Load Vimpulse components

(require 'vimpulse-dependencies)
(require 'vimpulse-viper-function-redefinitions)
(require 'vimpulse-utils)
(require 'vimpulse-misc-keybindings)
(require 'vimpulse-modal)
(require 'vimpulse-ex)
(require 'vimpulse-paren-matching)
(require 'vimpulse-operator)
(require 'vimpulse-text-object-system)
(require 'vimpulse-visual-mode)
(require 'vimpulse-compatibility)

(provide 'vimpulse)
