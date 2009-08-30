;;; vimpulse.el --- emulates Vim's most useful features, including Visual mode



;; ### For setup instructions, see "Installation" below. ###



;; Copyright (C) 2007 Brad Beveridge.
;; Copyright (C) 2007, 2009 Alessandro Piras.
;; Copyright (C) 2008 Frank Fischer.
;; Copyright (C) 2009 Jason Spiro.
;; 
;; Version: 0.3.0
;; Keywords: emulations
;; Human-Keywords: vim, visual-mode, rsi, ergonomics, Emacs pinky finger
;; Authors: Alessandro Piras and Brad Beveridge
;; Contact: <implementations-list at lists.ourproject.org>.
;;          You don't have to subscribe.  We usually reply within a
;;          few days and CC our replies back to you.  If we don't,
;;          check the archives:
;;          http://tinyurl.com/implementations-list
;; New bug tracker: http://my-trac.assembla.com/vimpulse/report/1
;; URL: http://emacswiki.org/emacs/vimpulse.el
;; Source control: http://www.assembla.com/spaces/vimpulse/trac_subversion_tool
;; License: GPLv2 or later, as described below under "License"
;; Compatibility: Works well with GNU Emacs 21.4 and 22.0.
;;                Causes problems with undo, but has no other problems, 
;;                on XEmacs 21.4.19.
;;                Please send us compatibility info about other versions.
;; Maintainer: Alessandro Piras but I am looking for a co-maintainer
;;             or for a new maintainer to take over.  Contact the list.
;; Thanks to our
;;        Old Maintainer: Jason Spiro
;; we'll miss you as a maintainer :)
;; 
;; This file is not part of GNU Emacs.

;;; Commentary:

;; Vimpulse emulates Vim's most useful features, including Visual
;; mode and (still partially) text objects.  
;; Vimpulse is a set of modifications to viper, the minor mode
;; that emulates Vi.  Vimpulse is not a minor mode; as soon as it is
;; loaded, viper will start working in a more Vim-like way.
;; 
;; Vimpulse is beta software.  It seems to work quite well already
;; though.  Patches and feature requests welcome.
;; 
;; NEWS 19/06/2009: The visual mode is greatly improved, we also 
;;                  feature an improved paren matching. It can
;;                  become slow (i didn't check, but it's likely)
;;                  if you work with very big files, so deactivate it 
;;                  if you notice sluggishness (see point 5 of the 
;;                  installation instructions). 

;;; Installation:

;; 1. Copy vimpulse.el to somewhere in your load-path, e.g. your
;;    site-lisp directory.
;; 
;; 2. Add the following block of code to your ~/.emacs file.  (If the
;;    file does not exist, create it.  If you use Windows, see
;;    http://www.gnu.org/software/emacs/windows/faq3.html#TOC33 to
;;    learn where to save the file.  If you use a Mac, email 
;;    implementations-list for help.)  Leave out the semicolons at the 
;;    beginning of each line.
;; 
;(setq viper-mode t)                ; enable Viper at load time
;(setq viper-ex-style-editing nil)  ; can backspace past start of insert / line
;(require 'viper)                   ; load Viper
;(setq vimpulse-experimental nil)   ; don't load bleeding edge code (see 6. installation instruction)
;(require 'vimpulse)                ; load Vimpulse
;(setq woman-use-own-frame nil)     ; don't create new frame for manpages
;(setq woman-use-topic-at-point t)  ; don't prompt upon K key (manpage display)
;; 
;; 3. (Optional) If you will be using C-r (the redo key) and you use
;;    GNU Emacs, also install http://wonderworks.com/download/redo.el
;;    and add (require 'redo) to your .emacs file.  (XEmacs and
;;    Aquamacs come with redo.el included.)
;; 
;; 4. (Optional) If you want a nice-looking block visual mode (i.e.
;;    rectangle selection), download and install
;;    http://emacswiki.org/elisp/download/rect-mark.el 
;;    in your load-path and add the Lisp expression (require
;;    'rect-mark) to your .emacs file. Block mode will work
;;    without rect-mark but you won't see the nice rectangle.
;; 
;; 5. (Optional) If you don't want to use the enhanced paren-matching 
;;    algorithm, add 
;;         (setq vimpulse-enhanced-paren-matching nil)
;;    to your .emacs _before_ (require 'vimpulse)
;; 
;; 6. (Optional) remove (setq vimpulse-experimental nil) from your .emacs 
;;    before loading vimpulse if you want to use "alpha" features.
;;    This may annoy your cat, kill your dog, make vimpulse suck
;;    more and whatever, but you'll be on the bleeding edge, and with your 
;;    feedback you'll help vimpulse get better.
;;    Experimental features are (and must be, if you want to contribute)
;;    marked with an ;;EXPERIMENTAL comment, and wrapped in a 
;;    (when vimpulse-experimental ... ) block. Please describe what
;;    they try to acheive. If your new pre-alpha feature is likely to
;;    break everything, define your own variable and wrap your definitions
;;    in a (when myexperimental-code ... ) block.
;;    To know what's experimental, try searching EXPERIMENTAL in this file.
;; 
;; We would love it if you sent an email to the maintainer saying
;; whether you liked or disliked vimpulse and why, and which
;; additional Vim features you would like implemented soonest.
;;; Usage:

;; The only feature of vimpulse with documentation available is visual
;; mode.
;; 
;; Visual mode:
;; 
;; To use visual mode, press v in normal mode.  Then use the motion
;; commands to select the region.  Then press d to delete, c to
;; change, r to replace, or y to copy.  You can use p to paste.  For
;; linewise visual mode, press V instead of v.  Then you can copy and
;; paste whole lines. If you want go to block visual mode, press C-v.
;; Here you can copy and paste the selected rectangle[*].
;; In block and linewise visual mode you may use I or A the insert or append
;; text before or after the selection in each selected line.
;;
;; [*] Old documentation follows: 
;; ", but you have ;; to use C-p. C-p is used because it was a simple 
;; way to add visual block mode in a way *close* to Vim without having 
;; to hack viper mode to use the normal 'p' key.  In the future, it 
;; would be nice to see vimpulse provide this the "right" way, but at 
;; this point I'm too inexperienced with elisp to make that happen."
;;      -I just made it work with the normal 'p' key, this is sort of 
;;       a hack, as it pushes a non printable string in the kill-ring
;;       and when pasting checks if the current-kill is `equal' to 
;;       the special string, and if it is pastes the rectangle instead.
;;
;; Other features:
;; 
;; This documentation is not written yet.  For now, see the definition
;; of viper-vi-global-user-map right near the beginning of the code.
;; You'll see a list of keys and what function each one calls.  The
;; documentation that comes with Vim -- which is also online at
;; http://vimdoc.sf.net -- may also be helpful.
;;
;; Tips:
;;
;; - Vimpulse makes C-r run "redo" in command mode but you can 
;;   still get reverse i-search by pressing C-s then C-r.


;;; Bugs:

;; (We would appreciate very much if you report bugs.)
;; 
;; Known bugs:
;; 
;; - (class of bugs) In visual or line visual mode, pressing things
;;   like C-g or C-SPC tends to confuse Vimpulse and do weird things.
;; 
;; - In visual mode, repeatedly pressing H, M, or L acts weirdly.  (I
;;   wonder if there are other keys that act weirdly in that mode too.)
;;      -in which way? it seems normal (at least with the current version
;;       19/06/2009
;; 
;; - One user who uses an ancient emacs-snapshot (from 2005) mentions
;;   that this mode causes all the keys on his keyboard to stop
;;   working unless he deletes the line that reads 'viper--key-maps
;;   from the macro my-get-emulation-keymap in this file.
;;         -Wasn't this already fixed? I think it is, as there's
;;          the conditional for the emacs version.
;; 
;; - cw with a count doesn't work the same as Vim when the point
;;   is just before a space between words
;;     - Fix plan: try cw with a count, then try dwi with a count; or,
;;       ask on a relevant forum how the commands differ; or, check
;;       how it works in Vi / Vim then check the Vim manual for more
;;       info; then, decide how to best fix.
;;         - Vim's behavior seems unconsistent in this case.
;;           I think making the fix optional would be best, as I (at least)
;;           like more vimpulse behavior in this case (defaulting to 
;;           standard vim behavior though)
;; 
;; - Undo has problems in XEmacs.

;;; Development and documentation TODOs:

;; - make sure I have added all stuff in Brad's viper additions and
;;   from my collection, then start documenting already.  Once there
;;   are even the simplest of docs (a nice key map) people will have a
;;   far easier time using vimpulse and so I bet more will contribute.
;; 
;; - the / key should allow isearch that works like Vim's, or until
;;   that's implemented, it should at least remap / to isearch-forward
;;   or viper-isearch-forward.  This should be an option that should
;;   be disabled by default.  For now, have viper load .vimrc and
;;   check for vim-specific option strings like "set incsearch".  If
;;   anyone complains, rethink that plan.
;; 
;; - Folding.  This should be implemented as a separate lisp library
;;   usable for even non-viper users.  Which foldmethods to do first?
;;   I personally only use foldmethod=marker, and even that only rarely.
;; 
;; - i_C-(I forgot the letter) should do (copy-from-above-command 1)
;;   from misc.el
;; 
;; - add advanced C-w commands; they can can use windmove.el
;;   (directional window-selection routines)
;; 
;; - add :set spell / :set nospell that uses flyspell-mode
;; 
;; - add support for tabs.el, a tabs mode that works sensibly (get it
;;   from Emacs Lisp List)
;;     - minimum needed: :tabedit, :tabnext, :tabprevious 
;;     - since I'm emulating Vim, emulate its tab pages feature.  So a
;;       tab page should be able to hold one or more buffers.
;; 
;; - add Customize option to let users stop C-r from being redo?
;; 
;; - email and try to get redo.el included with GNU Emacs (since I
;;   won't include redo.el here since nobody else includes it in their
;;   Lisp files either)
;; 
;; - copy more features from Brad's work in darcs and from vimpact
;;   into vimpulse
;; 
;; - doc: look in google chat log, find description of one-char-off
;;   bug, see if it applies to this or to the not-yet-released
;;   viper-x, and if to this, mention under Bugs
;; 
;; - doc: fix ref to "your home directory": Windows users don't have
;;   one
;; 
;; - doc: list all new keys (and maybe all differences from viper) in
;;   Usage section
;; 
;; - doc: describe all new keys in Usage section; can look at Vim
;;   manual for ideas
;; 
;; - modify how tramp works so it also automatically handles URLs
;;   typed in the netrw syntax, i.e. http:// etc.  But first ask tramp
;;   upstream if they could please make those changes themselves.
;; 
;; - add CTRL-O for jumping back in the jumplist and CTRL-I for
;;   jumping forwards (for undoing one CTRL-O).  I wonder if emacs'
;;   tags functionality allows a jumplist.  I wonder if viper does
;;   tags like nvi does.
;;     - The global mark ring is not what I want.
;;     - Try code.google.com/p/ejumplist/source/browse/trunk/jumplist.el
;; 
;; - on my PC (I run Ubuntu), if you start plain Vim then press CTRL-O
;;   many times, it starts opening recently opened files.  Is that
;;   useful?  Should vimpulse have persistent jump table functionality
;;   like that, and if so, should it use recentf or vim's .viminfo
;;   file or some tag functionality in emacs?  How will it interact
;;   with the fact that in emacs it's not traditional to suddenly
;;   close files without warning?
;; 
;; - make sentence movement work like in Vim.  I wonder if this can be
;;   done by setting viper options.
;;     - In Vim, according to :help sentence, end of sentence is:
;;         - '.', '?', or '!'
;;         - then (optionally) one or more '"', ''', ')', and ']'
;;           characters
;;         - then a newline, space, or tab.
;;         - A paragraph or section boundary is also a sentence
;;           boundary, but I bet viper handles that, and if it doesn't,
;;           it should.
;;             - A paragraph begins after each truly empty line (no
;;               whitespace chars on it) or after certain col-1 nroff
;;               macros.  A sentence begins after a form feed (^L), or
;;               certain nroff macros, in column 1.
;;             - The characters '{' and '}' sometimes affect paragraph
;;               definitions.  See :help paragraph.
;;     - In Viper, on the other hand, I bet sentences are like in vi,
;;       where Tabs aren't whitespace, and you need at least two spaces
;;       after the punctuation mark.
;; 
;; - try to get vimpulse included with upstream viper; also, ideally,
;;   if you pressed "v" in viper, viper would offer to load vimpulse.
;;   (likely to happen?  Consider that Michael Kifer, the viper
;;   maintainer, told me he doesn't need vim keys.  Then again, maybe
;;   I could convince him that it's worth it to ship vim keys, for
;;   other people's benefit.)
;; 
;; - email ridip <rdp@inthefaith.net> and ask him for his vimpulse
;;   contribs and his dvorak stuff
;; 
;; - email to Tromey for upload into ELPA?  we'd have to redo this
;;   when a new major version comes out.  Or maybe we should just
;;   contribute some auto-ELPA-management code.  By the way, should we
;;   move vimpulse into CVS somewhere?
;; 
;; - maybe merge all feature requests that anyone has ever sent into a
;;   "Feature requests" section here

;;; Development plans:

;; The design plan for Vimpulse is for it to only emulate features
;; that are in Vim.  Unfortunately, other new features do not belong
;; in Vimpulse unless you can get the Vim people to implement those
;; features too.

;;; Undecided development questions:

;; - In vimpulse, like in real vim, C-r only does redo in command
;;   mode; in insert mode it does something else.  (In vimpulse that
;;   "something else" is reverse i-search.)  Should it do reverse
;;   i-search in insert mode too?
;; 
;; - When you press "v" for visual mode, Vimpulse modifies the mode
;;   section of the modeline, so it reads e.g. "(Emacs-Lisp visual)".
;;   Shouldn't it do something to the <V> indicator instead?
;; 
;; - In Vim, when a line starts with a "// " or ";; " comment and I
;;   press enter, Vim extends the comment onto the next line.  What
;;   Vim function is it that does this?  Is the function enabled in
;;   plain vanilla Vim 7 as shipped by vim.org?  (Check by seeing how
;;   it works on Vim for Windows running on either Windows or Wine.)
;;   Is it mostly useful or mostly annoying?  Is it worth implementing
;;   in Emacs considering there are other easy ways to create
;;   comments?
;; 
;; - In v / V mode, Vim makes sure there is always at least 1 char /
;;   line selected.  IMO it provides nice feedback as to whether
;;   visual mode is on or not.  Is this worth implementing?  This is
;;   especially important for the block mode because currently it's
;;   impossible to select the last character in a line.
;;       19/06/2009: we only need to fix block mode.
;; 
;; - Sometimes when you use C (viper-change-to-eol) or other change
;;   commands, Jason's new viper-exec-change function shows a message
;;   like "Deleted 50 characters" as a side effect.  Is that annoying?
;;     - Update 1 month later:  I hardly notice the message.
;;     - Dear users:  Do you think I should disable the message?
;; 
;; - I want to allow buffer-switching without using the C-x key, since
;;   C-x b RET an extremely large amount of times per day is
;;   uncomfortable for my right pinky which presses RET.  There's
;;   already :b which seems to just invoke switch-to-buffer.  Is this
;;   right?  Is it bad if I make vimpulse emulate set autowrite=on
;;   then write new multi-buffer code?  What should the code's user
;;   interface be like?  I really should switch back to Vim for a day,
;;   learn more about how it deals with multiple buffers at once (and
;;   maybe also with tab pages) and emulate whatever of Vim's is most
;;   convenient.  What do you think of all the above?\
;;     - update: IIRC :set hidden lets you switch buffers w/o saving
;;     - update from Sebastien Rocca Serra: :set wildmenu plus 
;;       tab-completion makes :b very pleasant to use when you have 
;;       50+ buffers open.  Wildmenu is almost like iswitchb or ido.
;;     - I wonder how well that stuff works with just a few buffers open.
;; 
;; - simulate Vim's set virtualedit=onemore option to make C-x C-e
;;   possible w/o first advancing to next line?
;; 
;; - Would it be bad to edit users' .viminfo files without asking
;;   permission, or should some variable have to be customized on to do
;;   such a thing?
;; 
;; - should gj and gk do longlines-style movement like in Vim?  I
;;   really must resolve my Windows vs. Unix line-length hangups by
;;   Googling or asking before I even think about this.
;; 
;; - is there any need to implement Vim's new
;;   [count]dk-can-go-past-top-of-file-without-error functionality (to
;;   me, no need) or any related functionality?
;; 
;; - What to do about xemacs?  It doesn't ship with woman.  I wonder
;;   if woman is in some xemacs package?
;;; Change Log:
;; 
;; Version 0.3.0      
;;  [laynor@gmail.com:]
;;  - [NEW] register support on text object commands
;;  - [NEW] issuing : when visual selecting has a behavior closer to vim's
;;  [jasonspiro3@gmail.com:]
;;  - [FIX]: The Enter key now does what it should do -- insert a
;;    newline -- even when longlines-mode is on.
;;  - Comment changes.
;; 
;; Version 0.2.6.9
;;  [laynor@gmail.com:]
;; - [FIX & NEW] Text Objects support fixed and integrated with viper. Now
;;   count works (i.e. you can do 3caw and it works correctly), and it's
;;   possible to repeat the commands with ".".
;;   
;; Version 0.2.6.8
;;  [laynor@gmail.com:]
;; - [NEW]: Text object support: paren blocks, sentences, word, Words, quoted expressions,
;;   paragraphs. Delete and change commands.
;;   Example commands : diw ci( das etc.
;; - [FIX]: It's now possible to exit visual mode by pressing the ESC key or ^[
;;
;; Version 0.2.6.7
;;  [jasonspiro3@gmail.com:]
;;  - No code changes.
;;  - Fixed up "thanks" section below to mention Mieszko <sillyfox at yahoo.com>'s full
;;    name.  He wrote a small patch which was included long ago.  I must have 
;;    forgotten to include it in the changelog.
;;
;; Version 0.2.6.6
;;  [laynor@gmail.com:]
;; - Fixed pasting in visual mode, works like in vim now (Experimental, see point 6.
;;   installation instructions) 
;;
;; Version 0.2.6.5
;;  [laynor@gmail.com:]
;; - Fixed some major suckage with the change command. Still alpha, comments welcome,
;;   to use it see the installation instructions, point 6. (it's still experimental)
;; - Cleaned namespace, hope there are no hidden bugs
;; - Fixed loading on emacs snapshot
;;
;; Version 0.2.6.4
;;  [laynor@gmail.com:]
;;  This can probably be considered a major release.
;;  - [NEW and FIX] Rewritten visual mode, v and V variants (no changes to visual block still)
;;    It doesnot use the region like before: highlighting is done thru overlays,
;;    and the region is set inside the command code, before calling the viper 
;;    commands. = in visual mode calls vimpulse-visual-indent-command. The visual mode
;;    (apart form block mode) looks and feels like vim.
;;  - [NEW] Enhanced paren matching. Moving the cursor on a closing paren in normal mode 
;;    now highlights the opening paren. 
;;  - [NEW] Pressing RET in insert mode automatically indents the new line.
;;  - [NEW] ^[ works
;;  - [FIX] a<ESC> leaves the cursor in the same location as it was before (it advanced the 
;;    cursor 1 character before - viper-exit-insert-state's fault)
;;  - [FIX] cW doesn't suck anymore at the end of a line
;;
;; Version 0.2.6.3:
;;  [frank.fischer@s2001.tu-chemnitz.de:]
;;  - Support more visual-block-mode features: insert, append, delete, yank, change.
;;  - Change some vimpulse-functions and some viper-functions to handle
;;    block-mode properly.
;;  - Update documentation to reflect visual-block-mode.
;;  - The '=' command in visual-mode calls 'indent-region'.
;;
;; Version 0.2.6.2:
;;  [jasonspiro3@gmail.com:]
;;  - Improved XEmacs compatibility.
;;  - Small documentation improvements.
;; 
;; Version 0.2.6.1:
;;  [jasonspiro3@gmail.com:]
;;  - Removed duplicate definition of vimpulse-detect-mark-deactivate
;;    and duplicate add-hook call to add the hook.  I must have added
;;    the extra copies by accident when doing my last big merge; now
;;    they are gone.
;; 
;; Version 0.2.6.0:
;;  [jasonspiro3@gmail.com:]
;;  - Merged a patch for the function that powers * and #. Based on
;;    Ryoichi's patch and a cleaned-up version of Weihua's patch --
;;    thanks.  Now * and # will search for entire symbol at point,
;;    including underscores, not just word at point.
;;  - Todo addition.
;; 
;; Version 0.2.5.1:
;;  [jasonspiro3@gmail.com:]
;;  - Redefined viper-adjust-undo to do nothing.  This way, in
;;    insert mode, typing then moving the cursor then typing more
;;    counts as two separately undoable actions instead of one.
;;    Thanks to Weihua JIANG and to max_ from IRC #emacs for the idea.
;;  - Small extra TODO.
;; 
;; Version 0.2.5.0:
;;  [jasonspiro3@gmail.com:]
;;  - I've ignored my local changes for too long.  Here they are:
;;  - added keybindings from a Usenet post by Samuel Padgett
;;  - made change (cw, etc.) commands work more like Vim (my code)
;;  - I removed (setq ex-cycle-other-window nil); although it is very
;;    useful, it merely works around a problem with Viper.  I plan to
;;    discuss it with the Viper maintainer instead.
;;  - other changes and bugfixes from various people
;;  
;; Version 0.2.0.3:
;;  [jasonspiro3@gmail.com:]
;;  - Added Brad's viper-jump-to-tag-at-point
;; 
;; Version 0.2.0.2:
;;  [jasonspiro3@gmail.com:]
;;  - Small C-w keys and doc fixes.
;; 
;; Version 0.2.0.1:
;;  [cppjavaperl:]
;;  - Added support for block visual mode (i.e. rectangle selection).
;;  - Made C-p look for matches *prior* to the cursor, added C-n
;;    binding to look for matches *before* the cursor.  This works 
;;    more like Vim does.
;;  [jasonspiro3@gmail.com:]
;;  - Since vimpulse has no website, I added a prominent 
;;    pointer at the top to the installation instructions.
;; 
;; Version 0.2.0.0: Brad merged in several changes, including:
;;  - exit visual mode when the mark deactivates
;;  - changed the window manipulation to be global
;;  - added gf (goto file at point)
;;  - added \C-] and \C-t, tag jump & pop
;;  - added a helper function for defining keys
;;  - commented out show-paren-function, what is it meant to do?
;; 
;; Version 0.1.0.1: No code changes.  Small documentation changes,
;; including updates on moving-left bug.
;; 
;; Version 0.1: Initial release.

;;; Acknowledgements:

;; Thanks to <cppjavaperl@yahoo.com>, John <jn at ngedit.com>, Samuel
;; Padgett, Ryoichi Kanetaka <ryoichi.kanetaka at gmail.com>,
;; Mieszko <sillyfox at yahoo.com>, Stian S., Toby Cubitt, Wang Xin,
;; Weihua JIANG <weihua.jiang at gmail.com>, Frank Fischer
;; <frank.fischer@s2001.tu-chemnitz.de> and all the other people who
;; have sent in bug reports or feedback.  Also, thanks to Michael
;; Kifer and all those who have contributed to viper-mode.
;; 
;; We love patches.  Would you like to see your name here?  Please
;; send code and/or documentation patches to the maintainer.  Ideas,
;; comments, and test results are appreciated too.


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
(defvar vimpulse-experimental t
  "Define whether or not use experimental features. Turned on by default, so you will give feedback :P.")

;; Load advice.el.
(require 'advice)

;; Load redo.el if available.  Sadly we can't use APEL's require
;; function to get 'noerror functionality because GNU Emacs 21 doesn't
;; ship with APEL included.
(unless (featurep 'redo)
  (load "redo" 'noerror))

(provide 'vimpulse-dependencies)
;;;;
;;;; This file contains helper functions that 
;;;; a) Can be useful for the end user
;;;; b) Can be useful for the contributor, thus avoiding 
;;;;    duplication of functionalities.
;;;;


(defun vimpulse-is-whitespace (pos)
  "Returns true if the character at `pos' is whitespace, nil otherwhise"
  (equal (char-syntax (char-after pos)) 32))

;; Define a helper function that sets up the viper keys in a given map.
;; This function is useful for creating movement maps or altering existing
;; maps
(defun vimpulse-set-movement-keys-for-map (map)
  (define-key map "\C-d" 'viper-scroll-up)
  (define-key map "\C-u" 'viper-scroll-down)
  (define-key map "j" 'viper-next-line)
  (define-key map "k" 'viper-previous-line)
  (define-key map "l" 'viper-forward-char)
  (define-key map "h" 'viper-backward-char))

;; EXAMPLE, the following lines enable Vim style movement in help
;; and dired modes.
;; create a movement map and set the keys
;(setq vimpulse-movement-map (make-sparse-keymap))
;(vimpulse-set-movement-keys-for-map vimpulse-movement-map)
;(viper-modify-major-mode 'dired-mode 'emacs-state vimpulse-movement-map) 
;(viper-modify-major-mode 'help-mode 'emacs-state vimpulse-movement-map)

(defmacro vimpulse-region-command (function)
  "Commodity macro to convert emacs region commands to vimpulse 
visual selection commands. See the comments on the source for an 
example on how to use it."
  `(lambda ()
     (interactive)
     (,function (vimpulse-get-vs-start) (vimpulse-get-vs-end))
     (vimpulse-visual-mode nil)))

;;; The macro vimpulse-region-commands works with any emacs command that
;;; operates with a region and takes as arguments the beginning and end of
;;; the region. For example, the comment-region and uncomment-region commands:
;;;        (comment-region beg end &optional arg)
;;;       (uncomment-region beg end &optional arg)
;;; You can define new bindings for comment region and uncomment region as 
;;; easily as 
;;
;; (define-key viper-vi-global-user-map "\\\]" 
;;   (vimpulse-region-command comment-region))
;; (define-key viper-vi-global-user-map "\\," 
;;   (vimpulse-region-command uncomment-region))




(provide 'vimpulse-utils)
;;;;
;;;; Almost all of this code is taken from extended-viper 
;;;; coded by Brad Beveridge (bradbev@gmail.com)
;;;; - I changed the prefix of the custom functions to vimpulse 
;;;;   to avoid multiple prefixes
;;;;
(defvar vimpulse-fold-level 0)
(defun vimpulse-hs-Open ()
  (interactive)
  (hs-show-block)
  (hs-hide-level -1))
(when (boundp 'hs-minor-mode)
  (add-hook 'hs-minor-mode-hook (lambda () 
				 (call-interactively 'hs-hide-all)
				 (define-key viper-vi-global-user-map "za" '(lambda () (hs-toggle-hiding) (hs-hide-level h)))
				 (define-key viper-vi-global-user-map "zA"   'hs-toggle-hiding)
				 (define-key viper-vi-global-user-map "zM"   'hs-hide-all)
				 (define-key viper-vi-global-user-map "zR"   'hs-show-all)
				 (define-key viper-vi-global-user-map "zO" 'vimpulse-hs-Open)
				 (define-key viper-vi-global-user-map "zo"   'hs-show-block)
				 (define-key viper-vi-global-user-map "zc"   'hs-hide-block))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;; VISUAL MODE BINDINGS ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-key viper-vi-basic-map "v" 'vimpulse-visual-mode)
(define-key viper-vi-basic-map "V" 'vimpulse-visual-mode-linewise)

(define-key viper-vi-global-user-map "K"    'woman)
(define-key viper-vi-global-user-map "gf"   'find-file-at-point)
(define-key viper-vi-global-user-map "gg"   'vimpulse-goto-first-line) 
(define-key viper-vi-global-user-map "zb"   'viper-line-to-bottom)
(define-key viper-vi-global-user-map "zh"   'scroll-right)
(define-key viper-vi-global-user-map "zl"   'scroll-left)
(define-key viper-vi-global-user-map "zt"   'viper-line-to-top)
(define-key viper-vi-global-user-map "zz"   'viper-line-to-middle)
(define-key viper-vi-global-user-map "*"    'vimpulse-search-forward-for-symbol-at-point) 
(define-key viper-vi-global-user-map "#"    'vimpulse-search-backward-for-symbol-at-point) 
(define-key viper-vi-global-user-map " "    nil)
(define-key viper-vi-global-user-map "\C-]" 'vimpulse-jump-to-tag-at-point)
(define-key viper-vi-global-user-map "\C-t" 'pop-tag-mark)
;; Map undo and redo from XEmacs' redo.el
(define-key viper-vi-global-user-map "u"    'undo)
(define-key viper-vi-global-user-map "\C-r" 'redo)

; Window manipulation
(define-key viper-vi-global-user-map "\C-w" (make-sparse-keymap))
(define-key viper-vi-global-user-map "\C-w\C-w" 'vimpulse-cycle-windows)
(define-key viper-vi-global-user-map "\C-ww" 'vimpulse-cycle-windows)
(define-key viper-vi-global-user-map "\C-wo" 'delete-other-windows)
(define-key viper-vi-global-user-map "\C-wc" 'delete-window)
(define-key viper-vi-global-user-map "\C-ws" 'split-window-vertically)
(define-key viper-vi-global-user-map "\C-wS" 'split-window-vertically)

; Block Visual Mode keys
(define-key viper-vi-global-user-map "\C-p" 'yank-rectangle)
(define-key viper-vi-global-user-map "\C-v" 'vimpulse-visual-mode-block)

; Insert mode keys
; Vim-like completion keys
(define-key viper-insert-global-user-map "\C-p" 'dabbrev-expand)
(define-key viper-insert-global-user-map "\C-n" 'vimpulse-abbrev-expand-after)
;;(define-key viper-insert-global-user-map [backspace] 'backward-delete-char-untabify) ;vim doesn't do this!
(define-key viper-insert-global-user-map [delete] 'delete-char) ;; delete key
; make ^[ work
(define-key viper-insert-global-user-map (kbd "ESC") 'viper-exit-insert-state)

;;; My code (Alessandro)
(defun vimpulse-indent-lines (count)
  (save-excursion
    (dotimes (i count)
      (indent-according-to-mode)
      (next-line))))
;;; His code (Brad)
(defun vimpulse-goto-first-line ()
  "Send point to the start of the first line."
  (interactive)
  (viper-goto-line 1)) 

(defun vimpulse-cycle-windows ()
  "Cycle point to another window."
  (interactive) 
  (select-window (next-window)))

(defun vimpulse-search-for-symbol-at-point (whether-forward)
  "Search forwards or backwards for the symbol under point."
  (let ((symbol (concat "\\<" (thing-at-point 'symbol) "\\>")))
    (setq viper-s-string symbol)
    (setq viper-s-forward whether-forward)
    (viper-search symbol whether-forward 1)))

(defun vimpulse-search-forward-for-symbol-at-point ()
  (interactive)
  (vimpulse-search-for-symbol-at-point t))

(defun vimpulse-search-backward-for-symbol-at-point ()
  (interactive)
  (vimpulse-search-for-symbol-at-point nil))

(defun vimpulse-jump-to-tag-at-point ()
 (interactive)
 (let ((tag (thing-at-point 'word)))
   (find-tag tag)))

;;; cppjavaperl's code
(defun vimpulse-abbrev-expand-after ()
  (interactive)
  (dabbrev-expand -1))

(provide 'vimpulse-misc-keybindings)
;;; All this code is taken from Brad Beveridge's extended viper.
(defvar vimpulse-extra-ex-commands '(
      ("b" "buffer")
      ("bdelete" (vimpulse-kill-current-buffer))
      ("bnext" "next")
      ("syntax" (global-font-lock-mode))
      ("split" (split-window))
      ; Emacs and Vim use inverted naming conventions for splits.
      ("vsplit" (split-window-horizontally))
))
 

(defun vimpulse-kill-current-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer nil)) 


;;; Manipulation of Vipers functions by using the advice feature
;;; Many of the functions here rely as heavily on Viper's internals as Viper itself
;;; Additional Ex mode features.
;;; ex-token-alist is defined as a constant, but it appears I can safely push values to it!
(defadvice viper-ex (around vimpulse-extended-ex-commands (arg &optional string) activate)
  ad-do-it) 

(setq ex-token-alist (append vimpulse-extra-ex-commands ex-token-alist))


(provide 'vimpulse-ex)

;; This function replaces viper's original viper-exec-change function
;; which is invoked by key sequences starting with 'c'.  When the user
;; requests a command like 'cw', this function calls a sequence like
;; 'dwi' instead.  This stops viper from indicating the change
;; operation with distracting colored overlays and $ signs.  Instead,
;; it simply deletes the text then enters Insert mode, like Vim does.
;; 
;; The function works fine at eol and eob but TODO: understand the
;; original viper-exec-change command and see if mine does everything
;; it does.
(unless vimpulse-experimental
  (defun viper-exec-change (m-com com)
    (save-excursion ;; Added by Alessandro Piras, to fix cW suckage 
      (viper-exec-delete m-com com)) ;; on the last word of a line
    (if (eq m-com 'viper-goto-eol)
					; use viper-append here since vi's C (change to end of line)
					; command works differently than c
	(viper-append nil) 
      (viper-insert nil))))

;;EXPERIMENTAL: make the changecommand work like vim 
(when vimpulse-experimental
  (defun viper-exec-change (m-com com)
    (cond
     ((vimpulse-is-whitespace viper-com-point)            ;; check if the command has been issued on a whitespace
      (save-excursion (viper-exec-delete m-com com))      ;; deletes the stuff as in the old code
      (while (vimpulse-is-whitespace (point))             ;; eliminates all trailing whitespace like vim does
	(delete-char 1))                               
      (viper-insert nil))                                 
     (t                                                   ;; Old code executed in the other cases
      (save-excursion ;; Added by Alessandro Piras, to fix cW suckage 
	(viper-exec-delete m-com com)) ;; on the last word of a line
      (if (eq m-com 'viper-goto-eol) ; use viper-append here since vi's C (change to end of line)
					; command works differently than c
	  (viper-append nil) 
	(viper-insert nil)))))
  )

(when nil
  (defun viper-adjust-undo ()
    "This viper function has been redefined by vimpulse.el to
do nothing.  This way, in insert mode, typing then moving 
the cursor then typing more counts as two separately undoable 
actions instead of one."
    )
  )

  ;;
  ;; Thanks to the anonymous poster for the idea on how to modify the viper 
  ;; function to add the di da ci and ca partial commands.
  ;;


  ;; REDEFINITION OF VIPER FUNCTION
  ;;
  ;; `viper-prefix-arg-com', originally defined in viper-cmd.el, does
  ;; much of the work of reading keyboard input and chosing the
  ;; appropriate command. As an ugly way of getting basic "delete inner
  ;; parentheses" functionality, we extend it here with entries for our
  ;; custom `vimpulse-di' and `vimpulse-ci' functions (defined below).
  ;;
  ;; This should be done in a cleaner way. Michael Kifer gives some
  ;; hints in viper.el:
  ;;
  ;;     Some of the code that is inherited from VIP-3.5 is rather
  ;;     convoluted. Instead of viper-command-argument, keymaps should
  ;;     bind the actual commands. E.g., "dw" should be bound to a
  ;;     generic command viper-delete that will delete things based on
  ;;     the value of last-command-char. This would greatly simplify the
  ;;     logic and the code.
  ;;
  ;; For (some) brewity, Kifer's comments are removed. The added lines
  ;; are annotated with ";; MODIFICATION".

  (defun viper-prefix-arg-com (char value com)
    (let ((cont t)
	  cmd-info
	  cmd-to-exec-at-end)
      (while (and cont
		  (viper-memq-char char
				   (list ?q ?i ?a ?c ?d ?y ?! ?< ?> ?= ?# ?r ?R ?\" ;;modified ?i ?a ?q
					 viper-buffer-search-char)))
	(if com
	    ;; this means that we already have a command character, so we
	    ;; construct a com list and exit while.  however, if char is "
	    ;; it is an error.
	    (progn
	      ;; new com is (CHAR . OLDCOM)
	      (if (viper-memq-char char '(?# ?\")) (error "Viper bell"))
	      (setq com (cons char com))
	      (setq cont nil))
	  ;; If com is nil we set com as char, and read more.  Again, if char is
	  ;; ", we read the name of register and store it in viper-use-register.
	  ;; if char is !, =, or #, a complete com is formed so we exit the while
	  ;; loop.
	  (cond ((viper-memq-char char '(?! ?=))
		 (setq com char)
		 (setq char (read-char))
		 (setq cont nil))
		((viper= char ?#)
		 ;; read a char and encode it as com
		 (setq com (+ 128 (read-char)))
		 (setq char (read-char)))
		((viper= char ?\")
		 (let ((reg (read-char)))
		   (if (viper-valid-register reg)
		       (setq viper-use-register reg)
		     (error "giovanni"))
		   (setq char (read-char))))
		(t
		 (setq com char)
		 (setq char (read-char))))))

      (if (atom com)
	  ;; `com' is a single char, so we construct the command argument
	  ;; and if `char' is `?', we describe the arg; otherwise
	  ;; we prepare the command that will be executed at the end.
	  (progn
	    (setq cmd-info (cons value com))
	    (while (viper= char ?U)
	      (viper-describe-arg cmd-info)
	      (setq char (read-char)))
	    ;; `char' is a movement cmd, a digit arg cmd, or a register cmd---so
	    ;; we execute it at the very end
	    (or (viper-movement-command-p char)
		(viper-digit-command-p char)
		(viper-regsuffix-command-p char)
		(viper= char ?!) ; bang command
		(viper= char ?g) ; the gg command (like G0)
		(viper= char ?=) ; the == command
		(error "Viper bell"))
	    (setq cmd-to-exec-at-end
		  (viper-exec-form-in-vi
		   `(key-binding (char-to-string ,char)))))

	;; as com is non-nil, this means that we have a command to execute
	(if (viper-memq-char (car com) '(?r ?R))
	    ;; execute apropriate region command.
	    (let ((char (car com)) (com (cdr com)))
	      (setq prefix-arg (cons value com))
	      (if (viper= char ?r)
		  (viper-region prefix-arg)
		(viper-Region prefix-arg))
	      ;; reset prefix-arg
	      (setq prefix-arg nil))
	  ;; otherwise, reset prefix arg and call appropriate command
	  (setq value (if (null value) 1 value))
	  (setq prefix-arg nil)
	  (cond
	   ;; If we change ?C to ?c here, then cc will enter replacement mode
	   ;; rather than deleting lines.  However, it will affect 1 less line
	   ;; than normal.  We decided to not use replacement mode here and
	   ;; follow Vi, since replacement mode on n full lines can be achieved
	   ;; with nC.
	   ((equal com '(?q . ?d)) (vimpulse-test-function value))
	   ((equal com '(?a . ?d)) (vimpulse-delete-text-objects-command value ?a)) ; da<x>
	   ((equal com '(?a . ?c)) (vimpulse-change-text-objects-command value ?a)) ; ca<x>
	   ((equal com '(?a . ?y)) (vimpulse-yank-text-objects-command value ?a)) ; ya<x>
	   ((equal com '(?i . ?d)) (vimpulse-delete-text-objects-command value ?i)) ; di<x>
	   ((equal com '(?i . ?c)) (vimpulse-change-text-objects-command value ?i)) ; ci<x>
	   ((equal com '(?i . ?y)) (vimpulse-yank-text-objects-command value ?i)) ; yi<x>
	   ((equal com '(?c . ?c)) (viper-line (cons value ?C)))
	   ((equal com '(?d . ?d)) (viper-line (cons value ?D)))
	   ((equal com '(?d . ?y)) (viper-yank-defun))
	   ((equal com '(?y . ?y)) (viper-line (cons value ?Y)))
	   ((equal com '(?< . ?<)) (viper-line (cons value ?<)))
	   ((equal com '(?> . ?>)) (viper-line (cons value ?>)))
	   ((equal com '(?! . ?!)) (viper-line (cons value ?!)))
	   ((equal com '(?= . ?=)) (viper-line (cons value ?=)))
	   ;; gg  acts as G0
	   ((equal (car com) ?g)   (viper-goto-line 0))
	   (t (error "Viper bell")))))
      
      (if cmd-to-exec-at-end
	  (progn
	    (setq last-command-event
		  (viper-copy-event
		   (if (featurep 'xemacs) (character-to-event char) char)))
	    (condition-case err
		(funcall cmd-to-exec-at-end cmd-info)
	      (error
	       (error "%s" (error-message-string err))))))
      ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Redefining viper-ex to get a similar behavior to vim when ;;;
;;; issuing ":" when visual selecting.                        ;;;
;;; NOTE: this is a kludge.                                   ;;;
;;;       Vimpulse eats 'y and 'z marks to emulate vim's      ;;;
;;;       behavior instead of introducing '< and '>, because  ;;;
;;;       introducing them would introduce even more kludges  ;;;
;;;       like this one.                                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun viper-ex (arg &optional string)
  (interactive "P")
  (or string
      (setq ex-g-flag nil
	    ex-g-variant nil))
  (let* ((map (copy-keymap minibuffer-local-map))
	 (address nil)
	 (cont t)
	 (dot (point))
	 reg-beg-line reg-end-line
	 reg-beg reg-end
	 (initial-str (when (and vimpulse-visual-mode
				 (not vimpulse-visual-mode-block))
			"'y,'z"))
		      
	 prev-token-type com-str)
    (viper-add-keymap viper-ex-cmd-map map)

    (if arg
	(progn
	  (viper-enlarge-region (mark t) (point))
	  (if (> (point) (mark t))
	      (setq reg-beg (mark t)
		    reg-end (point))
	    (setq reg-end (mark t)
		  reg-beg (point)))
	  (save-excursion
	    (goto-char reg-beg)
	    (setq reg-beg-line (1+ (count-lines (point-min) (point)))
		  reg-end-line
		  (+ reg-beg-line (count-lines reg-beg reg-end) -1)))))
    (if reg-beg-line
	(setq initial-str (format "%d,%d" reg-beg-line reg-end-line)))

    (setq com-str
	  (if string
	      (concat initial-str string)
	    (viper-read-string-with-history
	     ":"
	     initial-str
	     'viper-ex-history
	     ;; no default when working on region
	     (if initial-str
		 nil
	       (car viper-ex-history))
	     map
	     (if initial-str
		 " [Type command to execute on current region]"))))
    (save-window-excursion
      ;; just a precaution
      (setq viper-ex-work-buf (get-buffer-create viper-ex-work-buf-name))
      (set-buffer viper-ex-work-buf)
      (delete-region (point-min) (point-max))
      (insert com-str "\n")
      (goto-char (point-min)))
    (setq ex-token-type nil
	  ex-addresses nil)
    (while cont
      (viper-get-ex-token)
      (cond ((memq ex-token-type '(command end-mark))
	     (if address (setq ex-addresses (cons address ex-addresses)))
	     (viper-deactivate-mark)
	     (let ((cmd (ex-cmd-assoc ex-token ex-token-alist)))
	       (if (null cmd)
		   (error "`%s': %s" ex-token viper-BadExCommand))
	       (ex-cmd-execute cmd)
	       (if (or (ex-cmd-is-mashed-with-args cmd)
		       (ex-cmd-is-one-letter cmd))
		   (setq cont nil)
		 (save-excursion
		   (save-window-excursion
		     (setq viper-ex-work-buf
			   (get-buffer-create viper-ex-work-buf-name))
		     (set-buffer viper-ex-work-buf)
		     (skip-chars-forward " \t")
		     (cond ((looking-at "|")
			    (forward-char 1))
			   ((looking-at "\n")
			    (setq cont nil))
			   (t (error
			       "`%s': %s" ex-token viper-SpuriousText)))
		     )))
	       ))
	    ((eq ex-token-type 'non-command)
	     (error "`%s': %s" ex-token viper-BadExCommand))
	    ((eq ex-token-type 'whole)
	     (setq address nil)
	     (setq ex-addresses
		   (if ex-addresses
		       (cons (point-max) ex-addresses)
		     (cons (point-max) (cons (point-min) ex-addresses)))))
	    ((eq ex-token-type 'comma)
	     (if (eq prev-token-type 'whole)
		 (setq address (point-min)))
	     (setq ex-addresses
		   (cons (if (null address) (point) address) ex-addresses)))
	    ((eq ex-token-type 'semi-colon)
	     (if (eq prev-token-type 'whole)
		 (setq address (point-min)))
	     (if address (setq dot address))
	     (setq ex-addresses
		   (cons (if (null address) (point) address) ex-addresses)))
	    (t (let ((ans (viper-get-ex-address-subr address dot)))
		 (if ans (setq address ans)))))
      (setq prev-token-type ex-token-type))))

(provide 'vimpulse-viper-function-redefinitions)
;;;;
;;;; This file contains an alternate paren matching function used when
;;;; viper is in vi mode, so that the paren under the cursor is matched, 
;;;; instead of the paren before the cursor. This visually makes checking 
;;;; parens at the end of the line possible.
;;;;

;;;
;;; paren matching LOADED BY DEFAULT
;;; set vimpulse-enhanced-paren-matching nil in your .emacs before requiring vimpulse to avoid loading it
;;;

;;; Begin Paren Matching Code {{{
(unless (boundp 'vimpulse-enhanced-paren-matching)  ;; Enhanced paren matching is enabled by default. To disable it 
  (setq vimpulse-enhanced-paren-matching t))       ;; just add (setq vimpulse-enhanced-paren-matching nil) to your .emacs

(when vimpulse-enhanced-paren-matching
  (require 'paren)
  (show-paren-mode 't) ;; enable the normal paren match highlight 
  
  (defvar vimpulse-paren-overlay-open nil) 		;;overlay used to highlight the opening paren
  (defvar vimpulse-paren-overlay-close nil)		;; overlay used to highlight the closing paren
  (make-variable-buffer-local 'vimpulse-paren-overlay-open) ;; overlays are buffer local.
  (make-variable-buffer-local 'vimpulse-paren-overlay-close)
  
  (defun vimpulse-pm-parenp (pos)
    (let ((class (syntax-class (syntax-after pos))))
      (or (= class 4) (= class 5))))
  (defun vimpulse-pm-open-parenp (pos)
    "Returns t if the character at position `pos' is an opening paren."
    (let ((class (syntax-class (syntax-after pos))))
      (= 4 class)))
  (defun vimpulse-pm-close-parenp (pos)
    "Returns t if the character at position `pos' is an closing paren."
    (let ((class (syntax-class (syntax-after pos))))
      (= 5 class)))

  (defun vimpulse-pm-get-candidate-pos (pos)
    "Returns the position of the possible matching paren of the character at position `pos'
if it's a paren, 'not-a-paren if it's not a paren, nil if no match is found."
    (let ((result nil))
      (condition-case ()
	  (cond 
	   ((vimpulse-pm-open-parenp pos)
	    (setq result (1- (scan-sexps pos 1))))
	   ((vimpulse-pm-close-parenp pos)
	    (setq result (scan-sexps (1+ pos) -1)))
	   (t 
	    (setq result 'not-a-paren)))
	(error (setq result nil)))
      result))
  
  (defun vimpulse-pm-is-real-match (pos1 pos2)
    "Checks the characters at position `pos1' and `pos2' and returns t if they are matching 
characters (in a paren match meaning), nil otherwise."
    (destructuring-bind ((class1 . match1) (class2 . match2)) 
	(list (syntax-after pos1) (syntax-after pos2))
      (or (eq match1 (char-after pos2))
	  (eq match2 (char-after pos1))
	  (eq match1 match2))))
  
  (defun vimpulse-pm-highlight-pos (pos face)
    "Highlights the paren at pos `pos' using `face'."
    (let ((ovl (if (vimpulse-pm-open-parenp pos) 
		   vimpulse-paren-overlay-open 
		 vimpulse-paren-overlay-close)))
      (overlay-put ovl 'face face)
      (move-overlay ovl pos (1+ pos))))
  
  (defun vimpulse-pm-show-paren ()
    "Paren matching routine. Highlights the paren at (point) and the eventual 
matching paren, or mismatched paren." ;;FIXME: this description sucks.
    (let ((candidate-pos (vimpulse-pm-get-candidate-pos (point))))
      (cond 
       ((not candidate-pos)
	(vimpulse-pm-highlight-pos (point) 'show-paren-mismatch))
       ((eq candidate-pos 'not-a-paren)
	(delete-overlay vimpulse-paren-overlay-open)
	(delete-overlay vimpulse-paren-overlay-close))
       (t
	(let ((pos-1 (vimpulse-pm-get-candidate-pos candidate-pos)))
	  (cond 
	   ((/= (point) pos-1)
	    (vimpulse-pm-highlight-pos (point) 'show-paren-mismatch))
	   ((vimpulse-pm-is-real-match candidate-pos pos-1)
	    (vimpulse-pm-highlight-pos (point) 'show-paren-match)
	    (vimpulse-pm-highlight-pos candidate-pos 'show-paren-match))
	   (t
	    (vimpulse-pm-highlight-pos (point) 'show-paren-mismatch)
	    (vimpulse-pm-highlight-pos candidate-pos 'show-paren-mismatch))))))))
  
  ;;;
  ;;; We advice show-paren-function and use it for insert mode
  ;;;
  ;;; TODO: check if using this paren matching function in replace mode is a problem 
  ;;;
  (defadvice show-paren-function (around vimpulse-parenmatching activate)
    (unless vimpulse-paren-overlay-open ;; define overlays if they don't exist
      (setq vimpulse-paren-overlay-open (make-overlay (point) (point)))
      (setq vimpulse-paren-overlay-close (make-overlay (point) (point)))
      (delete-overlay vimpulse-paren-overlay-open)
      (delete-overlay vimpulse-paren-overlay-close))
    (cond 
     ((and show-paren-mode viper-mode (not (eq viper-current-state 'insert-state))) ;; viper not in insert mode
      (when (boundp 'show-paren-overlay)                                            ;; we delete the overlays used by show-paren-function
	(delete-overlay show-paren-overlay)                                         ;; and call the custom paren-matching function
	(delete-overlay show-paren-overlay-1))
      (vimpulse-pm-show-paren))
     (t                                                 ;; viper in insert mode 
      (delete-overlay vimpulse-paren-overlay-open)      ;; delete the overlays used by the custom function
      (delete-overlay vimpulse-paren-overlay-close)
      ad-do-it)))                                       ;; call the adviced function
  
  )
;;; }}} End Paren Matching code

(provide 'vimpulse-paren-matching)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; EXPERIMENTAL    
;;;; Text Object Support:
;;;; This code implements the support for text objects, 
;;;; and implements commands like diw daw ciw caw.
;;;; It's still experimental, and not all that's supported in vim
;;;; is (still) supported here. However, the most common text objects 
;;;; are supported: 
;;;;    - paren blocks: { [ ( < > ) ] }
;;;;    - sentences
;;;;    - paragraphs
;;;;    - quoted expressions " and '
;;;;    - words and Words
;;;; Using text objects as motions in visual mode is (still) not supported.
;;;; Please note that Vimpulse's text objects are very close to Vim's, but
;;;; the behavior on certain occasions (e.g. daw issued with the cursor 
;;;; lying on whitespace) may be a little different. My aim was not acheiving 
;;;; the exact same behavior in all limit cases, but rather to give a close 
;;;; and consistent behavior to the commands.
;;;; Alessandro Piras
;;; Begin Text Objects code{{{

(when vimpulse-experimental
  (defun vimpulse-get-syntaxes-bounds (pos syntaxes)
    "Returns the bounds of contiguous character that match `syntaxes', 
where syntaxes is an emacs' syntax specification."
    (let ((result))
      (save-excursion
	(goto-char pos)
	(skip-syntax-forward syntaxes)
	(push (1- (point)) result)
	(skip-syntax-backward syntaxes)
	(cons (point) result))))

  (defvar vimpulse-paren-matching-table
    (make-hash-table)
    "Table used for paren matching:
table[key] = (match . opening-paren)"
    )
  (puthash ?\( 
	   '( ?\) . ?\( )
	   vimpulse-paren-matching-table)
  (puthash ?\) 
	   '( ?\( . ?\( )
	   vimpulse-paren-matching-table)
  (puthash ?{ 
	   '( ?} . ?\{ )
	   vimpulse-paren-matching-table)
  (puthash ?} 
	   '( ?{ . ?\{ ) 
	   vimpulse-paren-matching-table)
  (puthash ?\[ 
	   '( ?\] . ?\[)
	   vimpulse-paren-matching-table)
  (puthash ?\] 
	   '( ?\[ . ?\[ )
	   vimpulse-paren-matching-table)
  (puthash ?\< 
	   '( ?\> . ?\< )
	   vimpulse-paren-matching-table)
  (puthash ?\> 
	   '( ?\< . ?\< )
	   vimpulse-paren-matching-table)

  (defun vimpulse-skip-until-delimiters (pos paren match limb lime dir)
    "Skips all the character different from `paren' and `match' starting 
from `pos' following the direction `dir', with pos in [limb, lime]."
    (let ((pos-1 pos))
      (while (and (/= (char-after pos-1) paren)
		  (/= (char-after pos-1) match)
		  (or (and (= dir -1) (/= pos-1 limb)) ;; reached limits
		      (and (= dir 1) (/= pos-1 lime))))
	(setq pos-1 (+ dir pos-1)))
      pos-1))

  (defun vimpulse-find-first-unbalanced-1 (pos paren match limb lime dir)
    "Finds the first unbalanced `paren' following the direction `dir', starting 
from position `pos'. `match' is the paren that matches with `paren', limb is the 
lower bound of the position, lime is the upper bound to the position."
    (cond
     ((or (eq pos 'not-found))
      'not-found)
     ((= (char-after pos) paren) 
      pos)
     ((or (and (= dir -1) (= pos limb)) ;; reached limits
	  (and (= dir 1) (= pos lime)))
      'not-found)
     ((= (char-after pos) match) ;;
      (let ((pos-1 (vimpulse-find-first-unbalanced-1 (+ dir pos) paren match limb lime dir)))
	(vimpulse-find-first-unbalanced-1 (+ dir pos-1) paren match limb lime dir)))
     (t 
      (let ((pos-1 (vimpulse-skip-until-delimiters pos paren match limb lime dir)))
	(vimpulse-find-first-unbalanced-1 pos-1 paren match limb lime dir)))))

  (defvar vimpulse-balanced-bounds-char-list 
    (list 
     ?\( ?\) ?\[ ?\] ?\{ ?\} ?\< ?\>)
    "Parens supported by the text-object system.")

  (defun vimpulse-get-balanced-bounds (pos paren)
    "Returns the boundaries of a balanced expression."
    (let* ((limb (point-min))
	   (lime (1- (point-max)))
	   (paren-o (cdr (gethash paren vimpulse-paren-matching-table)))
	   (paren-c (car (gethash paren-o vimpulse-paren-matching-table)))
	   (pos-o (vimpulse-find-first-unbalanced-1 pos paren-o paren-c limb lime -1))
	   (pos-c (vimpulse-find-first-unbalanced-1 (if (integerp pos-o) (1+ pos-o) pos-o) paren-c paren-o limb lime 1)))
      (cond 
       ((eq pos-c 'not-found)
	nil)
       (t
	(list pos-o pos-c)))))

  (defun vimpulse-get-vword-bounds (pos)
    "Returns the boundaries of a word."
    (let ((syntaxes)
	  (syntax (char-syntax (char-after pos))))
      (cond
       ((= syntax ?\))
	(setq syntaxes (string syntax ?\()))
       ((= syntax ?\()
	(setq syntaxes (string syntax ?\))))
       (t 
	(setq syntaxes (string syntax))))
      (vimpulse-get-syntaxes-bounds pos syntaxes)))

  (defun vimpulse-get-vWord-bounds (pos)
    "Returns the boundaries of a Word."
    (let ((result))
      (save-excursion 
	(goto-char pos)
	(re-search-forward "[\n\r[:space:]]")
	(push (- (point) 2) result)
	(backward-char)
	(re-search-backward "[\n\r[:space:]]")
	(cons (1+ (point)) result))))

  (defun vimpulse-get-sentence-bounds (pos)
    "Returns the boundaries of a sentence."
    (let ((result))
      (save-excursion
	(goto-char pos)
	(when (not (posix-search-forward "\\(^\r\\|^\n\\|\\.[\n\r]?\\)" (point-max) t))
	  (goto-char (point-max)))
	(push (1- (point)) result)
	(backward-char (length (match-string 0)))
	(cond 
	 ((not (posix-search-backward "\\(^\r\\|^\n\\|\\.[\r\n]?\\)" (point-min) t))
	  (goto-char (point-min)))
	 (t 
	  (forward-char 1)))
	(posix-search-forward "\\(\n\\|\r\\|[[:space:]]\\)*" (point-max) t)
	(push (point) result))))

  (defun vimpulse-get-paragraph-bounds (pos)
    "Returns the boundaries of a paragraph."
    (let ((result))
      (save-excursion
	(goto-char pos)
	(when (not (re-search-forward "\\(^\r\\|^\n\\)" (point-max) t))
	  (goto-char (point-max)))
	(push (- (point) 2) result)
	(backward-char (length (match-string 0)))
	(cond 
	 ((not (re-search-backward "\\(^\r\\|^\n\\)" (point-min) t))
	  (goto-char (point-min)))
	 (t 
	  (forward-char 1)))
	(push (point) result))))

  (defun vimpulse-get-paired-bounds (pos char)
    "Returns the boundaries of a `char'-quoted expression."
    (save-excursion
      (goto-char pos)
      (if (= (char-before (point)) ?\\) (backward-char))
      (let ((result))
	(when (re-search-forward (concat "[^\\\\]" (string char)) (point-max) t)
	  (push (1- (point)) result)
	  (condition-case ()
	      (push (scan-sexps (point) -1) result)
	    (error (setq result nil))))
	result)))
  
  (defvar vimpulse-paired-expression-delimiters (list ?\" ?\')
    "Quotes supported by the text-object system.")

  (defun vimpulse-get-text-object-bounds-i (pos motion)
    "Returns the inner boundaries of a text object at point `pos'.
`motion' identifies the text object:
  - w -> word
  - W -> Word
  - s -> sentence
  - p -> paragraph
  - <paren> -> paren block (see variable `vimpulse-paren-matching-table'
               to see the supported parens.
  - <quote> -> quoted expression (see variable `paired-expression-delimiter'
               to see the type of quotes supported."
    (cond
     ((= motion ?w) (vimpulse-get-vword-bounds pos))
     ((= motion ?W) (vimpulse-get-vWord-bounds pos))
     ((= motion ?s) (vimpulse-get-sentence-bounds pos))
     ((= motion ?p) (vimpulse-get-paragraph-bounds pos))
     ((memq motion vimpulse-paired-expression-delimiters)
      (let ((bounds (vimpulse-get-paired-bounds pos motion)))
	(when bounds 
	    (destructuring-bind (s e) bounds
	      (list (1+ s) (1- e))))))
     ((memq motion vimpulse-balanced-bounds-char-list) 
      (let ((bounds (vimpulse-get-balanced-bounds pos motion)))
	(when bounds 
	    (destructuring-bind (s e) bounds
	      (list (1+ s) (1- e))))))
     (t nil)))

  (defun vimpulse-get-bounds-with-whitespace (func pos &optional trailing-newlines)
    "Given a function that returns inner boundaries, returns a boundary that includes 
the whitespace needed to get the \"a\" behavior. The logic 
followed is the same:
  - include all whitespace and newlines before the text object
  - include the text object
  - include trailing whitespace
  - if trailing-newlines is t, include also the trailing newlines"
    (save-excursion
      (goto-char pos)
      (let ((start (point))
	    (end nil))
	
	(skip-chars-forward "[:space:]\n\r")
	(let ((bounds (apply func  (list (point)))))
	  (cond
	   (bounds
	    (goto-char (1+ (cadr bounds)))
	    (skip-chars-forward (concat "[:space:]" (if trailing-newlines "\n\r" "")))
	    (list (min start (car bounds)) (1- (point))))
	   (t nil))))))

  
  (defun vimpulse-get-text-object-bounds-a (pos motion)
    "Returns the boundaries of `a' text object, whitespace to be killed included."
    (cond
     ((= motion ?w) 
      (vimpulse-get-bounds-with-whitespace 'vimpulse-get-vword-bounds pos))
     ((= motion ?W) (vimpulse-get-bounds-with-whitespace 'vimpulse-get-vWord-bounds pos))
     ((= motion ?s) (vimpulse-get-bounds-with-whitespace 'vimpulse-get-sentence-bounds pos))
     ((= motion ?p) (vimpulse-get-bounds-with-whitespace 'vimpulse-get-paragraph-bounds pos t))
     ((memq motion vimpulse-paired-expression-delimiters)
      (vimpulse-get-paired-bounds pos motion))
     ((memq motion vimpulse-balanced-bounds-char-list) 
      (vimpulse-get-balanced-bounds pos motion))
     (t nil)))
 
  (defun vimpulse-get-text-object-bounds (pos char motion)
    "Returns the boundaries of a text object. 'pos' indicates the start position,
char indicates 'inner' (?i) or 'a' (?a) behavior, 'motion' indicates the text-object."
    (cond 
     ((= char ?a) (vimpulse-get-text-object-bounds-a pos motion))
     ((= char ?i) (vimpulse-get-text-object-bounds-i pos motion))
     ((= char ?r) (list pos (+ pos (- (cadr motion) (car motion) 1))))
     ((= char ?l) (vimpulse-get-line-margins pos))
     (t (error "called with wrong arguments"))))
  (defun vimpulse-message-all-args (&rest args)
    "Helper function that prints all its arguments, plus some other values."
    (message "ARGS: %s, reg: %s" args (string viper-use-register)))
  (defun vimpulse-test-function (value)
    "This function is only defined for developing purposes."
    (viper-set-destructive-command (list 'vimpulse-message-all-args 'first-argument ?d viper-use-register "cane" nil)))
;;;;;;;;;;;;;;;;;;;;
;;;   Commands   ;;;
;;;;;;;;;;;;;;;;;;;;
  (defun vimpulse-unify-multiple-bounds (pos char count motion)
    "Returns the boundaries of a multiple text object motion. 
POS is the starting position,
CHAR indicates 'inner' or 'a' behavior,
COUNT indicates how many text objects to include,
MOTION indicates the kind of text object."
    (let* ((bounds-1 (vimpulse-get-text-object-bounds pos char motion))
	   (start (when bounds-1 (car bounds-1)))
	   (end (when bounds-1 (cadr bounds-1))))
      (dotimes (i (1- count))
	(setq end (cadr (vimpulse-get-text-object-bounds (1+ end) char motion))))
      (if end (list start end) nil)))

  (defun vimpulse-delete-text-objects-function (arg)
    "Deletes COUNT text objects of MOTION kind starting from `point', following the 
behavior indicated by CHAR: ?i stands for 'inner', ?a stands for 'a'. 
ARG has the form ((COUNT CHAR MOTION) . ?d)"
    (destructuring-bind (count char motion) (car arg)
      (let ((bounds (vimpulse-unify-multiple-bounds (point) char count motion)))
	(when bounds
	  (when viper-use-register ;; copy stuff to registers
	    ;; This code is take from viper-exec-delete
	    (cond 
	     ((viper-valid-register viper-use-register '(letter digit))
	      (copy-to-register
	       viper-use-register (car bounds) (1+ (cadr bounds)) nil))
	     ((viper-valid-register viper-use-register '(Letter))
	      (viper-append-to-register
	       (downcase viper-use-register) (car bounds) (1+ (cadr bounds))))
	     (t (setq viper-use-register nil)
		(error viper-InvalidRegister viper-use-register)))
	    (setq viper-use-register nil))
	  ;;end of viper-exec-delete code
	  (goto-char (car bounds))
	  (set-mark (1+ (cadr bounds)))
	  (call-interactively 'kill-region)))))

  (defun vimpulse-delete-text-objects-command (count char)
    "Deletes COUNT text objects following the behavior CHAR ('inner' or 'a').
The kind of text object is asked interactively to the user using `read-char'."
    (interactive)
    (let ((motion (read-char)))
      (viper-set-destructive-command (list 'vimpulse-delete-text-objects-function 
					   (list count char motion) ?d viper-use-register nil nil))
      (vimpulse-delete-text-objects-function (cons (list count char motion) ?d))))

  (defun vimpulse-change-text-objects-function (arg)
    "Executes `vimpulse-delete-text-objects-function' passing ARG to it and yanks the last insertion."
    (vimpulse-delete-text-objects-function arg)
    (viper-yank-last-insertion))

  (defun vimpulse-change-text-objects-command (count char)
    "Changes COUNT text objects following the behavior CHAR ('inner' or 'a').
The kind of text object is asked interactively to the user using `read-char'."
    (interactive)
    (let ((motion (read-char)))
      (viper-set-destructive-command (list 'vimpulse-change-text-objects-function (list count char motion) 
					   ?c viper-use-register nil nil))
      (vimpulse-delete-text-objects-function (cons (list count char motion) ?c))
      (viper-change-state-to-insert)))
  
  (defun vimpulse-yank-text-objects-function (arg)
    "Yanks COUNT text objects of MOTION kind starting from `point', following the 
behavior indicated by CHAR: ?i stands for 'inner', ?a stands for 'a'. 
ARG has the form ((COUNT CHAR MOTION) . ?d)"
    (destructuring-bind (count char motion) (car arg)
      (let ((bounds (vimpulse-unify-multiple-bounds (point) char count motion)))
	(when bounds
	  (when viper-use-register ;; copy stuff to registers
	    ;; This code is take from viper-exec-delete
	    (cond 
	     ((viper-valid-register viper-use-register '(letter digit))
	      (copy-to-register
	       viper-use-register (car bounds) (1+ (cadr bounds)) nil))
	     ((viper-valid-register viper-use-register '(Letter))
	      (viper-append-to-register
	       (downcase viper-use-register) (car bounds) (1+ (cadr bounds))))
	     (t (setq viper-use-register nil)
		(error viper-InvalidRegister viper-use-register)))
	    (setq viper-use-register nil))
	  ;;end of viper-exec-delete code
	  (copy-region-as-kill (car bounds) (1+ (cadr bounds)))
	  (goto-char (car bounds))))))

  (defun vimpulse-yank-text-objects-command (count char)
    "Yanks COUNT text objects following the behavior CHAR ('inner' or 'a').
The kind of text object is asked interactively to the user using `read-char'."
    (interactive)
    (let ((motion (read-char)))
      (vimpulse-yank-text-objects-function (cons (list count char motion) ?y))))
  )
;; This is for silencing viper when he checks if the insertion must be repeated, never true for 
;; this kind of commands.
(defvar vimpulse-text-objects-command (list 'vimpulse-delete-text-objects-function 
					    'vimpulse-change-text-objects-function 
					    'vimpulse-yank-text-objects-function))
(defadvice viper-repeat-insert-command (around vimpulse-text-objects-repeat-insert-command-fix activate)
  (when (not (memq (car viper-d-com) vimpulse-text-objects-command))
    ad-do-it))
;;; }}} End Text Objects code
(provide 'vimpulse-text-object-system)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; This file contains All the code relative to visual mode.  ;;;;;
;;;; Visual mode is implemented as a minor mode.               ;;;;; 
;;;; Currently, visual selection highlighting is done through  ;;;;;
;;;; the use of overlays for linewise and characterwise modes, ;;;;;
;;;; while for blockwise mode, rect-mark.el is needed.         ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Basic Minor Mode code ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; local variables
(eval-when-compile (require 'easy-mmode))
(defgroup vimpulse-visual nil
  "visual-mode for viper"
  :prefix "vimpulse-visual-"
  :group 'emulations)

 (define-minor-mode vimpulse-visual-mode
  "Toggles visual mode in viper"
  :lighter " visual"
  :initial-value nil
  :global nil
  :group 'vimpulse-visual)   
(defvar vimpulse-visual-mode-map (make-sparse-keymap)
  "Viper Visual mode keymap. This keymap is active when viper is in VISUAL mode")
(defvar vimpulse-visual-mode-linewise nil
  "If non nil visual mode will operate linewise")
(defvar vimpulse-visual-mode-block nil
  "If non nil visual mode will operate blockwise")
(defvar vimpulse-visual-current-register nil)
(defcustom vimpulse-visual-load-hook nil
  "Hooks to run after loading vimpulse-visual-mode."
  :type 'hook
  :group 'vimpulse-visual)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions related to visual selection activation,     ;;;
;;; mode of operation change (character-wise, block-wise, ;;;
;;; line-wise)                                            ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun vimpulse-visual-mode-ch-charwise ()
  "Starts Visual selection in character-wise mode or sets the current
mode of operation to character-wise if Visual selection is already started."
  (interactive)
  (cond
   (vimpulse-visual-mode-linewise
    (setq vimpulse-visual-mode-linewise nil))
   (t
    (vimpulse-visual-mode nil))))

(defun vimpulse-visual-mode-ch-linewise ()
  "Starts Visual selection in line-wise mode or sets the current
mode of operation to line-wise if Visual selection is already started."
  (interactive)
  (cond
   (vimpulse-visual-mode-block
    (vimpulse-visual-mode nil))
   ((not vimpulse-visual-mode-linewise)
    (setq vimpulse-visual-mode-linewise t))
   (t
    (vimpulse-visual-mode nil))))

(defun vimpulse-set-mark (pos)
  "Sets the region respecting the Emacsen-version, activates highlighting"
  (set-mark pos)
  (when (fboundp 'activate-region) 
    (activate-region))
  ;; Force transient-mark-mode to have visual selection 
  (when (fboundp 'transient-mark-mode)
    (transient-mark-mode t)))

(defun vimpulse-deactivate-mark ()
  "Deactivates the region respecting the Emacsen-version and type"
  (interactive)
  (if (and vimpulse-visual-mode-block (fboundp 'rm-deactivate-mark))
      (rm-deactivate-mark)
      (viper-deactivate-mark)))

;;;###auto-load
(defun vimpulse-visual-mode-toggle (&optional arg)
  (interactive "P")
  (make-local-variable 'vimpulse-visual-mode-linewise)
  (when (not vimpulse-visual-overlay)
      (setq vimpulse-visual-overlay (make-overlay (point) (point)))
      (delete-overlay vimpulse-visual-overlay)
      (overlay-put vimpulse-visual-overlay 'face (cons 'background-color "blue")))
  (unless vimpulse-visual-mode
    (setq vimpulse-visual-current-register nil)
    (vimpulse-deactivate-mark)
    (delete-overlay vimpulse-visual-overlay)
    (viper-change-state-to-vi))
  (when vimpulse-visual-mode
    (setq vimpulse-visual-mode-linewise nil)
    (setq vimpulse-visual-mode-block nil)
    (vimpulse-set-visual-overlay)))
    ;(vimpulse-set-mark (point))))

(defun vimpulse-visual-mode-linewise (&optional arg)
  "Starts viper visual mode in `linewise' mode"
  (interactive "P")
  (vimpulse-visual-mode 'toggle)
  (setq vimpulse-visual-mode-linewise t)
  (setq vimpulse-visual-mode-block nil)
  (vimpulse-set-visual-overlay))

(add-hook 'vimpulse-visual-mode-hook 'vimpulse-visual-mode-toggle t)
(run-hooks 'vimpulse-visual-load-hook)

(defun vimpulse-visual-mode-block (&optional arg)
  "Starts viper visual mode in `block' mode"
  (interactive "P")
  (vimpulse-visual-mode t)
  (setq vimpulse-visual-mode-block t)
  ;; perhaps a bad hack -> rm-set-mark deactivates the normal mark
  (when (fboundp 'rm-set-mark) 
    (rm-set-mark nil)))
;;;;;;;;;;;;;;;;;;;;
;;; Key bindings ;;;
;;;;;;;;;;;;;;;;;;;;
(define-key vimpulse-visual-mode-map "v" 'vimpulse-visual-mode-ch-charwise)
(define-key vimpulse-visual-mode-map "V" 'vimpulse-visual-mode-ch-linewise)
(define-key vimpulse-visual-mode-map "\C-v" 'vimpulse-visual-mode)
(define-key vimpulse-visual-mode-map "d" 'vimpulse-visual-delete-command)
(define-key vimpulse-visual-mode-map "x" 'vimpulse-visual-delete-command)
(define-key vimpulse-visual-mode-map "D" 'vimpulse-visual-delete-command)
(define-key vimpulse-visual-mode-map "d" 'vimpulse-visual-delete-command)
(define-key vimpulse-visual-mode-map "y" 'vimpulse-visual-yank-command)
(define-key vimpulse-visual-mode-map "u" 'vimpulse-visual-mode)
(define-key vimpulse-visual-mode-map "c" 'vimpulse-visual-change-command)
(define-key vimpulse-visual-mode-map "F" 'vimpulse-visual-change-command)
(define-key vimpulse-visual-mode-map "c" 'vimpulse-visual-change-command)
(define-key vimpulse-visual-mode-map "C" 'vimpulse-visual-change-command)
(define-key vimpulse-visual-mode-map "s" 'vimpulse-visual-change-command)
(define-key vimpulse-visual-mode-map "S" 'vimpulse-visual-change-command)
(define-key vimpulse-visual-mode-map "r" 'vimpulse-visual-replace-region)
(define-key vimpulse-visual-mode-map "\"" 'vimpulse-visual-set-current-register)
(define-key vimpulse-visual-mode-map "o" 'vimpulse-invert-origin-and-cursor)
(define-key vimpulse-visual-mode-map "O" 'vimpulse-invert-origin-and-cursor)
(define-key vimpulse-visual-mode-map "I" 'vimpulse-visual-insert)
(define-key vimpulse-visual-mode-map "A" 'vimpulse-visual-append)
(define-key vimpulse-visual-mode-map "=" 'vimpulse-visual-indent-command)
;; Keys that have no effect in visual mode
(define-key vimpulse-visual-mode-map "t" 'undefined)
(define-key vimpulse-visual-mode-map "." 'undefined)
(define-key vimpulse-visual-mode-map "T" 'undefined)
;; advice viper-intercept-ESC-key to exit visual mode with esc 
(defadvice viper-intercept-ESC-key (around vimpulse-esc-exit-visual-mode activate)
  (when vimpulse-visual-mode
    (vimpulse-visual-mode nil))
  ad-do-it)

;; this thing is just to silence the byte compiler
;; and stop it bugging about free variable
;; viper--key-maps in emacs 21 :)
;; update: and to stop emacs 23 bugging about the old macro
(defmacro vimpulse-add-visual-maps-macro(keymap)
  `(defadvice viper-normalize-minor-mode-map-alist (after vimpulse-add-visual-maps activate)
     "This function modifies minor-mode-map-alists to include the visual mode keymap"
     (push (cons 'vimpulse-visual-mode vimpulse-visual-mode-map) ,keymap)))

(cond
 ((>= emacs-major-version 22)
  (vimpulse-add-visual-maps-macro viper--key-maps))
 (t 
  (vimpulse-add-visual-maps-macro minor-mode-map-alist)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Visual selection visualization ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Overlay used to highlight the current visual selection
(defvar vimpulse-visual-overlay nil) ;; default value set in vimpulse-visual-mode-toggle
(make-variable-buffer-local 'vimpulse-visual-overlay)

;; vimpulse-visual-overlay-origin: stores the point location where the visual selection started
(defvar vimpulse-visual-overlay-origin nil)
(make-variable-buffer-local 'vimpulse-visual-overlay-origin)

;; functions used for linewise mode to access line margins
;; note: the structure returned by vimpulse-get-line-margins _can_ change to a more opaque one in the future. 
;; use the provided accessors.
(defun vimpulse-get-line-margins (&optional p)
  "Returns a structure containing the beginning-of-line and end-of-line 
markers of the line where the merker `p' resides. If `p' is nil, 
(point-marker) is used instead. The information can be retrieved using 
`vimpulse-get-bol' and `vimpulse-get-eol'."
  (save-excursion
    (if p (goto-char p)) 
    (list (point-at-bol) (point-at-eol))))
(defun vimpulse-get-bol (line-margins)
  "Retrieves the beginning-of-line marker from the structure returned by vimpulse-get-line-margins."
    (car line-margins))
(defun vimpulse-get-eol (line-margins)
  "Retrieves the end-of-line marker from the structure returned by vimpulse-get-line-margins."
    (cadr line-margins))

(defun vimpulse-set-visual-overlay ()
  (setq vimpulse-visual-overlay-origin (point-marker))
  (vimpulse-update-overlay))
      
(defun vimpulse-get-vs-bounds ()
  (list (overlay-start vimpulse-visual-overlay) (overlay-end vimpulse-visual-overlay)))
(defun vimpulse-get-vs-start ()
  (overlay-start vimpulse-visual-overlay))
(defun vimpulse-get-vs-end ()
  (overlay-end vimpulse-visual-overlay))

(defvar vimpulse-vs-start-marker (make-marker))
(defvar vimpulse-vs-end-marker (make-marker))

;; This is a kludge. It's intended to emulate vim's behavior when
;; issuing : when visual selecting. To implement the kludge, 
;; viper-ex is redefined, see viper-function-redefinitions.el
;; furthermore, this function has to be called from vimpulse-update-overlay.
(defun vimpulse-set-vs-registers ()
  (set-marker vimpulse-vs-start-marker (vimpulse-get-vs-start))
  (set-marker vimpulse-vs-end-marker (1- (vimpulse-get-vs-end)))
  (set-register (viper-int-to-char (1+ (- ?y ?a))) vimpulse-vs-start-marker)
  (set-register (viper-int-to-char (1+ (- ?z ?a))) vimpulse-vs-end-marker))

(defun vimpulse-update-visual-overlay-mode-normal ()
  (let ((pt (point)))
    (if (< pt vimpulse-visual-overlay-origin) 
        (move-overlay vimpulse-visual-overlay 
		      pt 
		      (+ 1 vimpulse-visual-overlay-origin))
      (move-overlay vimpulse-visual-overlay 
		    vimpulse-visual-overlay-origin 
		    (+ 1 pt)))))

(defun vimpulse-update-visual-overlay-mode-linewise ()
  (let ((pt (point-marker)))
    (let ((lm-point (vimpulse-get-line-margins pt))
          (lm-origin (vimpulse-get-line-margins vimpulse-visual-overlay-origin)))
      (cond 
       ((< pt vimpulse-visual-overlay-origin)
	(move-overlay vimpulse-visual-overlay 
		      (vimpulse-get-bol lm-point) 
		      (1+ (vimpulse-get-eol lm-origin))))
       (t
	(move-overlay vimpulse-visual-overlay 
		      (vimpulse-get-bol lm-origin) 
		      (1+ (vimpulse-get-eol lm-point))))))))

(defun vimpulse-update-overlay ()
  (save-excursion
    (cond 
     (vimpulse-visual-mode-linewise 
      (vimpulse-update-visual-overlay-mode-linewise))
     (t
      (vimpulse-update-visual-overlay-mode-normal)))
    (vimpulse-set-vs-registers)))
      
(add-hook 'post-command-hook '(lambda ()
				(if (and vimpulse-visual-mode 
					 (not vimpulse-visual-mode-block))
				    (vimpulse-update-overlay))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Destructive commands ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vimpulse-visual-delete-command ()
  "Deletes the visual selection"
  (interactive)
  (cond 
   (vimpulse-visual-mode-block
    (vimpulse-visual-mode nil)
    (kill-new "\02VimpulseVisualBlockMode\03")
    ;;(rm-kill-region (region-beginning) (region-end))
    (kill-rectangle (region-beginning) (region-end))
    (goto-char (region-beginning)))
   (t
    (goto-char (vimpulse-get-vs-start))
    (let ((count (if vimpulse-visual-mode-linewise (count-lines (vimpulse-get-vs-start) (vimpulse-get-vs-end)) 1))
	  (char (if vimpulse-visual-mode-linewise ?l ?r))
	  (motion (unless vimpulse-visual-mode-linewise (vimpulse-get-vs-bounds))))
      (viper-set-destructive-command (list 'vimpulse-delete-text-objects-function 
					   (list count char motion) ?d nil nil nil))
      (vimpulse-delete-text-objects-function (cons (list count char motion) ?d))))))
(defun vimpulse-visual-indent-command ()
  "Indents the visual selection."
  (interactive)
  (unless vimpulse-visual-mode-block
    (save-excursion
     (indent-region (vimpulse-get-vs-start) (vimpulse-get-vs-end))
     (vimpulse-visual-mode nil))))


(defun vimpulse-visual-change-command ()
  "Called when in visual (block) mode to delete the selected region and go to insert mode"
  (interactive)
  
  (cond 
   (vimpulse-visual-mode-block
    (let ((beg (region-beginning))
	  (end (region-end)))
      (kill-new "\02VimpulseVisualBlockMode\03")
      (vimpulse-create-coords ?c)
      (kill-rectangle beg end)
      (vimpulse-visual-mode nil) 
      (viper-insert nil)))
   (t
    (goto-char (vimpulse-get-vs-start))
    (let ((count (if vimpulse-visual-mode-linewise (count-lines (vimpulse-get-vs-start) (vimpulse-get-vs-end)) 1))
	  (char (if vimpulse-visual-mode-linewise ?l ?r))
	  (motion (unless vimpulse-visual-mode-linewise (vimpulse-get-vs-bounds))))
      (viper-set-destructive-command (list 'vimpulse-change-text-objects-function 
					   (list count char motion) ?d viper-use-register nil nil))
      (vimpulse-delete-text-objects-function (cons (list count char motion) ?c))
      (vimpulse-visual-mode nil)
      (when (= char ?l)
	(open-line 1))
      (viper-change-state-to-insert)))))

(defun vimpulse-replace-chars-in-selection-function (arg)
  "Fills the text in the region identified by COUNT, VISUAL-MODE and MOTION 
with the CHAR character, without replacing the newlines."
  (destructuring-bind (count visual-mode motion char) (car arg)
    (let ((bounds (vimpulse-unify-multiple-bounds (point) visual-mode count motion)))
      (when bounds
	(goto-char (car bounds))
	(save-excursion
	  (dotimes (i (1+ (- (cadr bounds) (car bounds))))
	    (unless (memq (char-after (point)) '(?\r ?\n))
	      (delete-char 1)
	      (insert char))))))))
	      
(defun vimpulse-visual-replace-region (&optional arg)
  (interactive)
  (cond
   ((not vimpulse-visual-mode-block)
    
    (let ((count (if vimpulse-visual-mode-linewise (count-lines (vimpulse-get-vs-start) (vimpulse-get-vs-end)) 1))
	  (visual-selection-mode (if vimpulse-visual-mode-linewise ?l ?r))
	  (motion (unless vimpulse-visual-mode-linewise (vimpulse-get-vs-bounds)))
	  (char (read-char))
	  (start (vimpulse-get-vs-start)))
      (viper-set-destructive-command (list 'vimpulse-replace-chars-in-selection-function
					   (list count visual-selection-mode motion char) ?r nil nil nil))
      (goto-char start) 
      (vimpulse-visual-mode nil)
      (vimpulse-replace-chars-in-selection-function (cons (list count visual-selection-mode motion char) ?r))))))
      
(defun vimpulse-visual-replace-region (&optional arg)
  (interactive "P")
  (goto-char (vimpulse-get-vs-start))
  (vimpulse-set-mark (vimpulse-get-vs-end))
  (vimpulse-visual-mode 'toggle)
  (cond
   ((= (mark) (point)) nil)
   (t 
    (if (< (mark) (point)) (exchange-point-and-mark))
    (viper-replace-char arg)		
    (let ((c (char-after (point))))
      (dotimes (i (- (mark) (point)))
	(cond
	 ((member (char-after (point)) '(?\r ?\n))
	  (forward-char))
	  (t (delete-char 1)
	     (insert c))))))))
(defun vimpulse-visual-replace-region (&optional arg)
  (interactive "P")
  (goto-char (vimpulse-get-vs-start))
  (vimpulse-set-mark (vimpulse-get-vs-end))
  (vimpulse-visual-mode 'toggle)
  (cond
   ((= (mark) (point)) nil)
   (t 
    (if (< (mark) (point)) (exchange-point-and-mark))
    (viper-replace-char arg)		
    (let ((c (char-after (point))))
      (dotimes (i (- (mark) (point)))
	(cond
	 ((member (char-after (point)) '(?\r ?\n))
	  (forward-char))
	  (t (delete-char 1)
	     (insert c))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Intermediate  commands  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun vimpulse-visual-set-current-register ()
  (interactive)
  (setq viper-use-register (read-char)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Non destructive commands ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vimpulse-visual-yank-command ()
  "Saves the visual selection in the kill-ring"
  (interactive)
  (cond 
   (vimpulse-visual-mode-block
    (kill-new "\02VimpulseVisualBlockMode\03")
    (vimpulse-visual-mode nil)
    ;;(rm-kill-ring-save (region-beginning) (region-end))
    (kill-rectangle (region-beginning) (region-end))
    (goto-char (region-beginning))
    (yank-rectangle)
    (goto-char (region-beginning)))
   (t
    (goto-char (vimpulse-get-vs-start))
    (let ((count (if vimpulse-visual-mode-linewise (count-lines (vimpulse-get-vs-start) (vimpulse-get-vs-end)) 1))
	  (char (if vimpulse-visual-mode-linewise ?l ?r))
	  (motion (unless vimpulse-visual-mode-linewise (vimpulse-get-vs-bounds))))
      (vimpulse-yank-text-objects-function (cons (list count char motion) ?y))
      (vimpulse-visual-mode nil)))))

(defun vimpulse-invert-origin-and-cursor ()
  (interactive)
  (cond
   (vimpulse-visual-mode-block
    (exchange-point-and-mark))
   (t
    (let ((origin vimpulse-visual-overlay-origin))
      (setq vimpulse-visual-overlay-origin (point))
      (goto-char origin)))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Visual Block Mode Support ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This variable holds the point and column of the first line
;; as well as the number of lines in the region.
(defvar vimpulse-visual-insert-coords nil
  "A list with (i-com ul-pos col nlines), where
`i-com' is the insert command (?i, ?a, ?I or ?A)
`ul-pos' is the position of the upper left corner of the region
`col' is the column of insertion
`nlines' is the number of lines in the region")

;; Modified by Alessandro Piras
(defun vimpulse-create-coords (i-com)
  "Updates the list of block insert coordinates with the current rectangle.
`i-com' should be ?c, ?i, ?a, ?I or ?A, the column for the insertion will be
chosen according to this command."
  ;; New visual mode handling: instead of using (region-beginning) and 
  ;; (region-end) we use rb and re, bound to the right bounds:
  ;; overlay bounds in normal and linewise mode, region bounds in block mode.
  (destructuring-bind (rb re) (if vimpulse-visual-mode-block 
				  (list (region-beginning) (region-end))
				(list (vimpulse-get-vs-start)
				      (vimpulse-get-vs-end)))
    (make-local-variable 'vimpulse-visual-insert-coords)
    (setq vimpulse-visual-insert-coords nil)
  
    (let ((nlines (count-lines re rb))
	  (col 0))		  ; For ?I and ?A trivial: column is 0
      (when (or (eq i-com ?a) (eq i-com ?i) (eq i-com ?c))
	;; for ?i and ?a chose the left (the right) column of the rectangle
	(let ((start-col (save-excursion 
			   (goto-char rb)
			   (current-column)))
	      (end-col (save-excursion
			 (goto-char re)
			 (current-column))))
	  ;; decide if we use the left or the right column
	  (setq col (max 0 (if (or (eq i-com ?i) (eq i-com ?c))
			       (min start-col end-col)
			     (1- (max start-col end-col)))))))
    
      ;; Ok we have all information, so go to the insert-point ...
      (goto-char rb) 
      (move-to-column col)
      ;; ... and save the information
      (setq vimpulse-visual-insert-coords 
	    (list i-com (point) col nlines)))))


;; FIXME This function or the follwing one is bug ridden
(defun connect-undos (n undo-list)
  "Connects the last n undo steps in undo-list to one step"
  (when (and (listp undo-list) 
	     (listp (cdr undo-list)) 
	     (> n 1))
    (if (null (cadr undo-list))
	(progn 
	  (setcdr undo-list (cddr undo-list))
	  (connect-undos (1- n) undo-list))
        (connect-undos n (cdr undo-list)))))

;; __Redefinitions of viper functions to handle visual block-mode__
;; This function is not in viper-functions-redefinitions.el 
;; because its code is closely related to visual mode.
(defun viper-exit-insert-state ()
  (interactive)
  (viper-change-state-to-vi)
  (when vimpulse-visual-insert-coords
    ;; Get the saved info about the visual region
    (let ((i-com (car vimpulse-visual-insert-coords))
	  (pos (cadr vimpulse-visual-insert-coords))
	  (col (caddr vimpulse-visual-insert-coords))
	  (nlines (cadddr vimpulse-visual-insert-coords)))
      (goto-char pos)
      (save-excursion
	(dotimes (i (1- nlines))
	  (forward-line 1)
	  (let ((cur-col (move-to-column col)))
	    ;; If we are in block mode this line but do not hit the correct 
            ;; column, we check if we should convert tabs and/or append spaces
	    (if (and vimpulse-visual-mode-block
		     (or (/= col cur-col) ;; wrong column or 
			 (eolp)))         ;; end of line 
		(cond ((< col cur-col)	  ;; we are inside a tab 
		       (move-to-column (1+ col) 'fill) ;; -> convert to spaces
		       (move-to-column col 'fill) ;; this is needed for ?a
		       (viper-repeat nil))
		      ((and (>= col cur-col) ;; we are behind the end
			    (eq i-com ?a))   ;; and i-com is ?a
		       (move-to-column (1+ col) t) ;; -> append spaces
		       (viper-repeat nil)))
		    
	      (viper-repeat nil)))))
      (setq vimpulse-visual-insert-coords nil)
    
      ;; update the last two undos
      (if (> nlines 1)
	  (if (eq i-com ?c)
	      (connect-undos 3 buffer-undo-list) ; delete, insert, repeat
	    (connect-undos 2 buffer-undo-list))	 ; insert, repeat
	(if (eq i-com ?c)
	    (connect-undos 2 buffer-undo-list)	 ; delete, insert
	  (connect-undos 1 buffer-undo-list))))) ; insert
  (if (and (/= (char-before (point)) ?\r) 
	   (/= (char-before (point)) ?\n))
      (backward-char 1)))               ; <---------- a[ESC] leaves the cursor 
					; where it was before in VIM, without 
					; backward-char it advances 1 character.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Visual block hack:                              ;;
;; the yank command in visual block now inserts    ;;
;; "\02VimpulseVisualBlockMode\03"                 ;;
;; in the kill ring.                               ;;
;; I chose that string because the presence of     ;;
;; non-printable chars makes it _very_ unlikely    ;;
;; to be generated by a normal copy or cut command ;;
;; when it is found in the kill ring, the block    ;;
;; is pasted instead. (Alessandro Piras)           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unless vimpulse-experimental
  (defadvice viper-put-back (around vimpulse-visual-block-put-back (arg) activate)
    (if (equal (current-kill 0) "\02VimpulseVisualBlockMode\03")
	(yank-rectangle)
      ad-do-it)))
;; EXPERIMENTAL: FIX to make pasting when in visual mode work like vim
(when vimpulse-experimental
  (defadvice viper-put-back (around vimpulse-visual-block-put-back (arg) activate)
    (let ((was-in-visual-mode vimpulse-visual-mode))
      (when vimpulse-visual-mode
	(vimpulse-visual-delete-command)
	(current-kill 1)
	(backward-char ))
    (if (equal (current-kill 0) "\02VimpulseVisualBlockMode\03")
	(yank-rectangle)
      ad-do-it)
    (when was-in-visual-mode
      (current-kill -1))))
  )


;; These 2 functions implement insertion at the beginning/ end of a visual 
;; block or linewise selection
(defun vimpulse-visual-insert (&optional arg)
  "Called when in visual mode to go insert mode at the beginning of the selection."
  (interactive "P")
  
  (when vimpulse-visual-mode-linewise (vimpulse-create-coords ?I))
  (when vimpulse-visual-mode-block (vimpulse-create-coords ?i))
  (vimpulse-visual-mode nil)
  (if vimpulse-visual-mode-linewise
      (viper-Insert arg)
      (viper-insert arg)))

(defun vimpulse-visual-append (&optional arg)
  "Called when in visual mode to go to insert mode at the end of the selection"
  (interactive "P")
  
  (when vimpulse-visual-mode-linewise (vimpulse-create-coords ?A))
  (when vimpulse-visual-mode-block (vimpulse-create-coords ?a))
  (vimpulse-visual-mode nil)
  (if vimpulse-visual-mode-linewise
      (viper-Append arg)
      (viper-append arg)))

;; CHECKME: this stuff probably doesn't work well with the new visual
;;           mode code
;; We need to detect when a command has deactivated the mark so that
;; Vimpulse is able to exit Visual mode
(defun vimpulse-detect-mark-deactivate ()
  (when (and vimpulse-visual-mode (not mark-active))
    (vimpulse-visual-mode 'toggle)))
(add-hook 'deactivate-mark-hook 'vimpulse-detect-mark-deactivate)

;;;
;;; 
;;;
;; CHECKME: is this still needed?
(defadvice viper-move-marker-locally (around vimpulse-move-marker-locally-wrap activate)
 (unless vimpulse-visual-mode
   ad-do-it))

;; CHECKME: is this still needed?
(defadvice viper-deactivate-mark (around vimpulse-deactivate-mark-wrap activate)
 (unless vimpulse-visual-mode
   ad-do-it))

;;; }}} End visual mode code
(provide 'vimpulse-visual-mode)
;; Load vimpulse components
(require 'vimpulse-dependencies)
(require 'vimpulse-utils)
(require 'vimpulse-misc-keybindings)
(require 'vimpulse-ex)
(require 'vimpulse-viper-function-redefinitions)
(require 'vimpulse-paren-matching)
(require 'vimpulse-text-object-system)
(require 'vimpulse-visual-mode)

(provide 'vimpulse)
