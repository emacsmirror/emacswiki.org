;;; bc-mode.el --- Minor mode for formatting right-margin block comments.
;;;_@ 
;;
;; Copyright (C) 1991-1996 Steve Burgett <burgett@eecs.berkeley.edu>
;; $Id: bc-mode.el,v 1.1 1996/10/19 18:54:00 burgett Exp burgett $
;;
;; Author: Steve Burgett <burgett@eecs.berkeley.edu>
;; Created: July 1992
;; Version: 1.1
;; Keywords: comment minor-mode programming editing
;; HTTP: http://robotics.eecs.berkeley.edu/~burgett/

(defconst bc-mode-version "1.1" "The current version of bc-mode")
(defconst bc-mode-date "October 1996" "Date of last update")  
(defconst bc-mode-maintainer-address "burgett@eecs.berkeley.edu")

;; Get the latest version of this package from:
;; http://robotics.eecs.berkeley.edu/~burgett/elisp.html
;;
;; LCD Archive Entry:
;; bc-mode|Steve Burgett|burgett@eecs.berkeley.edu|
;; Minor mode for formatting right-margin block comments in source code.|
;; October 1996|1.1||
;;
;; This file is not part of GNU emacs
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If you did not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave., Cambridge, MA 02139, USA.
;;
;;
;; Steve Burgett
;; University of California, Berkeley
;; Department of Electrical Engineering and Computer Sciences
;; Robotics and Intelligent Machines Laboratory
;;
;;
;;; Advice:
;;; -------
;; This package advises the following function: `do-auto-fill'. You
;; can disable this advice by setting the variable
;; `bc-enable-auto-fill-advice' to nil.
 
;;;_ & Commentary:
;;;  ----------
;;
;;;_  * Description:
;;;   ------------
;;
;; This package provides commands that format right-margin block-style
;; comments in source code.  These are comments that appear to the
;; right of the code, and often span several consecutive lines.  On
;; each line, the comment is delimited by start and end delimiters,
;; and these are lined up with the delimiters on adjacent lines to
;; form a rectangular block. For example, in Emacs Lisp:
;;
;;   (if narrow-failure
;;      (progn                             ; If failure, move `top' down  
;;        (set-buffer code-buffer)         ; and repeat the process. If   
;;        (goto-char top)                  ; success, this `if' returns   
;;        (forward-line 1)                 ; nil so the `while' terminates.
;;        (setq top (point))))
;;  
;; Or, in C:
;;                                          /* If the leading char of the */
;;   if((String[0] != '0') ||               /* string is '0', then the    */
;;      (tolower(String[1]) != 'x' &&       /* number is probably in hex  */
;;       !isdigit(String[1]))){             /* or octal; that is, unless  */
;;     iRes = sscanf(String, "%ld", &Temp); /* the number is just zero.   */
;;
;; You can even format banner comments, like this:
;;
;;                                          /******************************/
;;                                          /* If the leading char of the */
;;   if((String[0] != '0') ||               /* string is '0', then the    */
;;      (tolower(String[1]) != 'x' &&       /* number is probably in hex  */
;;       !isdigit(String[1]))){             /* or octal; that is, unless  */
;;     iRes = sscanf(String, "%ld", &Temp); /* the number is just zero.   */
;;                                          /******************************/
;;
;; This minor mode provides 25 commands for formatting and
;; manipulating comments of this style.  See the documentation of
;; `bc-mode-doc' for a description.
;;
;; To submit bug reports, hit `C-c r C-b'.  See the documentation for
;; `bc-submit-bug-report' for more information.  Suggestions,
;; comments, etc., to <burgett@eecs.berkeley.edu>
;;
;;
;;;_  * Installation:
;;;   -------------
;; 
;; Put this file somewhere where Emacs can find it (i.e., in one of
;; the paths in your `load-path'), `byte-compile-file' it, and add the
;; following to your ~/.emacs:
;; 
;;   (autoload 'bc-mode "bc-mode"
;;   "Minor-mode for formatting right-margin block-style comments in
;;   source code." t)
;;
;;   (autoload 'bc-make-regexp "bc-mode"
;;   "Make a regular expression defining the comment syntax to use 
;;   with bc-mode." t)
;;
;;   (autoload 'turn-on-bc-mode "bc-mode" "Turn on bc-mode unconditionally.")
;;
;;   (global-set-key "\C-xr;" 'bc-mode)
;;
;; To use bc-mode, you call the function `bc-mode'.  See the
;; documentation strings for the function `bc-mode' and for the
;; variable `bc-mode-doc'.
;;
;; There isn't much to customize.  See `User Variables', below.
;;
;;
;;;_  * Compatibility:
;;;   --------------
;;
;; This package is known to work with Emacs 19.28, through 19.34, and
;; XEmacs 19.14.  It may work with others, I haven't tried it.
;;
;; This package has been used succesfully with the following major
;; modes: cc-mode (c and c++), emacs-lisp-mode, and text-mode (using
;; file-local variables).
;;
;; This package is compatible with ksh-mode, but has the same problems
;; with the shell pattern matching operator that other functions do.
;; See the Bugs section below.
;;
;; This package also is known to work with perl-mode, and cperl-mode,
;; but older versions of those may trigger some bugs.  See the Bugs
;; section below for some workarounds.
;;
;; This package has been used succesfully with the following minor
;; modes: font-lock, auto-fill, filladapt, outline.  Compatibility
;; with other minor modes has not been tested.  If you use Kyle Jones'
;; filladapt.el, you must `require' that BEFORE bc-mode gets loaded.
;;
;;
;;;_  * Commands:
;;;   ---------
;;
;; NOTE: COMMENTS THAT BEGIN IN THE FIRST COLUMN ARE NOT RECOGNIZED BY
;; BC-MODE COMMANDS AS BLOCK COMMENTS.
;;
;; This is so you can comment out lines of code without bc-mode
;; thinking you want them formatted as block comments.  You can even
;; have comments in the right margin of such code and still format
;; them with BC commands.
;;
;; For a summary of commands and key bindings, see the documentation
;; string for `bc-mode' (do a `C-h m' or M-x describe-mode when
;; bc-mode is turned on, or do `C-h f bc-mode').  For more
;; information, see the documentation for the variable `bc-mode-doc'
;; (do `C-h v bc-mode-doc').
;;
;;
;;;========================================================================
;;
;;;_  * Wish List:
;;;   ----------
;;
;;; Complete Rewrite?
;;
;; Well, maybe not a complete rewrite.  But, like any program that has
;; some history, BC has built up a lot of vestigial organs (and
;; behaviors).  For a very long time, the main set of BC commands were
;; `squeeze', `shorter', `taller', `lower-flow', and `raise-flow'.
;; These are the commands that do lots of searching and computing to
;; try to find some "optimal" formatting and placement for the
;; comment.  More recently, the tab-oriented commands, `fill',
;; `wider', `narrower', and the non-reshaping movement commands, `up'
;; and `down', were added.  These commands do less computing and give
;; more control to the user.
;;
;; It has become clear that in practice these "dumber" commands are
;; the more useful.  The "smart" commands don't get used nearly as
;; often.  A rewrite of BC would start with a core functionality to
;; support the dumb commands, and could potentially simplify the code
;; significantly.  This is because the present code is based on
;; routines that were originally written to support the smart
;; commands.  When the dumb commands were added, many routines were
;; further *generalized* to support them as well.
;;
;;
;;; Commands
;;
;; Versions of `wider' and `narrower' with finer control, i.e., only
;; change the width by one column.  This could well obviate the more
;; complex commands `shorter' and `taller'.
;;
;; Additional editing and motion functions that treat comments like
;; little windows, ala `bc-beginning-of-line', `bc-kill-line', etc.
;;
;; bc-splice-comments: Find the bc-comment on or below point and join
;; it with the one above.
;;
;; bc-delete-indentation, bc-split-line.
;;
;;
;;; More General Comment Parsing
;;
;; It is clear that we want to be able to recognize more than one
;; comment syntax in a buffer.  C++ is the prime example of a language
;; with more than one comment syntax.  That feature has been copied
;; by Java and JavaScript as well.  In fact, a buffer containing
;; JavaScript always contains HTML as well, so there may be three
;; syntaxes to deal with.
;;
;;
;;; Generalized Ignoring
;;
;; Presently, BC ignores comments that start in the first column so
;; that it doesn't try to format commented-out code.  This could be
;; generalized to ignore any comment starting in `bc-ignore-column' or
;; before.  Setting this to -1 would allow BC to format all comments.
;;
;; I now feel that it would be much better if BC were able to format
;; comments that start in the leftmost column just like any others.  I
;; refuse, however, to give up the ability to preserve commented-out
;; code.  I do think it is possible to have both, it is just going to
;; be work to implement it.
;;
;; 
;;; Membership Tags
;;
;; Sometimes you would like to have block comments on consecutive
;; lines, but currently bc-mode determines the extent of a comment by
;; looking for lines that have no comments.  Another way could be to
;; allow a `tag-character' to come immediately after the starting
;; delimiter.  Only comments with matching tag charaters would be
;; considered to comprise a block comment.  For example:
;;
;;  int index = 1;			    //@ Start at one because that is
;;					    //@ what Numerical Recipes does.
;;  TWVertexNodeIterator iterN(rG);		   //| Go through every node
;;  while (iterN)				   //| in the Edit graph.   
;;  {				     //- If it's white it's a Steiner node so 
;;    TWVertexNode *pN = iterN++;    //- it's moveable. However, don't pull it
;;    if ((pN->Color() == gr::White) //- if it only has one edge because it   
;;				     //- will just get pulled on top of the   
;;	   && (pN->Degree() > 1))    //- node at the other end of that edge.  
;;
;; An alternative might be a comment-beginning char to indicate the
;; first line of each block comment.  For example:
;;
;;  int index = 1;			    //> Start at one because that is
;;					    //  what Numerical Recipes does.
;;  TWVertexNodeIterator iterN(rG);		   //> Go through every node
;;  while (iterN)				   //  in the Edit graph.   
;;  {				     //> If it's white it's a Steiner node so 
;;    TWVertexNode *pN = iterN++;    //  it's moveable. However, don't pull it
;;    if ((pN->Color() == gr::White) //  if it only has one edge because it   
;;				     //  will just get pulled on top of the   
;;	   && (pN->Degree() > 1))    //  node at the other end of that edge.  
;;
;; Both of these seem pretty hard to read.  A blank line to separate
;; comments is desirable if space permits.  Still, there are times when
;; tighter spacing would be nice.
;;
;;
;;;_  * To Do:
;;;   ------
;;
;; Make `do-auto-fill' context sensitive.  If the comment has any code
;; to its left (for any line of the commment), assume it is a
;; right-margin comment and don't auto-fill until text exceeds
;; bc-G-rm-minus-stuff.
;;
;; `bc-moveit' should call `bc-cleanup-newlines' after deleting the
;; old comment and *before* inserting the newly formatted one.  To
;; this end, `bc-find-room' should ignore lines that contain no code
;; and are marked with the `bc-added-newline' property.
;;
;;
;;;_  * Could Do:
;;;   ---------
;;
;; It wouldn't be too hard to support center and right-justification.
;; Maybe this would be useful for banner comments.  `bc-put-text'
;; could run through the scratch buffer just before copying it to the
;; code buffer and look for the `justification' text property.  If
;; present, call the real `fill-individual-paragraphs'.
;;
;; Support for adaptive fill would be nice too.  It would probably
;; mean not using bc-fill-buffer and just using fill-buffer instead.
;; That in turn would require a post-scan of the buffer to collect the
;; various metrics about the fill.  Might take a performance hit.
;;
;;;_  * Bugs:
;;;   -----
;;
;;; Formatting Bugs:
;;
;; Like all Emacs' comment-handling commands, BC is confused by
;; comment characters that appear in strings.  The problem appears
;; worse in when using BC because it means that a block comment cannot
;; span any such line.  For example:
;;
;;  if (!os)				    // The string contains the C++
;;  {					    // comment starter (double
;;    cerr << "Error: //comment starter.";  // slash), so BC cannot properly
;;    return;				    // format this block comment. It
;;  }					    // will assume that the comment
;;					    // starts inside the string.
;;
;; You'll have the same problem with Emacs' built in commenting
;; commands, e.g., `indent-for-comment' (M-;).
;;
;;
;;; Compatibility Bugs:
;;
;; Auto-filling does not work with cperl-mode because that mode
;; defines its own auto-fill-function.  A workaround is to set
;; `auto-fill-function' in the mode hook.  This seems to work okay,
;; your mileage may vary:
;;
;;  (add-hook 'cperl-mode-hook
;;  	      (function (lambda ()
;;  			  (turn-on-bc-mode)
;;  			  (setq auto-fill-function 'do-auto-fill))))
;;
;;
;; With older versions of perl-mode, indenting does not work properly
;; when bc-keymap-level is non-nil.  You can fix this by placing the
;; following in your .emacs:
;;
;; (add-hook 'perl-mode-hook 
;;	     (function (lambda ()
;;			(setq indent-line-function 'perl-indent-command))))
;;
;; BC is compatible with ksh-mode, but has the same problems
;; with the shell # and ## pattern matching operators that other
;; functions do (comment functions, font-lock, etc.).  It will confuse
;; these operators for comment starters.  The behavior is at times
;; quite baffling.
;;
;;
;;; Other Bugs:
;;
;; Lots of non-serious but ugly behavior when formatting empty
;; comments.
;; 
;; Not really a bug, but it seems to be impossible to define the rules
;; of reformatting so that narrowing, widening, raising, lowering,
;; etc., always do what you expect.  This is because there are usually
;; a huge number of ways that a comment could be fit into the space to
;; the right of the code.
;;
;; BC-mode is not very integrated with built-in emacs comment support
;; functions.  It isn't clear to me just what form this should take
;; -- I'm open to suggestions.
;;
;; In particular, `indent-sexp' messes up all right margin block
;; comments.  This doesn't look easy to fix, because the comments get
;; messed up deep in the bowels of `indent-sexp'.  One way I can see
;; of fixing this is to store the starting column of each comment in a
;; text property and then advise `indent-for-comment' to use that.
;; Additional hackery would be necessary to remove the text property
;; when you really do want the standard indentation.  Further, if the
;; user doesn't store the text properties with the file, `indent-sexp'
;; will mess up the comments next time the file is loaded.
;;
;;
;;; Thanks:
;;; ------
;;
;; Hearty thanks are due to several authors for their high-quality
;; software tools for Emacs Lisp:
;;
;; To Daniel LaLiberte for edebug.el, the Emacs Lisp source-level debugger.
;; To Barry Warsaw for elp.el, the Emacs Lisp Profiler.
;; To Jamie Zawinski and Hallvard Furuseth for Emacs Lisp byte compiler.
;;
;; 
;;;_ & Change Log:
;;;   -----------
;;  1.1  21 Oct 1996 (burgett)
;;      Redefined the keymaps to conform to Emacs' convention.
;;      Added bc-fill-paragraph and code to put it in
;;        `fill-paragraph-function'.
;;      Added faux-end-delimiter capability.
;;      Updated BUGS section in documentation.
;;      Updated bc-mode-doc.
;;
;;  1.0  19 Oct 1996 (burgett)
;;      Initial release.
;;
 
;;;_ & Code:
;;;  -----

(require 'advice)
(require 'cl)

;;;========================================================================
;;;_  * For debugging 

; (setq elp-function-list '(1+ add-text-properties bc-count-lines-1
; bc-delete-text bc-get-buffer-create bc-put-text goto-line message))

; (setq elp-function-list '(1+ add-text-properties bc-count-lines-1
; bc-delete-text bc-get-buffer-create bc-put-text current-buffer
; end-of-line fboundp funcall goto-char goto-line insert integerp
; length let list listp make-marker marker-position max message point
; progn save-excursion set-buffer set-marker setq while))


;;;;========================================================================
;;;_  * Documentation:

(defconst bc-mode-doc "Hi there!"
  "This is a dummy variable.  Its purpose is to provide more
information on bc-mode.  See also the documentation for the function
`bc-mode' \(Do `\\[describe-mode]' when bc-mode is active, or `\\[describe-function] bc-mode'\).

USING THE MINOR MODE:
=====================

  BC-mode is a minor mode for editing right-margin block-style
  comments in source code.  These are comments that appear to the
  right of the code, and often span several consecutive lines.  On
  each line, the comment is delimited by start and end delimiters, and
  these are lined up with the delimiters on adjacent lines to form a
  rectangular block. For example, in C:

                                       /* If the leading char of the */
   if\(\(String[0] != '0'\) ||            /* string is '0', then the    */
      \(tolower\(String[1]\) != 'x' &&    /* number is probably in hex  */
       !isdigit\(String[1]\)\)\){          /* or octal; that is, unless  */
     iRes = sscanf\(String, \"%ld\", &T\); /* the number is just zero.   */


  Commands provided by bc-mode fall roughly into three groups:
  formatting, editing, and motion:

  Formatting commands include
  ---------------------------
    `bc-fill' and `bc-squeeze'          Block fill the comment.
    `bc-raise' and `bc-lower'           Move the comment up or down
					   one or more lines.
    `bc-raise-flow' and                 Move the comment up or down,
    `bc-lower-flow'                        reformatting it if necessary.
    `bc-narrower' and `bc-wider'        Reformat the comment narrower
					   or wider.
    `bc-taller' and `bc-shorter'        Reformat the comment taller
                                           or shorter.

   Editing commands include
  ---------------------------
    `bc-kill-comment'                   Kill the text of the comment.
    `bc-copy-comment-as-kill'           Save the comment as if killed,
					   but don't kill it. 
    `bc-yank' and `bc-yank-pop'         Yank from kill-ring and format
					   as comment. 
    `bc-kill-line'                      Comment-sensitive version of
					   `kill-line'.
    `bc-open-line', `bc-newline',
    `bc-indent-new-comment-line',       Comment-sensitive versions of
    `bc-indent-according-to-mode'          their namesakes.

  Motion commands include
  ---------------------------
    `bc-beginning-of-line' and          Move to beginning or end of
    `bc-end-of-line'                       comment or code.
    `bc-backward-comment' and           Move one or more block comments
    `bc-forward-comment'                   backward or forward.



How BC-mode Formats Block Comments:
-----------------------------------
  BC's formatting commands move and reshape block comments.  BC
  considers a block comment to consist of all the comment text on
  consecutive lines that contain comments.  Two block comments must be
  separated by at least one line that contains no comment, or contains
  only a comment that starts in the first column.

  COMMENTS THAT BEGIN IN THE FIRST COLUMN ARE NOT RECOGNIZED BY
  BC-MODE COMMANDS AS BLOCK COMMENTS.

  This is so you can comment out lines of code without bc-mode
  thinking you want them formatted as block comments.  You can even
  have comments in the right margin of such code and still format them
  with BC commands.

  The current implementation of BC can only recognize one comment
  style at a time.  In languages like C++ and Pascal, BC can only
  format comments having the syntax with which bc-mode was turned on.
  To switch to another syntax, use the command `bc-mode' with a prefix
  argument \(C-u \\[bc-mode]\) and select the desired syntax.

  BC formatting commands reshape block comments \(while keeping them
  blocked\), subject to the space available to the right of the code.
  In some cases, BC adds blank lines in order to be able to fulfill a
  formatting request.

  BC commands decide how to format by examining the comment.  Much of
  the format of a block comment is determined by its first few lines.
  For example, a C++ comment can begin with any number of slashes.  BC
  commands will examine the first line of the comment \(unless it is a
  banner comment\) to decide how many slashes to use when reformatting
  the comment.  If it is a banner comment, the first line containing
  the text of the comment is used.

  Thus, if you decide your comment would look better with three
  slashes instead of two, add one to the beginning of the first line
  of the comment and type `\\[bc-fill]' \(bc-fill, or any other BC
  formatting command\).  The comment will be reformatted so that the
  start and end delimiters on each line match those on the first line.
  This works in other languages as well.  For example, in C your start
  and end delimiters may include any number of `*' characters,
  e.g. `/*** ***/'

  BC-mode determines the following things by examining the comment:

    If the first line of the comment meets the criteria of being a
    banner border, the comment is considered to be a banner comment
    and the border character is determined from this line.

    If the second line of a banner comment contains only whitespace
    inside the delimiters, a single whitespace-only line will be
    preserved at the top of the formatted comment.

    The effective start delimiter is taken from the first line of a
    non-banner comment, or the first text line of a banner comment.

    If that line has whitespace between the start delimiter and the
    text, that is considered part of the effective delimiter.

    The effective end delimiter is determined in the same way, but
    unfortunately, it is not possible to accurately preserve
    whitespace before the end delimiter.  Instead, the effective end
    delimiter will contain the same amount of whitespace as the
    effective start delimiter.  Additional spaces are added as
    necessary to align the end delimiters to `bc-right-margin'.

    In languages that have no comment end delimiter, the first line is
    checked for a faux end delimiter.  To qualify, the line must end
    with a string of characters that is the mirror image of the start
    delimiter (see below).  If one is recognized, it becomes the
    effective end delimiter.

    If a banner comment has a blank line at the bottom \(above the
    bottom border\), a single whitespace-only line will be preserved at
    the bottom of the formatted comment.

    A comment can have multiple paragraphs.  The paragraph separator
    is a \"blank line,\" meaning a line with a comment that contains
    only delimiters and whitespace.  Note the difference between this
    and a line that contains no comment at all, which is used to
    separate two block comments.  Multiple paragraphs within a block
    comment are filled individually, and a single \"blank line\" will
    be preserved to separate them.


Squeezing and Tabbing:
----------------------
  BC's formatting commands can be further divided into three
  subgroups: tabbing, squeezing, and moving.  Tabbing commands place
  the formatted comment at at \"tab-stop\".  Tabbing commands include
  `bc-fill', `bc-wider', and `bc-narrower'.

  Squeezing commands square up the comment to make it take up the
  fewest number of columns for a given number of lines, then place the
  comment as far to the right as possible.  Squeezing commands include
  `bc-squeeze', `bc-shorter', and `bc-taller'.

  Moving commands don't squeeze or tab, they format the comment as
  close as possible to its current shape and place it as close as
  possible to its current horizontal position.  Moving commands
  include `bc-raise', `bc-raise-flow', `bc-lower' and `bc-lower-flow'.

  Probably you will use the tabbing commands most often.  These tend
  to give comments a more formal appearance because the comments can
  be horizontally aligned with others above and below.  You can set
  tab stops individually (in lisp), or you can set them to a uniform
  spacing with the command `bc-set-tab-spacing'.  Large values of tab
  spacing force block comments into a smaller selection of horizontal
  positions make them more visually aligned.  Large values also make
  it more difficult to format a comment in a tight space.

  Use the squeezing commands to fit a comment into a tight
  right-margin space or to get the maximum amount of white space
  between the code and the comment.  For example, if you need to fit a
  comment into a right-margin space of 20 columns, the default tab
  spacing of 8 is generally too coarse for the kind of fine-tuning you
  need.  The answer is not to set the tab spacing to a very small
  value (though this would work), but to adjust the comment by
  increments of one line taller or shorter.

  Moving commands preserve the horizontal placement of the comment as
  much as they can.  Two of these commands, `bc-raise-flow' and
  `bc-lower-flow', do it without disturbing the code to the left.
  Sometimes they may hit a line of code that is too wide to allow the
  same horizontal placement.  When this happens they will place the
  comment just after the end of that line.  The result is that the
  comment is neither squeeezed nor tabbed.  (When called with a prefix
  argument `bc-fill' has a similar behavior.  It just doesn't move the
  comment vertically).  Usually you will want to finish a sequence of
  such commands with a `bc-fill' (no prefix argument) or `bc-squeeze'.
  The other two moving commands `bc-raise-flow' and `bc-lower-flow',
  always preserve the horizontal position of the comment, and
  sometimes insert blank lines to permit them to do so.

  Note that a comment can be forced to any column (if there is room)
  by adding or deleting spaces before the start delimiter of the first
  line of text.  When that line of the comment begins in the desired
  column, do `bc-fill' with a prefix argument \(C-u \\[bc-fill]\) to
  align the rest of the comment.


Banner comments:
----------------
  Banner comments are those that include a border comment line for the
  first and last lines of the block comment. For example, in C:

  if\(*pId\){                          /**===========================**/
    return pId;                      /**                           **/
  } else {                           /** This is a banner comment. **/
    return 0;                        /**                           **/
  }                                  /**===========================**/

  Banner comments may optionally include blank lines above and below
  the text, as the one above does.  Note also that effective
  delimiters with an extra `*' character were used.  

  A comment on a line is considered to be a banner border if it
  consists entirely of the same character \(like the equal sign above\),
  and contains no whitespace anywhere inside the delimiters.  The
  character may be one that is found in the delimiters \(e.g. a `*' in
  C\) if desired.

  If a block comment has a top banner it is considered a banner
  comment.  It will be formatted with both top and bottom banners,
  whether or not the bottom border is initially present.  Either or
  both blank lines may be present, and they will be preserved
  independently.  Note that if a banner comment has a blank line above
  the bottom border, the bottom border must be initially present for
  it to be preserved.

  In languages that use only start delimiters , faux end delimiters
  may be added if desired.  For example, in Lisp, a comment begins
  with a semicolon and continues to the end of the line.  Semicolons
  may be added at the end of each line to provide a border:

     \(setq bc-mode t\)                ;;=============================;;
     \(bc-select-keymap t\)            ;;  This is a Banner comment   ;;
     \(bc-install-menubar\)            ;;  with faux end delimiters   ;;
     \(bc-sync-advice-enable\)         ;;=============================;;
  
  Faux end delimiters may be added independently of top and bottom
  banner borders.  When the comment is reformatted, the faux end
  delimiters will be maintained at the end of the line, rather than
  getting mixed in with the text of the comment.

  To qualify as a faux end delimiter, the string that ends the line
  must be an exact mirror of the start delimiter.  For example,
  suppose a language has comments that begin with `rem<' and continue
  to the end of the line.  The string `>mer' at the end of the line
  will be recognized as a faux end delimiter:

     \(setq bc-mode t\)       		rem<  Here is a comment	   >mer
     \(bc-select-keymap t\)   		rem<  with faux end	   >mer
     \(bc-install-menubar\)   		rem<  delimiters.	   >mer

  Optionally, the last character of the start delimiter and the first
  character of the faux end delimiter may be repeated any numbers of
  times: 

     \(setq bc-mode t\)       		rem<<  Here is a comment >>>mer
     \(bc-select-keymap t\)   		rem<<  with faux end	 >>>mer
     \(bc-install-menubar\)   		rem<<  delimiters.	 >>>mer


Line Killing and Insertion:
---------------------------
  Several of bc-mode's editing commands provide comment-sensitive line
  killing and insertion.  These include `bc-kill-line',
  `bc-open-line', `bc-newline', and `bc-indent-new-comment-line'.  If
  point is in the code, these commands apply to the code and attempt
  to preserve the integrity of any block comment.  If point is in the
  comment, these commands attempt to minimally affect the code.

  When adding a new line to a block comment, The line insertion
  commands try to move following lines of the block comment down onto
  subsequent lines of code, without adding blank lines.  For example,
  suppose point is at the location denoted by `@':

    } else {			   /* Handle bit-valued i/o point.@*/
      if(iValue){
        gaIo_shadows[iShadow].iData |= 1;

  If the user types `bc-indent-new-comment-line' (\\[bc-indent-new-comment-line]), the new
  comment line will be inserted like this:

    } else {			   /* Handle bit-valued i/o point. */
      if(iValue){		   /* @*/
        gaIo_shadows[iShadow].iData |= 1;

  However, if the user types `bc-indent-new-comment-line' a second
  time, the line will be inserted like this:

    } else {			   /* Handle bit-valued i/o point. */
      if(iValue){		   /*  */
                                   /* @*/
        gaIo_shadows[iShadow].iData |= 1;

  This is because the third line of code extended past the starting
  column of the block comment.  

  Suppose, however, that you would have preferred the third line of
  the comment to be appended to the third line of code in such a case.
  This is controlled by the variable `bc-code-width-threshold'.  The
  above example assumes the default value of zero.  Setting this
  variable to a value of six or more would have caused the comment to
  be appended to the code on the third line, because the code is six
  columns wider than the comment's starting column.  Had the line
  exceeded this threshold, a blank line would still have been
  inserted.

  A small value (like six) allows appending comments to lines that are
  just a little too wide, and forces a blank line to be inserted for
  lines of code that are very wide.  A small negative value causes BC
  to maintain some separation between the code and the comment.  In
  practice, the setting of this variable is not terribly important,
  because you will probably clean up your comments with a BC
  formatting command after editing them.


Auto Fill:
----------
  BC has some rudimentary support for `auto-fill-mode'.  If you turn
  on both bc-mode and auto-fill-mode, auto-filling is modified so that
  comments and code disrupt each other as little as possible.  

  If you use this, you will probably want to set `fill-column' to just
  a few less than `bc-right-margin'.  For example, in C the comment
  end delimiter is `*/'.  If you use the default value of
  `bc-right-margin' \(79\), and if you like a space before the end
  delimiter, set `fill-column' to 79 - 3 = 76.  You can do this by
  typing `M-7 M-6 \\[set-fill-column]'.  Or you may want to use a major-mode hook
  to set this from Lisp.

  If you want to use both bc-mode and auto-fill-mode, but do *not*
  want this behavior, set the variable `bc-enable-auto-fill-advice' to
  nil.  You can toggle this variable interactively with the function
  `bc-toggle-advice-enable'. 

Keymaps:
--------
  You can select from two keymaps for bc-mode to use.
  `bc-mode-basic-map' is a minimally intrusive set of bindings for
  BC's most powerful functions.  `bc-mode-extended-map' adds
  additional bindings for cursor motion commands.  These commands
  override some standard commands, such as `beginning-of-line' \(bound
  to C-a, Home, etc\).  BC's versions of these commands behave like the
  standard ones when point is outside a block comment, and move within
  the comment otherwise.

  To select a keymap interactively, use the function
  `bc-toggle-keymap-level'.  This sets the value of the variable
  `bc-keymap-level' and updates the current keymap.  Alternatively you
  can set the value of `bc-keymap-level' in lisp.  A value of nil
  selects `bc-mode-basic-map'; non-nil selects `bc-mode-extended-map'.

Variables controlling bc-mode:
------------------------------
  `bc-right-margin'
     Right-margin for block comments.
     Default: 79
  `bc-keymap-level'
     Select keymap level to use with BC minor mode.
     Default: nil
  `bc-code-width-threshold'
     Amount by which the width of a line of code may exceed the
     current line's for line-insertion commands to move comments onto
     it.
     Default: 0
  `bc-tab-stops'
     List of tab stops to use in each buffer.
     Default: (1 8 16 24 32 40 48 56 64 72)

  Other variables:
  ----------------
  `bc-enable-auto-fill-advice'
  `bc-enable-fill-paragraph-function'


  For convenience `bc-mode' sets the following variables if they are
  unbound.  BC does not otherwise use these because it has its own
  internal equivalents:

  `comment-start'
     String to insert to start a new comment.
  `comment-end'
     String to insert to end a new comment.
  `comment-start-skip'
     Regexp to match the start of a comment plus everything up to its body.


Hints:
------
  `bc-wider'  \(\\[bc-wider]\) and `bc-narrower' \(\\[bc-narrower]\) are conceptually the
  inverse of each other, but sometimes leave extra blank (except for
  the comment) lines.  Use `bc-lower' \(\\[bc-lower]\) to suck the code back from
  down below.

  If you can't get a comment moved to where you want with `bc-raise'
  \(\\[bc-raise]\) and `bc-lower' \(\\[bc-lower]\), try `bc-kill-comment' \(\\[bc-kill-comment]\) and
  `bc-yank' \(\\[bc-yank]\).

  If you can't make a comment narrow enough with `bc-narrower' \(\\[bc-narrower]\),
  try `bc-taller' \(\\[bc-taller]\), or `bc-squeeze' \(\\[bc-squeeze]\).

  To join two comments into one, go to the comment-free line that
  separates them and do `indent-for-comment' \(\\[indent-for-comment]\), which will add
  comment delimiters to the end of that line and move point just
  inside.  Then do `bc-kill-line' \(\\[bc-kill-line]\) to suck the lower comment
  up into the newly created line.

  To separate a comment into two, go to the beginning of the comment
  text on the line where you want to split and do `bc-open-line'
  \(\\[bc-open-line]\). This will leave a line with only delimiters, which can
  then be deleted with `kill-comment' \(\\[kill-comment]\).

  To add a banner to a comment, go to the line just above it and do
  `indent-for-comment' \(\\[indent-for-comment]\). \(Be careful not to inadvertently glue
  two comments together\).  Delete any whitespace between the comment
  delimiters \(not necessary if there is no end delimiter\).  If you now
  do a formatting command \(e.g. `bc-fill'\) a banner will be
  constructed of characters from the delimiters \(e.g the semicolon in
  Lisp\).  If you want a different character \(e.g. an equal sign\),
  first type that between the delimiters, with no whitespace.

  To remove a banner from a comment, delete the top banner and its
  delimiters.  Then do a BC formatting command, like `bc-fill' \(\\[bc-fill]\).

  To add a right-hand border of faux end delimiters, go to the first
  line of text in the comment and add a faux delimiter at the end.  Do
  a BC formatting command, and faux delimiters will be added to the
  end of each line in the comment.


CALLING THE FUNCTION BC-MODE:
=============================

  The function `bc-mode' toggles bc-mode.  Behavior of that function
  depends on whether it is called interactively or from Lisp.  

Interactive Command:
--------------------
  If called interactively, the user may turn bc-mode on or off, and
  optionally select a comment syntax.

  With no prefix argument, calling `bc-mode' toggles bc-mode.  If the
  minor mode has never been on in the current buffer, calling with no
  argument is equivalent to calling with a non-negative prefix
  argument.

  With a non-negative prefix argument, the user is prompted to choose
  a language to define the desired comment syntax.  An initial guess
  is placed in the minibuffer, based either on the current major mode
  \(if bc-mode has never been turned on in this buffer\), or the value
  from the last time bc-mode was on in the buffer.

  With a negative prefix argument, bc-mode is turned off.


Lisp Function:
--------------
  From Lisp, `bc-mode' is called as follows:

    \(bc-mode &optional ARG\)

  The simplest way to invoke `bc-mode' from Lisp is to call it with no
  arguments.  The minor mode will be toggled.  If the minor mode has
  never been on in the buffer, a comment syntax will be guessed from
  the major mode and bc-mode will be turned on.  If a guess cannot be
  made, an error is signalled.  If the minor mode has previously been
  on in the buffer, it will use the same comment syntax as before.

  For convenience, the function `turn-on-bc-mode' turns on bc-mode
  unconditionally.

  The argument ARG can be supplied to exert more control.  It may be a
  number, a string, or a symbol.

    If a number, non-negative turns bc-mode on, negative turns it off.
    When the mode is turned on, the comment syntax is guessed if this
    is the first time, otherwise the previous syntax is used.

    If ARG is a string, it must be a regular expression describing the
    comment syntax.  BC-mode is turned on.  For most languages, this
    regular expression can be generated automatically by the function
    `bc-make-regexp'.  Also see the documentation for the variable
    `bc-comment-regexp'.

    If ARG is a symbol, it must be one of the keys in
    `bc-languages-alist'.  BC-mode is turned on and the corresponding
    comment syntax is used.

  If either `comment-start' or `comment-end' is not a string, an
  attempt is made to set them appropriately.


Examples:
---------
  The following might be put in your ~/.emacs:

  \(add-hook 'c++-mode-hook 'turn-on-bc-mode\)    ; Syntax will be guessed
						; from major mode.

  \(add-hook 'c++-mode-hook		        ; Turn on bc-mode but
	    \(function \(lambda \('bc-mode 'c\)\)\)\)	; use C style comments.


  If you program in a language that bc-mode does not know, you can
  call `bc-make-regexp' to build a comment syntax for you.  Suppose
  your language begins comments with \"<*\" and ends them with \"*>\".
  You could add the following to your ~/.emacs:

  \(add-hook 'my-lang-mode-hook
	    \(function \(lambda \(\)
			\(bc-mode \(bc-make-regexp \"<*\" \"*>\"\)\)\)\)\)

  You can also turn on bc-mode using File Local Variables.  If the
  language is known to BC, you can use the following format.  For
  example, at the end of a shell script, the following lines invoke
  bc-mode whenever the file is loaded:

    # Local Variables:
    # bc-language: csh
    # mode: bc
    # End:

  Note that the line for `bc-language' must come before the `mode'
  line.

  In the case that the language in the file is not known to BC, you
  must use the `eval' construct:

    # Local Variables:
    # eval: \(bc-mode  \(bc-make-regexp \"<*\" \"*>\"\)\)
    # End:

  If your language has no comment-end string, i.e., comments continue
  to end-of-line, pass only the comment-start string to
  `bc-make-regexp'.

  Note that `bc-make-regexp' will set up `comment-start' and
  `comment-end' if they are not already strings.  It does this by
  appending a space to its first argument and prepending a space to
  its \(optional\) second argument.")

;;;;========================================================================
;;;_  * To quiet the byte compiler

 ; Quiet the byte-compiler. Yes, I know these are obsolete, but other
 ; packages don't. Some functions need to `let' these to nil to
 ; improve speed.
(eval-when-compile
  (put 'after-change-function 'byte-obsolete-variable nil)
  (put 'before-change-function 'byte-obsolete-variable nil))
  ; Quiet `reference to free variable' complaints

(if (not (boundp 'filladapt-mode)) (defvar filladapt-mode nil))

(defsubst bc-dummy (&rest args))
(defsubst bc-funcall (func &rest args) (apply func args))

;;;;========================================================================
;;;_  * User Variables

(defvar bc-mode-prefix "\C-c,"
  "Prefix key sequence for most bindings in `bc-mode-basic-map'.
This is the keymap bc-mode uses unless `bc-keymap-level' is non-nil.")

(defvar bc-mode-ext-prefix "\C-\\"
  "Prefix key sequence for certain bindings in `bc-mode-extended-map'.
This is the keymap bc-mode uses when `bc-keymap-level' is non-nil.")

(defvar bc-enable-auto-fill-advice t
  "*Whether to enable advice on `do-auto-fill'.

Bc-mode defines advice on this function to assist correct auto-filling of
block-comments.  If you don't like this, set the value of this variable to
nil.  To toggle this interactively, use the command
`bc-toggle-auto-fill-advice' \(\\[bc-toggle-auto-fill-advice]\)")

(defvar bc-enable-fill-paragraph-function t
  "*If non-nil, enable `bc-fill-paragraph' as a fill-paragraph-function.
This will cause `\\[fill-paragraph]' to fill comments as right-margin
comments whenever it sees them, if bc-mode is turned on.

If this variable is nil, `\\[fill-paragraph]' maintains is traditional
behavior even when bc-mode is on.

For changes to this variable to take effect, bc-mode must be turned
off and back on.")

(defvar bc-right-margin 79
  "*Right margin for block comments.
In a language with no end delimiters, comment text will be formatted
so as not to exceed this column.  In a language with end delimiters,
comment text will be padded with spaces so that all end delimiters end
in this column.

You can set this interactively by typing `\\[bc-set-margin]'
\(`bc-set-margin'\).

For more information, type `\\[describe-variable] bc-mode-doc'.")
(make-variable-buffer-local 'bc-right-margin)

(defvar bc-keymap-level nil
  "*Selects keymap level to use with BC minor mode.  
Nil causes bc-mode to use `bc-mode-basic-map'.  Non-nil causes bc-mode
to use `bc-mode-extended-map', which overrides some basic global keys
like C-a with versions defined by BC.  Most of these behave similarly
to the functions they shadow, but are context-sensitive to
right-margin comments.

You can set this interactively by typing `\\[bc-toggle-keymap-level]'
\(`bc-toggle-keymap-level'\).

For more information, type `\\[describe-variable] bc-mode-doc'.")

(defvar bc-tab-stops nil
  "*Tab stops for BC formatting commands.

This should be a list of integers in ascending order.  Each denotes a
column for BC to use as a tab stop.  A tab stop of zero will be
ignored.

You can set tab stops to a uniform spacing with the function
`bc-set-tab-spacing'.")
(make-variable-buffer-local 'bc-tab-stops)

(defvar bc-languages-alist ; NOCOLLECT
;(defconst bc-languages-alist ; for debugging

  ;; Must specify regexps for long delimiters explicitly because
  ;; bc-make-regexp can't handle more than two chars per delim.
  '(
    (ada                                         (delim "--"))
    (asm                                         (delim ";"))
    (awk                                         (delim "#"))
    (bash                                        (delim "#"))
    (bib                                         (delim "%"))
    (c                                           (delim "/*" "*/"))
    (c++                                         (delim "//"))
    (cperl                                       (delim "#"))
    (csh                                         (delim "#"))
    (eifel                                       (delim "--"))
    (emacs-lisp                                  (delim ";"))
    (f90                                         (delim "!"))
    (html (regexp "[^<]<!-+[ \t]*\\(.*\\)-+>")   (delim "<!--" "-->"))
    (java                                        (delim "//"))
    (ksh                                         (delim "#"))
    (latex                                       (delim "%"))
    (lisp                                        (delim ";"))
    (makefile                                    (delim "#"))
    (mim                                         (delim ";\"" "\"") )
    (modula-2                                    (delim "(*" "*)"))
    (nroff                                       (delim "\\\""))
    (pascal                                      (delim "(*" "*)"))
    (pascal-brace                                (delim "{" "}"))
    (pascal-paren                                (delim "(*" "*)"))
    (perl                                        (delim "#"))
    (postscript                                  (delim "%"))
    (rexx                                        (delim "/*" "*/"))
    (scheme                                      (delim ";"))
    (sgml (regexp "[^<]<!-+[ \t]*\\(.*\\)-+>")   (delim "<!--" "-->"))
    (tcl                                         (delim "#"))
    (tex                                         (delim "%"))
    (vrml                                        (delim "#"))
    )

  "Alist mapping languages to comment syntax information.  

The keys are symbols denoting languages; the values are alists
containing language information.  Each of these alists may contain the
following keys:

    `delim'         The value is a list of strings.  The first
                    string contains the comment start delimiter
                    \(without any whitespace\).  An optional second
                    string contains the comment end delimiter, if the
                    language has one.

    `regexp'        The value is a list of one string.  It is an
                    explicit regular expression defining the comment
                    syntax.  It is only necessary in special
                    circumstances.  In most cases, a regular
                    expression can be computed from the `delim' field.")

(defvar bc-code-width-threshold 0
  "*Used by line-inserting commands such as `bc-open-line' to decide
whether a new comment line can be added to the left of the code on a
line below, or if a blank line must be inserted.  If the width of the
code on a subsequent line is less than the starting column of this
comment plus `bc-code-width-threshold', no blank line is inserted.
Otherwise blank lines are inserted before the too-wide line.

Setting this to a large negative value, like -999, causes blank lines
to always be inserted, in the manner of the standard
`indent-new-comment-line' (\\[indent-new-comment-line]) command.
Setting this to a large positive value, like 999, causes blank lines
to never be inserted.

Good choices for this variable are zero and small positive and
negative integers.  In particular, a value of zero will cause blank
lines to be inserted only when necessary to allow the new comment line
to line up under the previous one.

For more information, type `\\[describe-variable] bc-mode-doc'.")

;;;;========================================================================
;;;_  * Menus

(defvar bc-mode-menu
  '("Comments"
    ("Options"
     ["Extended Keymap"   bc-toggle-keymap-level
      :style toggle
      :selected bc-keymap-level]
     ["Comment-Oriented Auto-Filling"  bc-toggle-auto-fill-advice
      :style toggle
      :selected bc-enable-auto-fill-advice]
     ["Set Tab Spacing..."  bc-set-tab-spacing t]
     ["Set Margin..."  bc-set-margin t]
     )
    "---"
    ["Fill"              bc-fill                         t]
    ["Wider"             bc-wider                        t]
    ["Narrower"          bc-narrower                     t]
    "---"
    ["Squeeze"           bc-squeeze                      t]
    ["Shorter"           bc-shorter                      t]
    ["Taller"            bc-taller                       t]
    "---"
    ["Raise"             bc-raise                        t]
    ["Raise/Flow"        bc-raise-flow                   t]
    ["Lower"             bc-lower                        t]
    ["Lower/Flow"        bc-lower-flow                   t]
    "---"
    ["Cut"               bc-kill-comment                 t]
    ["Copy"              bc-copy-comment-comment-as-kill t]
    ["Paste Most Recent" bc-yank                         t]
    "---"
    ["Open Line"         bc-open-line                    t]
    ["Newline"           bc-newline                      t]
    ["Indent New Line"   bc-indent-new-comment-line      t]
    ["Kill Line"         bc-kill-line                    t]
    "---"
    ["Next Comment"      bc-forward-comment              t]
    ["Previous Comment"  bc-backward-comment             t]
    )
  "XEmacs 19 menu for bc-mode.")

(defun bc-install-menubar (&optional remove)
  ;; install or remove the menu
  (let ((path '("Comments")))
    (condition-case nil
	(cond
	 (remove
	  (bc-funcall 'delete-menu-item path))
	 (t
	  (bc-funcall 'set-buffer-menubar
		      (copy-sequence (symbol-value 'current-menubar)))
	  (bc-funcall 'add-submenu nil bc-mode-menu)))
      (error nil))))

;;;(defvar bc-mode-menu-bar-map nil)

(defun bc-mode-fsf-menu (name map)
  ;; Add menu to a keymap.  Don't add them for XEmacs. This feature
  ;; test will fail on other than Emacs 19.  Borrowed from cc-mode.el
  (condition-case nil
      (progn
	(define-key map [menu-bar] (make-sparse-keymap))
	(define-key map [menu-bar bc] (cons name (make-sparse-keymap name)))

	(define-key map [menu-bar bc bc-backward-comment]
	  '("Previous Comment" . bc-backward-comment))
	(define-key map [menu-bar bc bc-forward-comment]
	  '("Next Comment" . bc-forward-comment))
	
	(define-key map [menu-bar bc separator-edit-line] '("--"))
	(define-key map [menu-bar bc bc-kill-line]
	  '("Cut Line" . bc-kill-line))
	(define-key map [menu-bar bc bc-indent-new-comment-line]
	  '("Indent New Line" . bc-indent-new-comment-line))
	(define-key map [menu-bar bc bc-newline]
	  '("Newline" . bc-newline))
	(define-key map [menu-bar bc bc-open-line]
	  '("Open Line" . bc-open-line))

	(define-key map [menu-bar bc separator-xcp] '("--"))
	(define-key map [menu-bar bc bc-yank]
	  '("Paste Most Recent" . bc-yank))
	(define-key map [menu-bar bc bc-copy-comment-comment-as-kill]
	  '("Copy" . bc-copy-comment-comment-as-kill))
	(define-key map [menu-bar bc bc-kill-comment]
	  '("Cut" . bc-kill-comment))
	
	(define-key map [menu-bar bc separator-edit-comment] '("--"))
	(define-key map [menu-bar bc bc-lower-flow]
	  '("Lower/Flow" . bc-lower-flow))
	(define-key map [menu-bar bc bc-lower]
	  '("Lower" . bc-lower))
	(define-key map [menu-bar bc bc-raise-flow]
	  '("Raise/Flow" . bc-raise-flow))
	(define-key map [menu-bar bc bc-raise]
	  '("Raise" . bc-raise))
	
	(define-key map [menu-bar bc separator-squeeze] '("--"))
	(define-key map [menu-bar bc bc-taller]
	  '("Taller" . bc-taller))
	(define-key map [menu-bar bc bc-shorter]
	  '("Shorter" . bc-shorter))
	(define-key map [menu-bar bc bc-squeeze]
	  '("Squeeze" . bc-squeeze))
	
	(define-key map [menu-bar bc separator-fill] '("--"))
	(define-key map [menu-bar bc bc-narrower]
	  '("Narrower" . bc-narrower))
	(define-key map [menu-bar bc bc-wider]
	  '("Wider" . bc-wider))
	(define-key map [menu-bar bc bc-fill]
	  '("Fill" . bc-fill))

							; The Options submenu

	(define-key map [menu-bar bc separator-options] '("--"))
	(define-key map [menu-bar bc options]
	  (cons "Options" (make-sparse-keymap "Options")))
	(define-key map [menu-bar bc options bc-set-margin]
	  '("Set Margin..." . bc-set-margin))
	(define-key map [menu-bar bc options bc-set-tab-spacing]
	  '("Set Tab Spacing..." . bc-set-tab-spacing))
	(define-key map [menu-bar bc options bc-toggle-auto-fill-advice]
	  (if bc-enable-auto-fill-advice
	      '("Normal Auto-Filling" . bc-toggle-auto-fill-advice)
	    '("Comment-Oriented Auto-Filling" . bc-toggle-auto-fill-advice)))
	(define-key map [menu-bar bc options bc-toggle-keymap-level]
	  (if bc-keymap-level
	      '("Basic Keymap" . bc-toggle-keymap-level)
	    '("Extended Keymap" . bc-toggle-keymap-level)))
	t)
    (error nil)))

;;;;========================================================================
;;;_  * Keymaps

(defvar bc-mode-map nil ; NOCOLLECT
  "Keymap for BC minor mode.")

(defvar bc-mode-prefix-map nil) ; NOCOLLECT
(if bc-mode-prefix-map
    nil
  (setq bc-mode-prefix-map (make-sparse-keymap))
  (define-key bc-mode-prefix-map ";"    'bc-mode)
  (define-key bc-mode-prefix-map "[" 'bc-toggle-keymap-level)
  (define-key bc-mode-prefix-map "\C-a" 'bc-beginning-of-line)
  (define-key bc-mode-prefix-map "\C-b" 'bc-submit-bug-report)
  (define-key bc-mode-prefix-map "\C-e" 'bc-end-of-line)
  (define-key bc-mode-prefix-map "\C-f" 'bc-set-margin)
  (define-key bc-mode-prefix-map "\C-j" 'bc-indent-new-comment-line)
  (define-key bc-mode-prefix-map "\C-k" 'bc-kill-line)
  (define-key bc-mode-prefix-map "\C-m" 'bc-newline)
  (define-key bc-mode-prefix-map "\C-o" 'bc-open-line)
  (define-key bc-mode-prefix-map "\C-p" 'bc-yank-pop)
  (define-key bc-mode-prefix-map "\C-w" 'bc-kill-comment)
  (define-key bc-mode-prefix-map "\C-y" 'bc-yank)
  (define-key bc-mode-prefix-map "\M-q" 'bc-fill)
  (define-key bc-mode-prefix-map "\M-w" 'bc-copy-comment-comment-as-kill)
  (define-key bc-mode-prefix-map "\M-y" 'bc-yank-pop)
  (define-key bc-mode-prefix-map "\t" 'bc-indent-according-to-mode)
  (define-key bc-mode-prefix-map "a"    'bc-beginning-of-line)
  (define-key bc-mode-prefix-map "b"    'bc-backward-comment)
  (define-key bc-mode-prefix-map "d"    'bc-lower)
  (define-key bc-mode-prefix-map "e"    'bc-end-of-line)
  (define-key bc-mode-prefix-map "f"    'bc-forward-comment)
  (define-key bc-mode-prefix-map "i"    'bc-set-tab-spacing)
  (define-key bc-mode-prefix-map "j"    'bc-indent-new-comment-line)
  (define-key bc-mode-prefix-map "k"    'bc-kill-line)
  (define-key bc-mode-prefix-map "l"    'bc-lower-flow)
  (define-key bc-mode-prefix-map "n"    'bc-narrower)
  (define-key bc-mode-prefix-map "o"    'bc-open-line)
  (define-key bc-mode-prefix-map "q"    'bc-fill)
  (define-key bc-mode-prefix-map "r"    'bc-raise-flow)
  (define-key bc-mode-prefix-map "s"    'bc-shorter)
  (define-key bc-mode-prefix-map "t"    'bc-taller)
  (define-key bc-mode-prefix-map "u"    'bc-raise)
  (define-key bc-mode-prefix-map "w"    'bc-wider)
  (define-key bc-mode-prefix-map "y"    'bc-yank)
  (define-key bc-mode-prefix-map "z"    'bc-squeeze))

(defvar bc-mode-basic-map nil ; NOCOLLECT
  "Basic keymap for BC minor mode.")
(if bc-mode-basic-map
    nil
  (setq bc-mode-basic-map (make-sparse-keymap))
  (define-key bc-mode-basic-map bc-mode-prefix bc-mode-prefix-map)
  (define-key bc-mode-basic-map "\C-xr;" 'bc-mode)
  (define-key bc-mode-basic-map [(meta return)] 'bc-fill)
;;;  (bc-mode-fsf-menu "Comments" bc-mode-basic-map)
  )

(defvar bc-mode-ext-prefix-map nil) ; NOCOLLECT
(if bc-mode-ext-prefix-map
    nil
  (setq bc-mode-ext-prefix-map (make-sparse-keymap))
  (define-key bc-mode-ext-prefix-map "\C-j" 'bc-indent-new-comment-line)
  (define-key bc-mode-ext-prefix-map "\C-k" 'bc-kill-line)
  (define-key bc-mode-ext-prefix-map "\C-m" 'bc-newline)
  (define-key bc-mode-ext-prefix-map "\C-o" 'bc-open-line)
  (define-key bc-mode-ext-prefix-map "\C-p" 'bc-yank-pop)
  (define-key bc-mode-ext-prefix-map "\C-w" 'bc-kill-comment)
  (define-key bc-mode-ext-prefix-map "\C-y" 'bc-yank)
  (define-key bc-mode-ext-prefix-map "\M-w" 'bc-copy-comment-comment-as-kill)
  (define-key bc-mode-ext-prefix-map "\M-y" 'bc-yank-pop)
  (define-key bc-mode-ext-prefix-map "\M-j" 'indent-new-comment-line)
  (define-key bc-mode-ext-prefix-map "\t"   'indent-according-to-mode))

;(setq bc-mode-extended-map nil) ; for debugging
(defvar bc-mode-extended-map nil ; NOCOLLECT
  "Extended keymap for BC minor mode.")
(if bc-mode-extended-map
    ()
  ;; XEmacs and Emacs 19 do this differently
  ;; Use `bc-funcall' to quiet the byte compiler about obsolete/unknown
  ;; functions.
  (cond
   ;; 				; Inherit from bc-mode-basic-map.
   ((fboundp 'set-keymap-parents)
    (setq bc-mode-extended-map (make-sparse-keymap))
    (bc-funcall 'set-keymap-parents bc-mode-extended-map bc-mode-basic-map))
   ((fboundp 'set-keymap-parent)
    (setq bc-mode-extended-map (make-sparse-keymap))
    (bc-funcall 'set-keymap-parent bc-mode-extended-map bc-mode-basic-map))
   (t (setq bc-mode-extended-map (cons 'keymap bc-mode-basic-map))))
  ;; add bindings
  (substitute-key-definition 'beginning-of-line 'bc-beginning-of-line
			     bc-mode-extended-map global-map)
  (substitute-key-definition 'end-of-line 'bc-end-of-line
			     bc-mode-extended-map global-map)
  (substitute-key-definition 'indent-new-comment-line
			     'bc-indent-new-comment-line
			     bc-mode-extended-map global-map)
  (define-key bc-mode-extended-map bc-mode-ext-prefix bc-mode-ext-prefix-map)
  (define-key bc-mode-extended-map "\M-B" 'bc-backward-comment)
  (define-key bc-mode-extended-map "\M-D" 'bc-lower)
  (define-key bc-mode-extended-map "\M-E" 'bc-forward-comment)
  (define-key bc-mode-extended-map "\M-F" 'bc-forward-comment)
  (define-key bc-mode-extended-map "\M-K" 'bc-lower-flow)
  (define-key bc-mode-extended-map "\M-L" 'bc-lower-flow)
  (define-key bc-mode-extended-map "\M-N" 'bc-narrower)
  (define-key bc-mode-extended-map "\M-Q" 'bc-fill)
  (define-key bc-mode-extended-map "\M-R" 'bc-raise-flow)
  (define-key bc-mode-extended-map "\M-S" 'bc-shorter)
  (define-key bc-mode-extended-map "\M-T" 'bc-taller)
  (define-key bc-mode-extended-map "\M-U" 'bc-raise)
  (define-key bc-mode-extended-map "\M-W" 'bc-wider)
  (define-key bc-mode-extended-map "\M-Z" 'bc-squeeze)

  (define-key bc-mode-extended-map "\M-y" 'bc-yank-pop)
  (define-key bc-mode-extended-map "\t" 'bc-indent-according-to-mode))


;;;;========================================================================
;;;_  * Bug Reporting:

(eval-when-compile (require 'reporter))

(defvar bc-bug-reporting nil
  "Non-nil during bc-submit-bug-report.")
;;;_    | bc-submit-bug-report
(defun bc-submit-bug-report ()
  "Submit, via mail, a bug report on bc-mode."
  (interactive)
  (let ((reporter-prompt-for-summary-p t)
	varlist salutation
	(bc-bug-reporting t))
    (setq varlist '(major-mode
		    kill-whole-line
		    fill-column
		    auto-fill-function

		    bc-added-newlines-counter
		    bc-banner-bottom-border
		    bc-banner-bottom-space
		    bc-banner-top-border
		    bc-banner-top-space
		    bc-bug-last-command
		    bc-bug-reporting
		    bc-bug-save-column
		    bc-bug-save-line
		    bc-bug-save-point
		    bc-code-width-threshold
		    bc-comment-end
		    bc-comment-looking-at-regexp
		    bc-comment-regexp
		    bc-comment-start
		    bc-delims-length
		    bc-effective-end
		    bc-effective-start
		    bc-enable-auto-fill-advice
		    bc-extra-lines
		    bc-fb-debug
		    bc-fb-lines
		    bc-fb-next-width
		    bc-fb-prev-fill-column
		    bc-fb-widest-is-one
		    bc-fb-width
		    bc-fill-buffer-cache
		    bc-fill-count
		    bc-format-commands
		    bc-keymap-level
		    bc-language
		    bc-mode
		    bc-mode-date
		    bc-mode-doc
		    bc-mode-maintainer-address
		    bc-mode-version
		    bc-parse-line-cache
		    bc-phantom-column
		    bc-pref-first-line
		    bc-right-margin
		    bc-G-rm-minus-stuff
		    bc-saved-delims
		    bc-scratch-buffer
		    bc-show-fill-count
		    bc-start-first-char
		    bc-start-marker
		    bc-tab-stops
		    filladapt-mode

		    (t . bc-bug-get-user-buffer)
		    ))
    (setq salutation "
Congratulations! You are about to report a bug in BC!

Please check the end of this message to see that it contains the
text you were editing.  Then make a concise and accurate summary
of what happened and mail it to the address above.
----------------------------------------------------------- ")

    (require 'reporter)
    (switch-to-buffer (get-buffer-create " bc-msg-buffer"))
    (erase-buffer)
    (fundamental-mode)
    (delete-other-windows)
    (insert "
Please read this first:
----------------------

BC-mode is very sensitive to the text you are editing.  For this
reason, `bc-submit-bug-report' tries to copy a bit of the buffer near
where you were editing.  For this to be useful, before submitting a
bug report you must 

  (1)  Position the cursor inside the comment where you experienced
       the error.
  (2)  Run the BC command that failed.
  (3)  If that altered the buffer, type `"
	    (substitute-command-keys "\\[advertised-undo]") "' \(`undo'\).")
      
    (if (y-or-n-p "I have done all that and I'm ready to mail! ")
	(progn
	  (bury-buffer)
	  (let ((emacs-lisp-mode-hooks nil)
		(orig-filladapt (default-value filladapt-mode)))
	    (bc-dummy emacs-lisp-mode-hooks)	      ; quiet the byte compiler
	    (setq-default filladapt-mode nil)
	    (reporter-submit-bug-report bc-mode-maintainer-address
					(concat "bc-mode.el " bc-mode-version)
					varlist
					'bc-bug-save-point nil
					salutation)
	    (setq-default filladapt-mode orig-filladapt)))
      (bury-buffer)
      (error "Bug report aborted"))
    ))

(defvar bc-bug-save-point nil
  "Gets value of (point) to pass to reporter-submit-bug-report.")
(defvar bc-bug-save-column nil
  "Gets column at (point) to pass to reporter-submit-bug-report.")
(defvar bc-bug-save-line nil
  "Gets a copy of the line containing point to pass to
reporter-submit-bug-report." )
(defvar bc-bug-last-command nil
  "Contains the most recent BC command to pass to reporter-submit-bug-report.")

(defsubst bc-bug-save-command ()
  (if (not bc-bug-reporting)
      (setq bc-bug-last-command this-command)))
;
;;;_    | bc-bug-save-point
(defun bc-bug-save-point ()
  (save-excursion
    (set-buffer reporter-eval-buffer)
    (setq bc-bug-save-point (point)
	  bc-bug-save-column (current-column)
	  bc-bug-save-line (buffer-substring 
			    (save-excursion (beginning-of-line) (point))
			    (save-excursion (end-of-line) (point))))
    (set-text-properties 0 (length bc-bug-save-line) nil bc-bug-save-line)))
;;;_    | bc-bug-get-user-buffer
(defun bc-bug-get-user-buffer (sym &optional mail-buffer)
  (or mail-buffer (setq mail-buffer		; Old versions of reporter
			(current-buffer)))	; don't supply mail-buffer.
  (let (nocomment range beg end)
    (save-excursion
      (setq nocomment
	    (catch 'failure
	      (save-excursion
		(set-buffer reporter-eval-buffer)
		(setq range (bc-find-range))	; See if point is in a comment.
		(goto-char (car (car range)))	; If so, get buffer contents in
		(forward-line -10)		; the vicinity.
		(setq beg (point))		; 
		(goto-char (car (cdr range)))	; If there are other comments
		(forward-line -2)		; nearby, just copy to those,
		(setq beg (max beg (point)))	; otherwise ten lines in either
		(goto-char (cdr (car range)))	; direction.
		(forward-line 10)
		(setq end (point))
		(goto-char (cdr (cdr range)))
		(forward-line 2)
		(setq end (min end (point))))
	      nil))
      (if (not nocomment) nil			; If there was no comment, just
	(save-excursion				; grab 20 lines around point.
	  (set-buffer reporter-eval-buffer)
	  (save-excursion
	    (forward-line -10)
	    (setq beg (point)))
	  (save-excursion
	    (forward-line 10)
	    (setq end (point)))))

      (set-buffer mail-buffer)
      (insert (make-string 64 ?=) ?\n)
      (insert "Code Buffer:\n" (make-string 64 ?-) ?\n)
      (insert
       (save-excursion
	 (set-buffer reporter-eval-buffer)
	 (buffer-substring beg end)))
      (if (bolp) nil (insert "<EOB>\n"))
      (insert (make-string 64 ?=) ?\n)
      (insert "Scratch Buffer:\n" (make-string 64 ?-) ?\n)
      (insert-buffer (bc-get-buffer-create))
      (goto-char (point-max))
      (insert (make-string 64 ?=) ?\n)
      )))
 
;;;;========================================================================
;;;_  * Internal variables:

(defconst bc-format-commands
  '(bc-fill bc-raise bc-raise-flow bc-lower bc-lower-flow bc-taller
	    bc-shorter bc-narrower bc-wider))

(defvar bc-comment-start nil
  "Default comment-start string.")
(make-variable-buffer-local 'bc-comment-start)

(defvar bc-comment-end nil
  "Default comment-end string.")
(make-variable-buffer-local 'bc-comment-end)

(defvar bc-faux-comment-end nil)
(make-variable-buffer-local 'bc-faux-comment-end)

(defvar bc-start-first-char nil
  "The first character of `comment-start'.

This is necessary for two reasons.  The first is that
`bc-comment-regexp' matches one character before the start delimiter,
and it must be different than the first character of that.  Therefore,
we must be careful not to move a comment to a line where the code ends
in that character.

It is further neccessary because if the comment-start string is all
the same character, you can add another and the whole string still
begins with comment-start.  If a line of code were to end with this
character, we must be sure not to jamb a comment up against it, or
that character would become part of the start delimiter.")
(make-variable-buffer-local 'bc-start-first-char)

(defvar bc-comment-regexp nil
  "*Regular expression matching a comment in the language being
edited. It must match the entire comment including starting and ending
delimiters, plus the single character before the starting delimiter.
This extra char is to force `re-search-forward' to match all of the
delimiter, in cases where a delimiter char is repeated (e.g. more than
one semicolon in Lisp).  It can be assumed that all searches based on
this regexp will be confined to one line, so the regexp itself does
not need to be concerned with matching newline characters. (e.g. in
Lisp, the comment-end delimiter is a newline, but `bc-comment-regexp'
needn't try to match it.)  In addition, `bc-comment-regexp' must
contain a substring match which matches only the text of the comment,
excluding delimiters and leading whitespace.

This variable is normally set on entry to bc-mode to a value
appropriate to the current major mode.

For more information, type `\\[describe-variable] bc-mode-doc'.")
(make-variable-buffer-local 'bc-comment-regexp)

(defvar bc-comment-looking-at-regexp nil
 "*Regular expression matching a comment in the language being
edited.  It must match enough of the comment to determine if we are
looking at a right-margin comment.  

The reason this regexp is necessary is because `bc-comment-regexp'
matches more than just the comment.  Specifically, it matches one
character before the start delimiter.  In some cases, at the beginning
of a line for example, it isn't practical to match the previous
character, so this regexp is needed.

The simplest is for this regexp to be the same as `bc-comment-regexp'
minus the match for the character before the start delimiter.
This variable is normally set on entry to `bc-mode' to a value
appropriate to the current major mode.

For more information, type `\\[describe-variable] bc-mode-doc'.")
(make-variable-buffer-local 'bc-comment-looking-at-regexp)

(defvar bc-language nil 
  "Symbol indicating if `bc-mode' has ever been turned on in the
buffer.  If not, `bc-language' is nil, otherwise it is a symbol
denoting the language selected by the user from `bc-languages-alist'
OR t if the user supplied `bc-comment-regexp' explicitly.

For more information, type `\\[describe-variable] bc-mode-doc'.")
(make-variable-buffer-local 'bc-language)
(setq-default bc-language nil)

(defvar bc-mode nil
  "Non-nil if bc-mode is active.

For more information, type `\\[describe-variable] bc-mode-doc'.")
(make-variable-buffer-local 'bc-mode)
;;;(put 'bc-mode 'permanent-local t)

(condition-case nil				; bc-funcall to quiet
    (bc-funcall 'add-minor-mode 'bc-mode " BC")	; byte-compiler
    (error
     (or (assq 'bc-mode minor-mode-alist)
	 (setq minor-mode-alist (append minor-mode-alist
					(list '(bc-mode " BC")))))))
(setq-default bc-mode nil)

(defvar bc-scratch-buffer " *bc-scratch*"
  "Scratch buffer in which to do text filling for block comments.")

(defvar bc-show-fill-count nil
  "*Non-nil means display `bc-fill-count' after each BC command.")

(defvar bc-start-marker nil)
(setq bc-start-marker (make-marker))
(set-marker bc-start-marker nil)

(defvar bc-saved-fill-paragraph-function nil)
(make-variable-buffer-local 'bc-saved-fill-paragraph-function)

;;;;========================================================================
;;;_   o Persistent variables.
;;
;; These variables are used to save info between calls to BC commands.

(defvar bc-phantom-column nil
  "Phantom comment column.
Used by BC formatting commands when the previous command was also a BC
formatting command.")
(make-variable-buffer-local 'bc-phantom-column)

(defvar bc-pref-first-line nil
  "Line number of preferred top line of the current comment.  Used by
bc-shorter and bc-taller commands to preserve the original top of the
comment whenever the previous command was a BC command.")
(make-variable-buffer-local 'bc-pref-first-line)

(defvar bc-parse-line-cache nil
  "Cache for `bc-parse-line'.  This is an alist with keys the
buffer positions of the beginnings of lines that `bc-parse-line'
has previously scanned.  Values are character positions of code-end,
comment-start, etc.")

(defvar bc-fill-buffer-cache nil 
 "Cache for `bc-fill-buffer'.
This caches the results of each call to `bc-fill-buffer' so that
`bc-query-fill' can retrieve them later.  The structure is an array of
lists.  It is indexed by `fill-column', so the array size is chosen to
accomodate the widest forseable fill column (2 times `bc-right-margin')

If an element is nil, it means the buffer has not yet been filled at
that width.  Otherwise it is a list of the values of `bc-fb-lines',
`bc-fb-width', `bc-fb-widest-is-one', and `bc-fb-next-width' for that
value of `fill-column'.")

(defvar bc-added-newlines-counter 0
  "Used by `bc-cleanup-newlines' to decide whether newlines were added
on the current or a previous command.  This counter must be incremented
once at entry to any function that will call `bc-cleanup-newlines'.
This is usually done by calling `bc-bump-counters'.")

;;;;========================================================================
;;;_   o The Back Alley.
;;
;;;_    | Buffer local
;; These global variables get assigned copies of buffer-local
;; variables so that they will be available when we switch to the
;; scratch buffer

(defvar bc-G-code-buffer nil
  "The current buffer in which a BC command is operating.")

(defvar bc-G-tab-stops nil
  "A copy of the value of the buffer-local variable `bc-tab-stops'.")

;
;;;_    | Low level
;; These global variables are used to pass info around between low
;; level functions during a single command invocation.

(defvar bc-G-scratch-buffer nil
  "Scratch buffer in which to do text filling for block comments.")

(defvar bc-delims-length nil
  "Total width of the effective delimiters for the comment currently
being formatted.  Set by `bc-analyze-comment'.")

(defvar bc-G-rm-minus-stuff nil
  "Number of columns available for code and comment.  This is
`bc-right-margin' minus `bc-delims-length' for the comment currently
being formatted.  Set by `bc-analyze-comment'.")

(defvar bc-fill-count nil
  "Number of times a BC formatting command calls `bc-fill-buffer'
during a single invocation.  Used only for display of this expense.")

(defvar bc-fb-lines nil 
  "Number of lines resulting from last call to `bc-fill-buffer'.")

(defvar bc-fb-width nil 
  "Width of widest line resulting from last call to `bc-fill-buffer'.")

(defvar bc-fb-widest-is-one nil 
  "Non-nil means widest line resulting from last call to `bc-fill-buffer' 
contains no whitespace.")

(defvar bc-fb-next-width nil 
  "Set by `bc-fill-buffer'.  This is the next-wider value that
`fill-column' would have to be for another call of `bc-fill-buffer' to
have an effect (on the same text).")

(defvar bc-fb-prev-fill-column nil
  "Most recent value of `fill-column' to which `bc-fill-buffer' filled.
Used by `bc-fill-buffer-maybe'.")

(defvar bc-fb-debug nil
  "*Non-nil forces `bc-query-fill' to always call `bc-fill-buffer'.
For debugging.")

(defvar bc-effective-start nil
  "Effective comment-start string for the comment currently being formatted.")

(defvar bc-effective-end nil
  "Effective comment-end string for the comment currently being formatted.")

(defvar bc-banner-top-border nil
  "Non-nil means the comment currently being formatted is a banner
comment.  In this case, it is a list.  The car is a single char that
will be repeated to form the top and bottom banners.  The cdr is
non-nil if this is a `hard' border.")

(defvar bc-banner-top-space 0
  "Non-zero means the banner comment currently being formatted has an
initial blank line.")

(defvar bc-banner-bottom-space 0
  "Non-zero means the banner comment currently being formatted has a
final blank line.")

(defvar bc-banner-bottom-border nil
  "Non-nil means the banner comment currently being formatted has a
bottom border.  Format is that of `bc-banner-top-borde'.")

(defvar bc-extra-lines 0
  "Number of extra lines the current comment needs to allow for banner
borders, etc.")

(defvar bc-saved-delims nil
  "List of delimiters from the comment currently being edited.
Set by `bc-analyze-comment'. Used by BC editing commands.")
 
;;;;======================================================================
;;;_  * Macros and Inlines:

(defsubst bc-do-command-init ()
  (bc-bug-save-command)
  (setq bc-G-code-buffer (current-buffer)
	bc-G-tab-stops bc-tab-stops
	bc-G-scratch-buffer (bc-get-buffer-create)))

(defsubst bc-count-columns (start end)
  "Count the number of columns between buffer positions START and END.
Returns the difference between the respective columns at START and
END.  \(If START and END are on the same line and have no tabs between
them, this is just their difference.\)"
  (save-excursion
    (- (progn (goto-char end) (current-column))
       (progn (goto-char start) (current-column)))))

(defsubst bc-code-width-this-line ()
  "Measures the length of the code on the current line, ignoring comments.
If a comment starts in the first column, it is treated as code."
  (let (code-width)
    (bc-oparse-line nil nil nil nil nil 'code-width)
    code-width))

(defsubst bc-bump-counters ()
  (setq
   bc-fill-count 0
   bc-added-newlines-counter (1+ bc-added-newlines-counter)))

(defsubst bc-flush-parse-cache ()
    (setq bc-parse-line-cache nil))

(defsubst bc-flush-caches ()
  (setq bc-parse-line-cache nil
	bc-fb-prev-fill-column nil)
  (if (memq last-command bc-format-commands) nil
    (setq bc-fill-buffer-cache nil)))

(defsubst bc-count-lines-1-fast (start end)
  "This function is like `count-lines' only it counts lines inclusively.
So, if START == END, or if END is at the beginning of the line, this
function returns one more than `count-lines' would.  

This function is also specialized to ignore `selective-display'.  The
return value is only useful for `bc-goto-line-fast'."
  (save-excursion
    (save-restriction
      (widen)
      (let ((new-start (progn (goto-char start) (beginning-of-line) (point)))
	    (new-end (progn (goto-char end) (beginning-of-line) (point)))
	    )
        (narrow-to-region new-start new-end)
        (goto-char (point-min))
        (- (buffer-size) (forward-line (buffer-size)) -1)))))

(defsubst bc-goto-line-fast (arg)
  "Goto line ARG, counting from line 1 at beginning of buffer.

This function is specialized to ignore  `selective-display'.  It is
for use with counts returned by `bc-count-lines-1-fast'."
  (save-restriction
    (widen)
    (goto-char 1)
    (forward-line (1- arg))))

(defsubst bc-remassq (key list)
  (delq nil (mapcar
	     (function (lambda (element)
			 (if (eq key (car element)) nil element))) list)))

(defmacro bc-pop (place)
  (list 'car (list 'prog1 place (list 'setq place (list 'cdr place)))))

;;; This is adapted from multiple-value-setq, stolen from cl-macs.el  
(defmacro bc-msetq (vars form)
  "(bc-msetq (SYM SYM...) FORM): collect multiple return values.  

FORM must return a list; the first N elements of this list are stored
in each of the symbols SYM in turn.  If any of the SYMs are nil, they
are ignored and the corresponding element of FORM is skipped.  This
allows selection of just the needed values in FORM.

Returns \(car FORM\).

This is analogous to the `multiple-value-setq' macro defined in
cl-macs.el, except that macro does not allow for nil symbols."
  (cond ((null vars) (list 'progn form nil))
	((null (cdr vars))
	 (if (car vars)
	     (list 'setq (car vars) (list 'car form))
	   (list 'car form)))
	(t
	 (let* ((temp 'bc-msetq-temp-symbol) (n 0) (firstvar (bc-pop vars)))
	   (list 'let (list (list temp form))
		 (list 'prog1
		       (if firstvar
			   (list 'setq firstvar (list 'car temp))
			 (list 'car temp))
		       (cons
			'setq
			(apply 'nconc
			       (mapcar (function
					(lambda (v)
					  (if v
					      (list v (list
						       'nth
						       (setq n (1+ n))
						       temp))
					    (setq n (1+ n))
					    nil)))
				       vars)))))))))

(defmacro bc-parse-comment () '(car (bc-parse-line)))

(defmacro bc-parse-code () '(nth 1 (bc-parse-line)))

(defmacro bc-comment-msetq (&rest vars)
  "Parse the current line for a comment and return positions of delimiters, etc.

\(bc-comment-msetq BODY-B BODY-E S-DELIM-B E-DELIM-E S-DELIM-E E-DELIM-B\)

SETs values into the symbols according to their position in the
argument list.  Arguments are not evaluated, so unquoted symbols
should be passed.  Nil arguments may be used as placeholders when some
values are unneeded.

For example, the form

  \(bc-comment-msetq nil nil myvar1 myvar2\)

setq's myvar1 to the character position of the first character of the
comment start delimiter on the current line.  It also setq's the
position of the last character of the comment end delimiter into myvar2.

Returns BODY-B, the character position of the first nonwhite character
in the text of the comment on this line, or nil if there is no comment
on this line."
  (` (bc-msetq (, vars) (bc-parse-comment))))

(defmacro bc-comment-p () '(car (bc-parse-comment)))

(defmacro bc-code-msetq (&rest vars)
  "Parse the current line and return information about non-comment text.

\(bc-code-msetq CODE-P CODE-END CODE-WIDTH\)

SETs values into the symbols according to their position in the
argument list.  Arguments are not evaluated, so unquoted symbols
should be passed.  Nil arguments may be used as placeholders when some
values are unneeded.

For example, the form

  \(bc-code-msetq nil myvar1 myvar2\)

setq's myvar1 to the character position of the end of the
(non-comment) code on the current line.  It also setq's the width in
columns of the code into myvar2.

Returns non-nil if the current line contains code, nil otherwise."
  (` (bc-msetq (, vars) (bc-parse-code))))

(defmacro bc-code-width () '(nth 2 (bc-parse-code)))

(defmacro bc-code-p () (car (bc-parse-code)))

;;;;========================================================================
;;;_  * Advice:

; (defadvice indent-new-comment-line (around bc-indent-new-comment-line
;            activate)
;   "When called from `do-auto-fill' run `bc-indent-new-comment-line' if
; bc-mode is active, otherwise run the standard function."
;   (setq bc-bug-last-command 'do-indent-new-comment-line-advice)
;   (if (and bc-mode
; 	   (eq this-command 'self-insert-command))
;       (bc-indent-new-comment-line)
;     ad-do-it))

;;;;======================================================================

(defadvice do-auto-fill (around bc-do-auto-fill preactivate)
  "When bc-mode is on and the current line contains a comment, do
block-comment-sensitive filling using `bc-indent-new-comment-line'.
Otherwise do normal filling."

  (setq bc-bug-last-command 'do-auto-fill-advice)
  (cond
   ((or (not bc-mode) (< (current-column) fill-column))
      ad-do-it)
   ((eq this-command 'newline)
    nil)
   (t
    (bc-flush-caches)
    (bc-bump-counters)

    (let* ((current-point (point))
	   delim-b body-b delim-e
	   (comment-p (bc-oparse-line nil 'delim-b 'body-b nil 'delim-e))
	   first-line last-line
	   )

      (if (not comment-p)
	  ad-do-it
 
	(bc-msetq (first-line last-line) (bc-find-comment))
	(let*
	    ((past-comment (and (<= delim-e current-point) 
				(not (equal bc-comment-end ""))))
	     (in-comment (<= delim-b current-point))
	     
	     (first-line-line (bc-count-lines-1 1 first-line))
	     (last-line-line (bc-count-lines-1 1 last-line))
	     body-range body-last body-first extra-white white-point
	     unbreakable save-point
	     )

	  (cond					; If past the end delimiter
	   (past-comment			; just punt.
	    (message "Cannot auto-fill beyond comment."))

						;==============================
	   (in-comment				;          In comment.
	    (setq				;==============================
	     body-range (bc-analyze-comment first-line last-line)
	     body-first (nth 3 body-range)
	     body-last (nth 4 body-range)
	     extra-white (- (skip-chars-backward " \t"))
	     white-point (point)
	     save-point (bc-save-excursion current-point first-line
					   body-first body-last
					   last-line 'stick-to-delims)
	     )
	    (goto-char current-point)			; This is really
							; minimal. It only
							; tries once, not
	    (if (> (current-column) fill-column)	; repeatedly like
							; do-auto-fill.

		(let ((fill-point			; Determine where to
		       (let ((opoint (point)))		; split the line.
			 (save-excursion
			   (move-to-column (1+ fill-column))
			   (skip-chars-backward "^ \t\n")	; Move back to
			   (if (< body-b (point)) nil		; a word
			     (goto-char body-b)			; boundary.
			     (re-search-forward "[ \t]" opoint t)
			     (if (< (point) white-point) nil
			       (setq unbreakable t)))
			   (skip-chars-backward " \t")
			   (if (= (point) white-point)
			       (setq unbreakable t))	; Let fill-point be set
			   (point)))))			; to the place where we
							; end up.
		  (if (save-excursion
			(goto-char fill-point)		; If that place is not
			(not (<= (point) body-b)))	; the beginning of the
		      (progn				; line, break the line
			(goto-char fill-point)		; there.
			(bc-indent-new-comment-line)))))

	    (if unbreakable nil
	      (goto-line first-line-line)
	      (setq first-line (point))
	      (goto-line last-line-line)
	      (forward-line 1)
	      (setq last-line (point))			  ; bc-restore-
	      (bc-restore-excursion save-point first-line ; excursion doesn't
				    last-line)		  ; account for
	      (if (and (eq (car save-point) 'in-comment)  ; multiple whitespace
		       (eq (nth 1 save-point) 'in-text)	  ; chars, so we have
		       (not (cdr (nth 2 save-point))))	  ; to do that here.
		  (if (not (eolp))
		      (forward-char extra-white)
		    (insert (make-string extra-white ?\ ))
		    (end-of-line))))
	    );; end 'in-comment

	   (t					;==============================
	    ad-do-it				;          In code.
	    ))))))))				;==============================

;;;;==================================================================
;;;_  * Commands:
;;;_   o The mode
;;;_    | turn-on-bc-mode

;;;###autoload;_   : turn-on-bc-mode
(defun turn-on-bc-mode ()
  "Turn on bc-mode unconditionally.

For more information, type `\\[describe-variable] bc-mode-doc'."
  (interactive)
  (bc-do-command-init)
  (bc-mode 1))

;;;;==================================================================
;;;_    | bc-mode
;;;###autoload;_   : bc-mode
(defun bc-mode (&optional arg)
  "Minor mode for editing right-margin block-style comments.
bc-mode version: 1.1

To submit a problem report, type `\\[bc-submit-bug-report]'.  This automatically 
sets up a mail buffer with version information already added.  You
just need to add a description of the problem and send the message.

This minor mode provides commands that format right-margin block-style
comments in source code.  

COMMENTS THAT BEGIN IN THE FIRST COLUMN ARE NOT RECOGNIZED AS BLOCK COMMENTS.

See the documentation for the variable `bc-mode-doc' for more
information and on using the function `bc-mode' 
\(Do `\\[describe-variable] bc-mode-doc'\).

Commands:
\\{bc-mode-map}
Entry to this mode calls the value of `bc-mode-hook' if that value is
non-nil."

  (interactive "P")
  (bc-do-command-init)
  (make-local-variable 'bc-language)
  (let ((first-time (not (and (boundp 'bc-language) 
			      (symbolp bc-language)
			      bc-language)))
	prev-name
	language-names
	selected-name
	language-sym
	)					;==============================
    (cond					; ------ Called interactively.
						;==============================
     ((interactive-p)
      (cond					; If no prefix arg and bc has
       ((and (not arg) (not first-time))	; run before in this buffer
	(if bc-mode (bc-internal-turn-mode-off) ; then we don't need to prompt.
	  (cond
	   ((eq bc-language t)			; Use bc-comment-regexp.
	    (bc-internal-turn-mode-on bc-comment-regexp t))
	  
	   ((and bc-language (symbolp bc-language))
	    (bc-internal-turn-mode-on bc-language t)))))     ; Use bc-language.


       ((< (prefix-numeric-value arg) 0)	; Negative prefix: turn off.
	(bc-internal-turn-mode-off))

       (t					; If called with prefix arg, or
	(if (not (boundp 'bc-language))		; if this is first time, prompt
	    (setq bc-language nil))		; for syntax.
	(setq prev-name (if bc-language (symbol-name bc-language) 
			  (bc-guess-language)))
	(setq language-names (mapcar
			      (function (lambda (list) 
					  (cons (symbol-name (car list)) nil)))
			      bc-languages-alist))
	(setq language-names (cons '("TURN OFF BC-MODE") language-names))
	(setq selected-name
	      (completing-read (concat "Comment syntax ("
				       prev-name
				       "): ")
			       language-names nil t))
	(if (equal "" selected-name)
	    (if prev-name (setq selected-name prev-name)
	      (error "Please choose a comment syntax for BC to use.")))
	(cond
	 ((equal selected-name "TURN OFF BC-MODE")
	  (bc-internal-turn-mode-off))
	 (t
	  (setq language-sym (intern selected-name))
	  (bc-internal-turn-mode-on language-sym t)))
	)))
						;==============================
						; ------ Called from Lisp.
     (t						;==============================
      (cond
       ((or (stringp arg)			; Setting bc-comment-regexp
	    (and arg (symbolp arg)))		; explicitly. A symbol from
	(bc-internal-turn-mode-on arg))		; bc-languages-alist.

       ((< (prefix-numeric-value arg) 0)	; Negative prefix: turn off.
	(bc-internal-turn-mode-off))

       (t					; Arg is positive or nil, get
	(cond					; language from bc-language or
	 (first-time				; guess.
	  (if (setq selected-name (bc-guess-language))
	      (progn
		(setq language-sym (intern selected-name))
		(bc-internal-turn-mode-on language-sym))
	    (error "bc-mode: Cannot determine language syntax.")))
	 
	 ((eq bc-language t)			; Use bc-comment-regexp.
	  (bc-internal-turn-mode-on bc-comment-regexp))

	 ((and bc-language (symbolp bc-language))
	  (bc-internal-turn-mode-on bc-language))	; Use bc-language.

	 (t
	  (error 
	   "bc-mode Internal error 002.  Please submit a full bug report."))
	 ))))))

  (if bc-mode
      (run-hooks 'bc-mode-hook))
					; Use `bc-funcall' to quiet the byte
  (bc-funcall 'force-mode-line-update)	; compiler about obsolete functions in
  )					; XEmacs

;;;;======================================================================
;;;_   o Formatting
;;;_    | bc-fill
(defun bc-fill (&optional arg)
  "Block-fill the block comment surrounding the current line.  
The comment is taken to consist of all the text in the right-margin
comments on concecutive lines adjacent to the current line.  

COMMENTS THAT BEGIN IN THE FIRST COLUMN ARE NOT RECOGNIZED AS BLOCK COMMENTS.

The formatted comment is placed at the nearest tab stop unless a
prefix argument is given.  In that case, the comment is placed as near
as possible to its current location.

For more information, type `\\[describe-variable] bc-mode-doc'."

  (interactive "*P")
  (bc-do-command-init)
  (bc-fill-engine 0 nil arg)
  )

;;;;======================================================================
;;;_    | bc-fill-paragraph
(defun bc-fill-paragraph (&optional justify)
"Like M-q, but handle right-margin block comments.
If any of the current contains part of a block comment, fill the block
comment on this and surrounding lines.  The comment is taken to
consist of all the text in the right-margin comments on concecutive
lines adjacent to the current line.

COMMENTS THAT BEGIN IN THE FIRST COLUMN ARE NOT RECOGNIZED AS BLOCK COMMENTS.

If no block comment is on the current line, return nil.  Optional arg
JUSTIFY is ignored.

For further information, type 
`\\[describe-variable] bc-enable-fill-paragraph-function' or `\\[describe-variable] bc-mode-doc'."

  (interactive "*P")
  (bc-do-command-init)
  (if (bc-oparse-line)
      (progn (bc-fill-engine 0) t)))

;;;;======================================================================
;;;_    | bc-squeeze
(defun bc-squeeze ()
  "Block-fill the block comment surrounding the current line.  
The comment is squeezed to occupy the fewest columns as possible for
its height.

COMMENTS THAT BEGIN IN THE FIRST COLUMN ARE NOT RECOGNIZED AS BLOCK COMMENTS.

For more information, type `\\[describe-variable] bc-mode-doc'."

  (interactive "*")
  (bc-do-command-init)
  (bc-narrow-engine 0 t)
  )

;;;;======================================================================
;;;_    | bc-raise
(defun bc-raise (motion)
  "Raise the comment surrounding the current line by one or more lines.
The comment is not reformatted.  If necessary, blank lines are added
to host the comment lines.

COMMENTS THAT BEGIN IN THE FIRST COLUMN ARE NOT RECOGNIZED AS BLOCK COMMENTS.

With a prefix argument, raise the comment that many lines.

For more information, type `\\[describe-variable] bc-mode-doc'."

  (interactive "*p")
  (bc-do-command-init)
  (bc-fill-engine (- motion) nil t t)
  )

;;;;======================================================================
;;;_    | bc-raise-flow
(defun bc-raise-flow (motion)
  "Raise the comment surrounding the current line by one or more lines.
The comment is reformatted if necessary to fit in available space.
See the command `bc-fill'.

COMMENTS THAT BEGIN IN THE FIRST COLUMN ARE NOT RECOGNIZED AS BLOCK COMMENTS.

With a prefix argument, raise the comment that many lines.

For more information, type `\\[describe-variable] bc-mode-doc'."

  (interactive "*p")
  (bc-do-command-init)
  (bc-fill-engine (- motion) nil t)
  )

;;;;======================================================================
;;;_    | bc-lower
(defun bc-lower (motion)
  "Lower the comment surrounding the current line by one or more lines.
The comment is not reformatted.  If necessary blank lines are added
to host the comment lines.

COMMENTS THAT BEGIN IN THE FIRST COLUMN ARE NOT RECOGNIZED AS BLOCK COMMENTS.

With a prefix argument, lower the comment that many lines.

For more information, type `\\[describe-variable] bc-mode-doc'."

;;;Bound to keys: \"\\[bc-lower]\"."

  (interactive "*p")
  (bc-do-command-init)
  (bc-fill-engine motion nil t t)
  )

;;;;======================================================================
;;;_    | bc-lower-flow
(defun bc-lower-flow (motion)
  "Lower the comment surrounding the current line by one or more lines.
The comment is reformatted if necessary to fit in available space.
See the command `bc-fill'.

COMMENTS THAT BEGIN IN THE FIRST COLUMN ARE NOT RECOGNIZED AS BLOCK COMMENTS.

With a prefix argument, lower the comment that many lines.

For more information, type `\\[describe-variable] bc-mode-doc'."

;;;Bound to keys: \"\\[bc-lower]\"."

  (interactive "*p")
  (bc-do-command-init)
  (bc-fill-engine motion nil t)
  )

;;;;======================================================================
;;;_    | bc-taller
(defun bc-taller ()
  "Reformat the comment surrounding the current line so that it
occupies one less line if possible.  The comment will be raised or
lowered if necessary to fit in available space.  See the command
`bc-fill'.

COMMENTS THAT BEGIN IN THE FIRST COLUMN ARE NOT RECOGNIZED AS BLOCK COMMENTS.

For more information, type `\\[describe-variable] bc-mode-doc'."

  (interactive "*")
  (bc-do-command-init)
  (bc-narrow-engine 1 t)
  )

;;;;======================================================================
;;;_    | bc-shorter
(defun bc-shorter ()
  "Reformat the comment surrounding the current line so that it
occupies one more line if possible.  The comment will be raised or
lowered if necessary to fit in available space.  See the command
`bc-fill'.

COMMENTS THAT BEGIN IN THE FIRST COLUMN ARE NOT RECOGNIZED AS BLOCK COMMENTS.

For more information, type `\\[describe-variable] bc-mode-doc'."

  (interactive "*")
  (bc-do-command-init)
  (bc-narrow-engine -1 t)
  )

;;;;======================================================================
;;;_    | bc-narrower
(defun bc-narrower ()
  "Reformat the comment surrounding the current line so that it
occupies one less line if possible.  The comment will be raised or
lowered if necessary to fit in available space.  See the command
`bc-fill'.

COMMENTS THAT BEGIN IN THE FIRST COLUMN ARE NOT RECOGNIZED AS BLOCK COMMENTS.

For more information, type `\\[describe-variable] bc-mode-doc'."

  (interactive "*")
  (bc-do-command-init)
  (bc-narrow-engine 1 nil)
  )

;;;;======================================================================
;;;_    | bc-wider
(defun bc-wider ()
  "Reformat the comment surrounding the current line so that it
occupies one more line if possible.  The comment will be raised or
lowered if necessary to fit in available space.  See the command
`bc-fill'.

COMMENTS THAT BEGIN IN THE FIRST COLUMN ARE NOT RECOGNIZED AS BLOCK COMMENTS.

For more information, type `\\[describe-variable] bc-mode-doc'."

  (interactive "*")
  (bc-do-command-init)
  (bc-narrow-engine -1 nil)
  )

;;;;======================================================================
;;;_   o Editing
;
;;;_    | bc-kill-comment
(defun bc-kill-comment ()
  "Kills the text of the block comment on lines around point,
including delimiters.  Text of the comment (without delimiters) is
appended to the kill-ring.

For more information, type `\\[describe-variable] bc-mode-doc'."

  (interactive "*")
  (bc-do-command-init)
  (let (
	(save-column (current-column))
	(save-line (bc-count-lines-1-fast 1 (point)))
	first-line last-line
;	body-range
	body-first-line body-last-line
	result
	)
    (bc-flush-caches)
    (setq 
     bc-added-newlines-counter (1+ bc-added-newlines-counter)
     result
     (catch 'failure
       (unwind-protect
	   (progn
	     (bc-msetq (first-line last-line) (bc-find-comment))
	     (bc-msetq (body-first-line body-last-line)
		       (bc-analyze-comment first-line last-line))
; 	     (setq ;range (bc-find-comment)
; 		   ;first-line (car range)
; 		   ;last-line (cdr range)
; 		   body-range (bc-analyze-comment first-line last-line)
; 		   body-first-line (car body-range)
; 		   body-last-line (nth 1 body-range))
;;;		   last-line-line (bc-count-lines-1 (point-min) last-line))
	     (bc-get-text body-first-line body-last-line)
	     (set-buffer bc-G-scratch-buffer)
	     (kill-region 1 (point-max))
	     (set-buffer bc-G-code-buffer)
	     (bc-delete-text first-line last-line))
	 
	 (set-buffer bc-G-code-buffer);; cleanup
	 (bc-goto-line-fast save-line)
; 	 (goto-char (point-min))
; 	 (forward-line (1- save-line))
;;;	 (goto-line save-line)
	 (move-to-column save-column));; end unwind-protect
       nil));; end setq
    
    (if result					; was there a problem?
	(progn (beep t) (message "%s" result))
      (save-excursion
	(bc-cleanup-newlines first-line (progn (goto-line last-line) 
					       (end-of-line) (point)))))))

;;;;======================================================================
;;;_    | bc-copy-comment-comment-as-kill
(defun bc-copy-comment-comment-as-kill ()
  "Copy the text of the block comment on lines around point.
Text of the comment \(without delimiters\) is appended to the kill-ring.

For more information, type `\\[describe-variable] bc-mode-doc'."

  (interactive "*")
  (bc-do-command-init)
  (let (
;;;	(scratch-buffer (bc-get-buffer-create))
;;;	range
	body-range first-line last-line
	result
	)

    (setq 
     result
     (catch 'failure
       (save-excursion
	 (bc-flush-caches)
	 (bc-msetq (first-line last-line) (bc-find-comment))
	 (setq ;range (bc-find-comment)
	       body-range (bc-analyze-comment first-line last-line))
	 (bc-get-text (car body-range) (nth 1 body-range))
	 (set-buffer bc-G-scratch-buffer)
	    (kill-region 1 (point-max)))
	nil)) ; end setq result
    
    (if result ; was there a problem?
	(progn
	  (beep t)			; signal failure
	  (message "%s" result)
	  )
      )))
    
;;;;======================================================================
;;;_    | bc-yank
(defun bc-yank ()
  "Yanks the most recently killed text and formats it as a right-margin
block comment on and around the current line.

For more information, type `\\[describe-variable] bc-mode-doc'."

  (interactive "*")
  (bc-do-command-init)
  (let*
      (
;;;       (code-buffer (current-buffer))
;;;       (current-point (point))
       (save-column (current-column))
       (save-line (bc-count-lines-1-fast (point-min) (point)))
;;;       (scratch-buffer (bc-get-buffer-create))
       column top
;;;       lim-top lim-bottom
       fit-area
       result
       )

    (save-excursion
      (bc-default-analysis-vars)
      (bc-flush-caches)
      (bc-bump-counters)
      (setq
       result
       (catch 'failure
	 (set-buffer bc-G-scratch-buffer)
	 (erase-buffer)
	 (insert (car kill-ring-yank-pointer))
	 (set-buffer bc-G-code-buffer)
	 (setq  bc-parse-line-cache nil
		bc-pref-first-line save-line)
	 
	 (setq fit-area (bc-fit-move 1))
	 (setq column (car fit-area))
	 (setq top (car (cdr fit-area)))
	 
	 (setq bc-phantom-column column)
	 (bc-moveit top nil nil column (nth 2 fit-area)  
		    (nth 3 fit-area) t)
	 (bc-set-pref-first-line)
	 (bc-goto-line-fast save-line)
	 (move-to-column save-column)
	 nil)))
    
    (if result						; was there a problem?
	(progn
	  (beep t)					; signal failure
	  (message "%s" result)
	  )
      (if bc-show-fill-count (message "Fill count: %d" bc-fill-count)))
    ))

;;;;======================================================================
;;;_    | bc-yank-pop
(defun bc-yank-pop (arg) 
  "Replace just-yanked block comment with the next chunk of text in
the kill ring.  This command is allowed only immediately after a
`bc-yank', `bc-yank-pop', `yank', or `yank-pop'.  If one of the latter
two, this just calls `yank-pop'.

With no argument, the previous kill is inserted.
With argument n, the n'th previous kill is inserted.
If n is negative, this is a more recent kill.

The sequence of kills wraps around, so that after the oldest one
comes the newest one.

For more information, type `\\[describe-variable] bc-mode-doc'."

  (interactive "*p")
  (bc-do-command-init)
  (let* (
;;;	 (code-buffer (current-buffer))
;;;	 (current-point (point))
	 (save-column (current-column))
	 (save-line (bc-count-lines-1-fast (point-min) (point)))
;;;	 (scratch-buffer (bc-get-buffer-create))
	 column top first-line last-line fit-area result new-range
;;;	 range 
	 )

    (bc-default-analysis-vars)
    (bc-flush-caches)
    (bc-bump-counters)
    
    (if (not (eq last-command 'bc-yank))	; This will barf if
	(yank-pop arg)				; last-command not yank.
      (save-excursion
	(setq
	 result
	 (catch 'failure
	   (setq this-command 'bc-yank)
	   (rotate-yank-pointer arg)
	   (bc-msetq (first-line last-line) (bc-find-comment))
;;	   (setq range (bc-find-comment))
	   (set-buffer bc-G-scratch-buffer)
	   (erase-buffer)
	   (insert (car kill-ring-yank-pointer))
	   (set-buffer bc-G-code-buffer)
	   (setq bc-pref-first-line save-line)
;;		   first-line (car range)
;;		   last-line (cdr range))

	   (bc-goto-line-fast bc-pref-first-line)
	   (setq fit-area (bc-fit-move 1 first-line last-line))
	   (setq column (car fit-area))
	   (setq bc-phantom-column column)
	   (setq top (car (cdr fit-area)))
	   
	   (set-buffer bc-G-code-buffer)
	   (setq new-range (bc-moveit top first-line last-line column
				      (nth 2 fit-area) (nth 3 fit-area) t))
	   (bc-goto-line-fast save-line)
	   (move-to-column save-column)
	   nil)) ;; end setq result
	
	(if result			; was there a problem?
	    (progn (beep t) (message "%s" result))
	  (save-excursion
	    (bc-cleanup-newlines (nth 2 new-range) (nth 3 new-range))
	    (bc-set-pref-first-line))
	  (if bc-show-fill-count (message "Fill count: %d" bc-fill-count)))
	))))

;;;;======================================================================
;;;_    | bc-newline
(defun bc-newline (&optional arg)
  "Insert a newline in either the code or the comment.
Break line at point and indent, continuing comment if within one.
This function is like `indent-new-comment-line' but  but is
context-sensitive to block comments.

With prefix argument, insert that many newlines. \(Negative
argument is not yet supported.\)

For more information, type `\\[describe-variable] bc-mode-doc'."

  (interactive "*P")
  (bc-do-command-init)
  (bc-insert-line arg '(newline) 'newline 'newline 'trim))

;;;;======================================================================
;;;_    | bc-indent-new-comment-line
(defun bc-indent-new-comment-line (&optional arg)
  "Break line at point and indent, continuing comment if within one.
This function is like `indent-new-comment-line' but  but is
context-sensitive to block comments.

With prefix argument, insert that many newlines before point. \(Negative
argument is not yet supported.\)

For more information, type `\\[describe-variable] bc-mode-doc'."

  (interactive "*P")
  (bc-do-command-init)
  (bc-insert-line arg 'indent-new-comment-line
		  '(newline indent-according-to-mode) 
		  '(newline delete-horizontal-space)))

;;;;======================================================================
;;;_    | bc-open-line
(defun bc-open-line (&optional arg)
  "Insert a newline in the code or comment and leave point before it.
This function is like `open-line' but is context-sensitive to
right-margin comments, depending on the current location of point.

With prefix argument, insert that many newlines from point. \(Negative
argument is not yet supported.\)

For more information, type `\\[describe-variable] bc-mode-doc'."

  (interactive "*p")
  (bc-do-command-init)
  (bc-insert-line arg '(open-line) 'open-line 'open-line 'trim))

;;;;======================================================================
;;;_    | bc-insert-line
(defun bc-insert-line (arg normal-command code-command comment-command
			   &optional trim-whitespace)
  "Insert a newline in the code or comment.  This does the actual work
for `bc-open-line', `bc-newline' `bc-indent-new-comment-line' and
`bc-split-line'."

  (if (< (prefix-numeric-value arg) 0)
      (error "Negative argument not supported yet."))
       
  (bc-flush-caches)
  (bc-bump-counters)

  (let* ((point (point))
	 delim-b body-b delim-e new-range first-line last-line
	 (save-line (bc-count-lines-1-fast 1 (point)))
	 (save-column (current-column))
	 (comment-p (bc-oparse-line nil 'delim-b 'body-b 'body-e 'delim-e))
	 )

    (setq this-command 'bc-insert-line)
						; If no comment on this line
    (if (not comment-p)				; then just do the regular
	(if (listp normal-command)		; command.
	    (funcall (car normal-command) arg)
	  (funcall normal-command))

      (unwind-protect				; Otherwise crank up the magic.
	  (progn

	    ; Should be able to use `below-only here for better
	    ; efficiency, but there is a bug when on the bottom banner
	    ; of a banner comment.
	       
	    (bc-msetq (first-line last-line) (bc-find-comment))
	    (let*
		(
		 (past-comment (and (<= delim-e point) 
				    (not (equal bc-comment-end ""))))
		 (in-comment (and (<= delim-b point)))

		 body-range body-last body-first
		 (current-line (progn (beginning-of-line) (point)))
		 (comment-lines-above (1- (bc-count-lines-1 first-line 
							    current-line)))
		 (comment-lines-below
		  (bc-count-lines-1 current-line last-line))
		 comment-point-column comment-point-line
		 new-delims saved-delims current-start-column max-code-width
		 current-start-delim current-start-delim-sans-white
		 current-end-delim current-end-delim-sans-white
		 (repeat (eq last-command 'bc-insert-line))
		 (counter -1)
		 (lines-needed nil)
		 (where nil) (extra 0)
		 )
	       
	      (setq arg (or arg 1))
	      (cond
	       (past-comment
		(error "Past comment"))		;==============================
	       (in-comment			;          In comment.
		(save-excursion			;==============================
		  (setq 
		   body-range (bc-analyze-comment first-line last-line t)
		   saved-delims (nthcdr comment-lines-above bc-saved-delims)
		   comment-point-column (bc-count-columns body-b point)
		   body-first (nth 3 body-range)
		   body-last (nth 4 body-range)
		   current-start-delim (if (< point body-b) 
					   (buffer-substring delim-b point)
					 (nth 1 (car saved-delims)))
		   current-start-delim-sans-white 
		   (if (string-match "[ \t]" current-start-delim)
		       (concat (substring current-start-delim 0
					  (match-beginning 0)) " ")
		     current-start-delim)
		   current-end-delim (nth 2 (car saved-delims))
		   current-end-delim-sans-white
		   (if (string-match "[^ \t]" current-end-delim)
		       (concat " " (substring current-end-delim
					      (match-beginning 0)))
		     "")
		   current-start-column (car (car saved-delims))
		   max-code-width (+ current-start-column 
				     bc-code-width-threshold) 
		   new-delims
		   (if trim-whitespace
		       (list current-start-column current-start-delim-sans-white
			     current-end-delim-sans-white)
		     (list current-start-column current-start-delim
			   current-end-delim)))
							; Fix up the
		  (setcar (cdr (car saved-delims))	; delimiters.
			  current-start-delim-sans-white)
		  (setq saved-delims (nconc (make-list arg new-delims)
					    saved-delims))
		
		  (setcar (cdr (car saved-delims)) current-start-delim)
		
		  (if (and (/= first-line body-first)	; Disallow insertion
			   (= current-line first-line))	; the top banner.
		      (error "Beginning of comment"))
		
		  ;; Here is kind of a kludge for a sticky problem. If we allow
		  ;; insertion in the bottom banner we get a mess. The bottom
		  ;; banner chars become part of the text, etc. But in
		  ;; languages with no comment-end (like Lisp), a blank comment
		  ;; line at the bottom looks like a bottom banner (one made up
		  ;; of repeated comment-start chars.) Worse, a single (or
		  ;; repeated) char after the start delimiter (no whitespace)
		  ;; looks like a bottom banner, though it may be part of the
		  ;; text.
		  ;; 
		  ;; We would like to be able to start a new paragraph in the
		  ;; comment with a command like M-j or C-c RET, so we need to
		  ;; allow those cases. The solution for now is to allow
		  ;; insertion in the bottom banner if it is a soft border.
		  ;; Perhaps someday we can clean up the delimiters for the
		  ;; ugly cases.

		  (if (and bc-banner-bottom-border	
			   (= current-line last-line)
			   (nth 1 bc-banner-bottom-border))
		      (error "End of comment"))
		
		  (bc-get-text current-line body-last)
		  (set-buffer bc-G-scratch-buffer)
		  (goto-char 1)
		  (move-to-column (max 0 comment-point-column))
		  (if (atom comment-command)
		      (funcall comment-command arg)
		    (funcall (car comment-command) arg)
		    (funcall (nth 1 comment-command)))
		  (setq 
		   comment-point-line (1- (bc-count-lines-1 1 (point)))
		   comment-point-column (current-column))
						; Make sure there is enough
		  (set-buffer bc-G-code-buffer)	; room below so that we don't
		  (save-excursion		; jam into another comment or a
		    (goto-char last-line)	; line of code that is too
		    (forward-line 1)		; wide.
		    (while (and (< counter arg)
				(not (bc-oparse-line))
				(not (< max-code-width
					(bc-code-width-this-line)))
				(not (eobp)))
		      (setq counter (1+ counter))
		      (forward-line 1))
						; Account for comment-free line
						; needed if we ran into another
		    (if (and (< counter arg)	; comment.
			     (not (bc-oparse-line)))
			(setq counter (1+ counter))
		      (setq extra 1)))
		  (if (= counter arg) nil
		    (setq lines-needed (- arg counter)
			  where (+ counter extra comment-lines-below)))
		  (setq
		   new-range
		   (bc-moveit current-line current-line last-line saved-delims
			      lines-needed where))
		  (goto-char current-line)
		  (while (< 0 comment-point-line)
		    (setq 
		     comment-point-line (1- comment-point-line)
		     saved-delims (cdr saved-delims))
		    (forward-line 1))
		  (setq save-line (bc-count-lines-1-fast 1 (point))
			saved-delims (car saved-delims))
		  (move-to-column (+ (car saved-delims)
				     (length (nth 1 saved-delims))
				     comment-point-column))
		  (setq save-column (current-column))
		  ))

						;==============================
						;          In code.
	       (t				;==============================
		(setq
		 body-range (bc-analyze-comment 
			     first-line last-line (not repeat))
		 saved-delims (nthcdr comment-lines-above bc-saved-delims)
		 body-first (nth 3 body-range)
		 body-last (nth 4 body-range))
		(bc-get-text current-line body-last)

		; If we are on the same line as the top banner, we will end up
		; duplicating it in saved-delims and scratch-buffer, so delete
		; it from scratch-buffer.

		(if (not (and (/= first-line body-first)
			      (= current-line first-line)))
		    nil
		  (set-buffer bc-G-scratch-buffer)
		  (goto-char 1)
		  (end-of-line)
		  (delete-region 1 (point))
		  (set-buffer bc-G-code-buffer))
		(bc-delete-text current-line last-line)
		(move-to-column save-column)
						; Commands like
						; newline-and-indent don't
						; support prefix arg, so we
		(if (atom code-command)		; will fake it.
		    (funcall code-command arg)
		  (funcall (car code-command) arg)
		  (funcall (nth 1 code-command)))
		(setq save-column (current-column)
		      save-line (bc-count-lines-1-fast 1 (point))
		      new-range (bc-moveit current-line nil nil saved-delims))
		))))
	(set-buffer bc-G-code-buffer)
	(bc-goto-line-fast save-line)
	(move-to-column save-column 'force))
      
      (save-excursion
	(bc-cleanup-newlines (nth 2 new-range) (nth 3 new-range))
	))))

;;;;======================================================================
;;;_    | bc-kill-line
(defun bc-kill-line (&optional arg)
  "Kill the rest of the code or comment on the current line.
This function is like `kill-line' but limits killing to either the
code or the comment, depending on the current location of point.

With prefix argument, kill that many lines from point. \(Negative
argument is not yet supported.\)

For more information, type `\\[describe-variable] bc-mode-doc'."

  (interactive "*P")
  (bc-do-command-init)
  (if (< (prefix-numeric-value arg) 0)
      (error "Negative argument not supported yet."))
  
  (bc-flush-caches)
  (bc-bump-counters)

  (let* ((point (point))
;;;	 (code-buffer (current-buffer))
	 delim-b body-b
;;;	 body-e
	 delim-e code-end first-line last-line
;;;	 (save-line (bc-count-lines-1 1 (point)))
	 (save-column (current-column))
	 (comment-p (bc-oparse-line 'code-end 'delim-b 'body-b nil 'delim-e))
	 new-range
	 )
    
    (if (not comment-p) 
	(progn
	  (kill-line arg)
	  (setq this-command 'bc-kill-line-plain))
      (unwind-protect
	  (progn
	    (bc-msetq (first-line last-line) (bc-find-comment))
	    (let* 
		(
;;;	       (scratch-buffer (bc-get-buffer-create))
		 (past-comment (and (<= delim-e point)
				    (not (equal bc-comment-end ""))))
		 (in-comment (and comment-p (<= delim-b point)))
		 ;;	       (range (bc-find-comment))
		 ;;		 (first-line (car range))
		 ;;		 (last-line (cdr range))
		 body-range body-last body-first
		 (current-line (progn (beginning-of-line) (point)))
		 (comment-lines-above (1- (bc-count-lines-1 first-line 
							    current-line)))
		 (comment-lines-below (bc-count-lines-1 current-line last-line))
		 comment-point-column
		 scratch-lines
		 saved-delims current-start-column current-start-delim
		 (last-was-kill (memq last-command '(bc-kill-line 
						     'bc-kill-line-plain)))
		 (repeat (eq last-command 'bc-kill-line))
		 (counter -1) 
		 (lines-needed nil) killable-lines
		 (where nil)
		 )
	    
	      (cond
	       (past-comment
		(error "Past comment"))		;==============================
	       (in-comment			;          In comment.
		(save-excursion			;==============================
		  (setq 
		   body-range (bc-analyze-comment first-line last-line t)
		   saved-delims (nthcdr comment-lines-above bc-saved-delims)
		   comment-point-column (bc-count-columns body-b point)
		   body-first (nth 3 body-range)
		   body-last (nth 4 body-range)
		   current-start-delim (if (< point body-b) 
					   (buffer-substring delim-b point)
					 (nth 1 (car saved-delims))))
		  (if (and bc-banner-top-border
			   (= current-line first-line))	; Disallow killing the
		      (error "Beginning of comment"))	; top banner.
		  (if (and bc-banner-bottom-border	
			   (= current-line last-line))	; Disallow killing the
		      (error "End of comment"))		; bottom banner.

		  (bc-get-text current-line body-last)
		  (set-buffer bc-G-scratch-buffer)
		  (setq scratch-lines (bc-count-lines-1 1 (point-max)))
		  (goto-char 1)
		  (move-to-column (max 0 comment-point-column))

		  ;; Bug fix: If point is after white in the comment,
		  ;; the white doesn't get preserved because
		  ;; bc-get-text does not bring trailing white to the
		  ;; scratch buffer. Fix this by intent-to-column'ing
		  ;; if current-column not equal to
		  ;; comment-point-column. Is this robust? Time will
		  ;; tell.

		  (if (< (current-column) comment-point-column)
		      (indent-to-column comment-point-column))
		  (let ((last-command (if last-was-kill 'kill-region nil)))
		    (kill-line arg))
		  (goto-char (point-max))
		  (if (not (bolp)) (insert ?\n))	; Since we have killed
		  (setq					; lines in the comment,
		   current-start-column			; we want the start
		   (car (car saved-delims))		; delimiter from this
		   saved-delims				; line but the end
		   (nthcdr (min				; delimiter from the
			    (- scratch-lines		; next unkilled line.
			       (bc-count-lines-1	; That is, unless the
				1 (point-max)))		; remaining line is the
			    (1- (length saved-delims)))	; bottom banner.
			   saved-delims))
		  (setcar (car saved-delims) current-start-column)
		  (if (and (= (length saved-delims) 1) bc-banner-bottom-border)
		      nil
		    (setcar (cdr (car saved-delims)) current-start-delim)))
		(setq
		 new-range (bc-moveit current-line current-line last-line 
				      saved-delims)))
	     
						;==============================
						;          In code.
	       (t				;==============================
		(setq body-range (bc-analyze-comment
				  first-line last-line (not repeat))
		      saved-delims (nthcdr comment-lines-above bc-saved-delims)
		      body-first (nth 3 body-range)
		      body-last (nth 4 body-range))
		(bc-get-text current-line body-last)
		(if (not (and (/= first-line body-first)
			      (= current-line first-line)))
							; If we are on the same
		    nil					; line as the top
		  (set-buffer bc-G-scratch-buffer)	; banner, we will end
		  (goto-char 1)				; up duplicating it in
		  (end-of-line)				; saved-delims and
		  (delete-region 1 (point))		; scratch-buffer, so
		  (set-buffer bc-G-code-buffer))	; delete it from
							; scratch-buffer.

		(if (and (not arg) (or (and kill-whole-line (bolp))
				       (<= code-end point))) (setq arg 1))
		(if (and arg (< 0 (prefix-numeric-value arg)))
		    (save-excursion
		      (goto-char last-line)		; Make sure there are
		      (forward-line 1)			; enough lines of code
		      (while (and (< counter arg)	; below so that we
				  (not (bc-oparse-line))	; don't suck the next
				  (not (eobp)))		; comment onto the
			(setq counter (1+ counter))	; bottom of this one.
			(forward-line 1))
		      (if (and (< counter arg)		; Account for
			       (not (bc-oparse-line)))	; comment-free line
			  (setq counter (1+ counter)))	; needed if we ran into
		      (if (= counter arg)		; another comment.
			  (setq killable-lines arg)
			(setq killable-lines
			      (min arg
				   (+ counter comment-lines-below))
			      lines-needed (- killable-lines counter)
			      where (- (+ counter comment-lines-below 1)
				       killable-lines)))))

		(bc-delete-text current-line last-line)
		(move-to-column save-column)
		(let ((last-command (if last-was-kill 'kill-region nil)))
		  (if arg (kill-line killable-lines)
		    (kill-line)))
		(setq 
		 save-column (current-column)
		 new-range (bc-moveit current-line nil nil saved-delims 
				      lines-needed where))
		))))
	(set-buffer bc-G-code-buffer)
	(move-to-column save-column)
	(setq this-command 'bc-kill-line))

      (save-excursion
	(bc-cleanup-newlines (nth 2 new-range) (nth 3 new-range))
	))))

;;;;======================================================================
;;;_    | bc-indent-according-to-mode
(defun bc-indent-according-to-mode ()
  "Indent line like `indent-according-to-mode' without disturbing comments.

For more information, type `\\[describe-variable] bc-mode-doc'."

  ;; This function has to do a surprising amount of struggling to
  ;; achieve a seemingly simple thing.  The idea is that we want to
  ;; have an indent-according-to-mode behavior, except that any
  ;; comment is left where it was.  In addition, the indentation
  ;; should be as though there is no comment present.  This is to
  ;; support cases where the user wants to add some code on lines
  ;; already containing comments.

  ;; The difficulty arises because the only indenting function we can
  ;; count on having is `indent-line-funtion'.  This typically indents
  ;; differently when the only thing on the line is a comment, and it
  ;; almost always messes up comment positions.

  ;; On top of all that, we want the cursor to end up in a reasonable
  ;; position, as it does with `indent-according-to-mode', but as
  ;; though there were no comment on the line.

  (interactive "*")
  (bc-do-command-init)
  (bc-flush-caches)
  (let* (code-end start-delim no-code temp
	 (copy-line "")
	 (current-column (current-column))
	 (mpoint (make-marker))
	 (mstart (make-marker))
	 (mend (make-marker))
	 (comment-p
	  (bc-oparse-line 'code-end 'start-delim nil nil nil nil 'no-code))
	 comment-column new-indent-column old-indent-column
	 )
    (unwind-protect
	(if (not comment-p)				 ; If this is a comment
	    (if (looking-at bc-comment-looking-at-regexp); that starts at bol,
		nil					 ; leave it alone.
	      (funcall indent-line-function))
	  
	  (set-marker mpoint (point))
	  (goto-char start-delim)
	  (setq comment-column (current-column))
	  (beginning-of-line)
	  (if no-code nil
	    (setq copy-line (buffer-substring (point) code-end)))
	  (skip-chars-forward " \t")
	  (setq old-indent-column (current-column))
	  (beginning-of-line)
	  (set-marker mstart (point))
	  (insert "\n")
	  (set-marker mend (point))
	  (forward-char -1)
	  (insert copy-line)
	  (beginning-of-line)
	  (funcall indent-line-function)
	  (setq new-indent-column (current-column))
	  (delete-region mstart mend)
	  
	  (if no-code
	      (if (< new-indent-column comment-column)	; Don't need to insert
		  (if (< current-column comment-column)	; any whitespace
		      (move-to-column new-indent-column 'force)
		    (goto-char mpoint))
		(beginning-of-line)			; Else move comment
		(delete-horizontal-space)		; enough to accomodate
		(indent-to-column new-indent-column)	; indentation.
		(if (< comment-column current-column)
		    (goto-char mpoint)))
	    
	    (beginning-of-line)		; If there is code
;	    (delete-horizontal-space)
	    (indent-to-column new-indent-column)
	    (setq temp (point))
	    (skip-chars-forward " \t")
	    (delete-region temp (point))
	    (bc-flush-parse-cache)			; The line has been
	    (bc-oparse-line nil 'start-delim)		; changed so flush
	    (goto-char start-delim)			; caches.
	    (delete-horizontal-space)
	    (indent-to-column comment-column)
	    (if (< current-column old-indent-column)
		(move-to-column new-indent-column 'force)
	      (goto-char mpoint))))
      
      (set-marker mpoint nil)
      (set-marker mstart nil)
      (set-marker mend nil))))

;;;;======================================================================
;;;_   o Motion
;;;_    | bc-beginning-of-line
(defun bc-beginning-of-line ()
  "Move to the beginning of the current comment or line.  

If point is in a right-margin comment, move to the beginning of its
text on the current line.  If point is outside a comment (or no
comment on the line), move to beginning of current line.

If this command is invoked twice in succession, point is moved to the
beginning of the current line.

For more information, type `\\[describe-variable] bc-mode-doc'."

  (interactive)
  (bc-do-command-init)
  (let ((point (point))
	delim-b comment-b 
	)
		
    (if (eq last-command 'bc-beginning-of-line)
	(beginning-of-line)
      (bc-flush-parse-cache)
      (if (bc-oparse-line nil 'delim-b 'comment-b)
	  (cond
	   ((< point delim-b)
	    (beginning-of-line))
	   (t
	    (goto-char comment-b)))
	(beginning-of-line)
	))))

;;;;======================================================================
;;;_    | bc-end-of-line
(defun bc-end-of-line ()
  "Move point to the end of the current comment or line.
If point is in a right-margin comment, move to the end of its text on
the current line.  If point is in code to the left of a comment, move
to end of the code.  If no comment on the current line, move to end of
line.

If this command is invoked twice in succession, point is moved to the
end of the current line.

For more information, type `\\[describe-variable] bc-mode-doc'."

  (interactive)
  (bc-do-command-init)
  (let ((point (point))
	code-e delim-b comment-e
;;;	comment-b delim-e
	)
		
    (if (eq last-command 'bc-end-of-line)
	(end-of-line)
      (bc-flush-parse-cache)
      (if (bc-oparse-line 'code-e 'delim-b nil 'comment-e)
	  (cond
	   ((< point delim-b)
	    (goto-char code-e))
	   (t
	    (goto-char comment-e)))
	(end-of-line)))))

;;;;======================================================================
;;;_    | bc-forward-comment
(defun bc-forward-comment (&optional count)
  "Move point forward ARG block comments (backward if ARG is negative).
Normally returns t.
If an end of the buffer is reached, point is left there
and nil is returned.

For more information, type `\\[describe-variable] bc-mode-doc'."

  (interactive "p")
  (bc-do-command-init)

  (bc-flush-parse-cache)
  (let* ((point (point))
	 body-b body-e
	 (comment-p (bc-oparse-line nil nil 'body-b 'body-e))
	 (before-comment-p (and comment-p (<= point body-b)))
	 (past-comment-p (and comment-p (<= body-e point)))
	 (range (and comment-p (bc-find-comment)))
	 (this-line (progn (beginning-of-line) (point)))
	 (forward (<= 0 count))
	 (direction (if forward 1 -1))
	 delim-start
	 first-line last-line in-first-line-p in-last-line-p move-on
	 )

    (setq count (abs count))

    (if range (setq first-line (nth 0 range)
		    last-line (nth 1 range)
		    in-first-line-p (= first-line this-line)
		    in-last-line-p (= last-line this-line)
		    move-on (if forward (and in-last-line-p past-comment-p)
			      (and in-first-line-p before-comment-p))))

    (if range
	(progn
	  (goto-char (if forward last-line first-line))
	  (if move-on nil
	    (bc-oparse-line nil nil 'body-b 'body-e)
	    (goto-char (if forward body-e body-b))
	    (setq count (1- count)))))

    (while (not (or (and (= direction 1) (eobp))
		    (and (= direction -1) (bobp))
		    (<= count 0)))
      (forward-line direction)
      (while (not (or (eobp) (bobp) (bc-oparse-line)))
	(forward-line direction))
      (if (or (eobp) (bobp)) nil
	(bc-msetq (first-line last-line) (bc-find-comment))
;;	  (setq range (bc-find-comment)
;;		first-line (car range)
;;		last-line (cdr range))
	(goto-char (if forward last-line first-line))
	(bc-oparse-line nil nil 'body-b 'body-e)
	(goto-char (if forward body-e body-b)))
      (setq count (1- count)))
    (if (or (eobp) (bobp)) nil
      (save-excursion
	(goto-char first-line)
	(bc-oparse-line nil 'delim-start)
	(goto-char delim-start)
	(message "Column: %d" (current-column))))))

;;;;======================================================================
;;;_    | bc-backward-comment
(defun bc-backward-comment (&optional count)
  "Move point backward ARG block comments (forward if ARG is negative).
Normally returns t.
If an end of the buffer is reached, point is left there
and nil is returned.

For more information, type `\\[describe-variable] bc-mode-doc'."

  (interactive "p")
  (bc-do-command-init)
  (bc-forward-comment (- count)))

;;;;======================================================================
;;;_   o Options
;;;_    | bc-set-tab-spacing
(defun bc-set-tab-spacing (&optional spacing quiet)
  "Set the tab spacing for BC formatting commands.
With prefix argument, set tab spacing to that.  Otherwise prompt.

BC-mode's tabbing commands use tab stops to position the comment.
These tab stops are kept in the variable `bc-tab-stops'.  To set
individual tab stops you have to use Emacs Lisp, but you can set all
tab stops to a uniform spacing with this function.  The previous list
of tab stops is lost.  The spacing size will be determined by SPACING,
except that a tab stop will be placed at column 1 instead of column
zero.

See the documentation for the variables `bc-tab-stops'
\(\\[describe-variable] bc-tab-stops\).
For more information, type `\\[describe-variable] bc-mode-doc'."

  (interactive "NTab Spacing: ")
  (bc-bug-save-command)
  (if (<= spacing 0) (error "Tab spacing must be positive."))
  (if (< bc-right-margin spacing)
      (error "Tab spacing must be less then the right-margin."))

  (let ((stoplist nil) (stop spacing))
    (while (<= stop bc-right-margin)
      (setq stoplist (cons stop stoplist))
      (setq stop (+ stop spacing)))
    (setq bc-tab-stops (cons 1 (nreverse stoplist))))

  (if (not quiet)
      (message "Tab stops in this buffer now = %s"
	       bc-tab-stops)))

;;;;========================================================================
;;;_    | bc-toggle-keymap-level
(defun bc-toggle-keymap-level (&optional on-off)
  "Toggle between BC's basic and extended keymaps.
With arg, turn extended keymap on if arg is positive, off otherwise.

You can select from two keymaps for bc-mode to use.
The `basic' map is a minimally intrusive set of bindings for BC's
most powerful functions.  The `extended' map adds additional
bindings for cursor motion commands.  These commands override some
standard commands, such as `beginning-of-line' \(bound to C-a, Home,
etc\).  BC's versions of these commands behave like the standard ones
when point is outside a block comment, and move within the comment
otherwise.

BC-mode's basic keymap binds these keys:
\\{bc-mode-basic-map}

BC-mode's extended keymap binds these keys:
\\{bc-mode-extended-map}"

  (interactive "P")
  (bc-bug-save-command)
  (setq bc-keymap-level
	(if (null on-off) (not bc-keymap-level)
	  (> (prefix-numeric-value on-off) 0)))

  (bc-select-keymap)
  (message "bc-mode now using %s keymap."
	   (if bc-keymap-level "extended" "basic")))

;;;;========================================================================
;;;_    | bc-toggle-auto-fill-advice
(defun bc-toggle-auto-fill-advice (&optional on-off)
  "Toggle block-comment-oriented auto-fill.
With arg, turn on if arg is positive, off otherwise.

Bc-mode can provide block-comment-oriented auto filling when
`auto-fill-mode' is turned on.  When it does, comments are filled with
as little disturbance to the source code as possible.  To use Emacs'
normal auto-filling, turn this option off.

Technical note: Bc-mode implements block-comment-oriented auto-filling
by way of advising the function `do-auto-fill'.  The variable
`bc-enable-auto-fill-advice' controls whether this advice is enabled.
This function first toggles that varible, then calls
`bc-sync-advice-enable' to update the state of the advice."

  (interactive "P")
  (bc-bug-save-command)
  (setq bc-enable-auto-fill-advice
	(if (null on-off) (not bc-enable-auto-fill-advice)
	  (> (prefix-numeric-value on-off) 0)))
  (bc-sync-advice-enable)
  (bc-select-keymap)			; To update the menu
  (message "Advice on `do-auto-fill' is now %s."
	   (if bc-enable-auto-fill-advice "enabled" "disabled")))

;;;;======================================================================
;;;_    | bc-set-margin
(defun bc-set-margin (arg)
  "Set `bc-right-margin' to current column, or to argument if given.
The variable `bc-right-margin' has a separate value for each buffer."
  (interactive "P")
  (bc-bug-save-command)
  (setq bc-right-margin (if (integerp arg) arg (current-column)))
  (message "bc-right-margin set to %d in this buffer" bc-right-margin))

;
 
;;;;======================================================================
;;;_  * Functions:
;;;_   o Utilities for turning on the mode
;;;_    | bc-guess-language
(defun bc-guess-language ()
  "Try to guess the language being edited by examining `major-mode'.
If the prefix of `major-mode' is one of the language symbols in
`bc-languages-alist, that name (a string) is returned, otherwise nil."
  (let* ((mode-name (symbol-name major-mode))
	 (lang-name (substring mode-name 0 (string-match "-mode" mode-name)))
	 (lang-sym (intern-soft lang-name))
	 (alt-lang-name (substring mode-name 0 (string-match "-" mode-name)))
	 (alt-lang-sym (intern-soft alt-lang-name))
	 )
    (or
     (if (assq lang-sym bc-languages-alist) lang-name)
     (if (assq alt-lang-sym bc-languages-alist) alt-lang-name))
    ))

;;;;========================================================================
;;;_    | bc-select-keymap
(defun bc-select-keymap ()
  "Select a minor mode map according to the value of `bc-keymap-level'.
Also rebuilds the menu to reflect the state of option variables, and to 
force the correct display of key bindings."
  (let ((alist-entry (assq 'bc-mode minor-mode-map-alist))
	)
    (if (fboundp 'set-buffer-menubar)
	;; For XEmacs, set the mode map to either the basic or the extended
	;; map.
	(setq
	 bc-mode-map
	 (if bc-keymap-level bc-mode-extended-map bc-mode-basic-map))
; 	 minor-mode-map-alist (bc-remassq 'bc-mode minor-mode-map-alist)
; 	 minor-mode-map-alist
; 	 (cons (cons 'bc-mode bc-mode-map) minor-mode-map-alist))


      ;; For FSF, make a fresh copy of the keymap without the menu, then add
      ;; the menu. This is a kludge to force the menu to update its list of key
      ;; bindings.

      (setq bc-mode-map
	    (copy-keymap (if bc-keymap-level bc-mode-extended-map
			   bc-mode-basic-map)))
      (bc-mode-fsf-menu "Comments" bc-mode-map)
      )
      (if alist-entry
	  (setcdr alist-entry bc-mode-map)
	(setq minor-mode-map-alist (cons (cons 'bc-mode bc-mode-map)
					 minor-mode-map-alist)))

    (bc-funcall 'force-mode-line-update)))

;;;;========================================================================
;;;_    | bc-sync-advice-enable
(defun bc-sync-advice-enable ()
  "Synchronize the enabled state of BC's advice on `do-auto-fill'.

Bc-mode defines advice on `do-auto-fill' to assist correct auto-filling of
block-comments.  This advice is enabled whenever the variable
`bc-enable-auto-fill-advice' is non-nil.  Use this function to update the state
of that advice to conform to the variable's current value."

  (if bc-enable-auto-fill-advice
      (ad-enable-regexp "^bc-")
    (ad-disable-regexp "^bc-"))
  (ad-activate-regexp "^bc-" 'compile))

;;;;========================================================================
;;;_    | bc-internal-turn-mode-off
(defun bc-internal-turn-mode-off ()
;;;  (bc-select-keymap)
  (setq bc-mode nil)
  (bc-install-menubar 'remove)
  (setq fill-paragraph-function bc-saved-fill-paragraph-function))

;;;;========================================================================
;;;_    | bc-internal-turn-mode-on
(defun bc-internal-turn-mode-on (symbol-or-regexp &optional show-message)
  "Turn on bc-mode in this buffer, using SYMBOL-OR-REGEXP as the syntax.

Variables `bc-comment-regexp', `bc-comment-start', and `bc-comment-end' are
initialized.

SYMBOL-OR-REGEXP must be either a symbol or a string.  If a symbol, it
should be one of the language keys in `bc-languages-alist'.  If a
string, it should contain a regular expression to be used for
`bc-comment-regexp'.

If SHOW-MESSAGE is non-nil, a message is displayed in the echo area.
This is useful when servicing an interactive call.

The buffer-local variables `comment-start' and `comment-end' are also
set to appropriate values, if possible.  If SYMBOL-OR-REGEXP is a
symbol, `comment-start' and `comment-end' are generated from the
`delim' strings in `bc-languages-alist'.  If SYMBOL-OR-REGEXP is a
string, `comment-start' and `comment-end' are left untouched and must
be set by the user."

  (let ((lang-info (if (symbolp symbol-or-regexp)
		       (or (assq symbol-or-regexp bc-languages-alist)
			   (error "bc-mode: Unknown language: %s" 
				  symbol-or-regexp))))
	delims
	regexp
	)

    (if (and (not lang-info) (not (stringp symbol-or-regexp)))
	(signal 'wrong-type-argument 
		(list '(or symbolp stringp) symbol-or-regexp)))

	; At this point, lang-info is either a symbol from bc-languages-alist,
	; or nil to indicate that a regexp was passed.

    (if lang-info
	(progn
	  (setq delims (assq 'delim (cdr lang-info))
		regexp (assq 'regexp (cdr lang-info)))
	  (if regexp (setq regexp (nth 1 regexp)))
	  (if (and delims (not regexp))
	      (setq regexp (bc-make-regexp (nth 1 delims) (nth 2 delims)))))
      (setq regexp symbol-or-regexp))

    (if (not (stringp regexp))
	(error "bc-mode: Unknown language: `%s'" symbol-or-regexp))

    (setq bc-comment-regexp regexp)
    (setq bc-comment-looking-at-regexp (bc-make-looking-at-regexp regexp))

    (if show-message
	(message "bc-mode set to `%s'." (car lang-info)))

    (set (make-local-variable 'bc-language) (if lang-info symbol-or-regexp t))
    (setq bc-mode t)
    (bc-select-keymap)
    (bc-install-menubar)
    (bc-sync-advice-enable)
    (if bc-tab-stops nil (bc-set-tab-spacing 8 'quiet))

					;======================================
					;   Set up `comment-start' and
					;   `comment-end' if not already set.
    (make-local-variable 'comment-start);======================================
    (make-local-variable 'comment-end)
    (if (not delims) nil			; These are redundant if
      (setq bc-comment-start			; bc-make-regexp was called,
	    (concat (nth 1 delims ) " ")	; but they don't hurt.
	    bc-comment-end
	    (if (nth 2 delims) (concat " " (nth 2 delims)) "")
	    bc-start-first-char (bc-analyze-comment-start))
      (if (and (boundp 'comment-start) (stringp comment-start))
	  nil
	(setq comment-start bc-comment-start 
	      comment-end bc-comment-end))
      (if (and (boundp 'comment-start-skip) (stringp comment-start-skip))
	  nil
	(setq comment-start-skip (bc-make-start-skip (nth 1 delims)))))

    (if (eq bc-saved-fill-paragraph-function 'bc-fill-paragraph)
	nil
      (setq bc-saved-fill-paragraph-function fill-paragraph-function))
    (if bc-enable-fill-paragraph-function
	(setq fill-paragraph-function 'bc-fill-paragraph))
    ))

;;;;========================================================================
;;;_    | bc-make-start-skip
(defun bc-make-start-skip (string)
  "Make a string suitable for `comment-start-skip'.
This is not used by BC itself, but for modes that do not set it, BC
will try to set it to something useful."

  (concat string "+[ \t]*"))

;;;;========================================================================
;;;_    | bc-make-regexp
;;;###autoload
(defun bc-mirror-char (char)
  (or (cdr (assq char '((?< . ?>) (?> . ?<)
			(?[ . ?]) (?] . ?[)
			(?{ . ?}) (?} . ?{)
			(?( . ?)) (?) . ?() )))
      char))

(defun bc-make-regexp (start &optional end)
  "Make a regular expression suitable for `bc-comment-regexp'.
Also sets `comment-start' and `comment-end' if they are unbound.

START must be a string containing the comment start delimiter
characters for the language being edited.  It should not contain any
whitespace.

END must be a string containing the comment end delimiter.  It should
not contain any whitespace.  If there are no special characters that
end a comment \(i.e., comments continue to end-of-line\), END need not
be supplied."

  (let* (
	 (startfirstchar (substring start 0 1))
	 (start-re (regexp-quote start))
	 end-re
	 )

    (setq bc-comment-start (concat start " ") 
	  bc-comment-end (if (and end (not (equal end ""))) (concat " " end) "")
	  bc-start-first-char (aref start 0))

    (make-local-variable 'comment-start)
    (make-local-variable 'comment-end)
    (if (and (boundp 'comment-start) (stringp comment-start))
	nil
      (setq comment-start bc-comment-start 
	    comment-end bc-comment-end))
    (if (and (boundp 'comment-start-skip) (stringp comment-start-skip))
	nil
      (setq comment-start-skip (bc-make-start-skip start)))


    (if (and end (> (length end) 0))
	(setq end-re (concat (regexp-quote (substring end 0 1))
			     "+"
			     (regexp-quote (substring end 1)))
	      bc-faux-comment-end nil)
      (setq bc-faux-comment-end
	    (regexp-quote (concat (reverse (mapcar 'bc-mirror-char start))))))


    (concat "[^" startfirstchar "]" start-re "+[ 	]*\\(.*\\)" end-re)))
		     

(defun bc-old-make-regexp (start &optional end)
  "Make a regular expression suitable for `bc-comment-regexp'.
Also sets `comment-start' and `comment-end' if they are unbound.

START must be a string containing the comment start delimiter
characters for the language being edited.  It should not contain any
whitespace.

END must be a string containing the comment end delimiter.  It should
not contain any whitespace.  If there are no special characters that
end a comment \(i.e., comments continue to end-of-line\), END need not
be supplied.

This currently will not work if either argument is longer than two
characters. \(Sure wish Emacs Lisp had the Perl substitution
operator!\)"

  (let* ((startlen (length start))
	 (endlen (length end))
	 (startstartchar (aref start 0))
	 (startstart (bc-protect-char startstartchar))
	 (startend (bc-protect-char (if (> startlen 1) 
					(aref start (1- startlen)))))
	 (endstart (if (> endlen 0) 
		       (concat (bc-protect-char (aref end 0)) "+")
		     ""))
	 (endend (bc-protect-char (if (> endlen 1)
				      (aref end (1- endlen)))))
	 )

    (setq bc-comment-start (concat start " ") 
	  bc-comment-end (concat " " end)
	  bc-start-first-char (bc-analyze-comment-start))

    (make-local-variable 'comment-start)
    (make-local-variable 'comment-end)
    (if (and (boundp 'comment-start) (stringp comment-start))
	nil
      (setq comment-start bc-comment-start 
	    comment-end bc-comment-end))
    (if (and (boundp 'comment-start-skip) (stringp comment-start-skip))
	nil
      (setq comment-start-skip (bc-make-start-skip start)))


    (concat "[^" (char-to-string startstartchar) "]" 
	    startstart startend 
	    "+[ \t]*\\(.*\\)" 
	    endstart endend)))
;
;;;_    | bc-protect-char
(defun bc-protect-char (char)
  (if (null char)
      ""
    (if (memq char '(?. ?* ?+ ?[ ?] ?^ ?$ ?\\ ))
	(concat "\\" (char-to-string char))
      (char-to-string char))))

;;;;======================================================================
;;;_    | bc-analyze-comment-start
(defun bc-analyze-comment-start ()
  "Return the first character of `bc-comment-start'.
This used to be more complex.  It doesn't really need to be a separate
function now."
  (aref bc-comment-start 0))

;;;;========================================================================
;;;_    | bc-make-looking-at-regexp
(defun bc-make-looking-at-regexp (string)

  "Return a regular expression suitable for `bc-comment-looking-at-regexp'.  

STRING should contain a regular expression appropriate for
`bc-comment-regexp'.  It will be parsed to remove the pattern at the
beginning that matches a character before the comment delimiter."

; Have you ever tried to parse a regexp with another regexp?
; Kids, don't try this at home!  

; This regexp (the first arg to `string-match') merits some
; explanation: It looks for a left-bracket followed by one or more
; non-right-bracket characters followed by a right bracket.  The whole
; pattern is anchored to the beginning of the string.

  (if (string-match "^\\[[^]]+\\]" string)
      (substring string (match-end 0))))

;;;;======================================================================
;;;_   o The engines
;;;_    | bc-fill-engine
(defun bc-fill-engine (motion &optional squeeze no-tab no-reshape)
  "Block-fill the block comment surrounding the current line, with
optional movement up or down in the buffer.  The comment is taken to
consist of all the text in the right-margin comments on concecutive
lines adjacent to the current line.  If 'motion' is zero, text is
paragraph filled to width (columns) nearest its present width that
will fit in the space available to the right of the code.  If 'motion'
is nonzero, attempt to move the comment down by that many lines (up if
negative)."

  (let*
      (
       (current-point (point))
       top lim-top save-point no-code
       first-line last-line 
       body-first-line body-last-line
       column initial-width initial-column
       lines-needed where why result
       (old-first-marker (make-marker))
       (new-first-marker (make-marker))
       new-first new-last change-beginning change-end
       )
    
    (unwind-protect
	(progn
	  (setq 
	   result
	   (catch 'failure
	     (bc-flush-caches)
	     (bc-bump-counters)
	     (bc-msetq (first-line last-line no-code) (bc-find-comment))
	     (setq 
;;;		range (bc-find-comment)
;;;		first-line (car range)
;;;	      first-line-char first-line 
	      new-first first-line
;;;	      last-line (cdr range)
;;;	      last-line-char last-line)
	      new-last last-line)
	     (bc-msetq (body-first-line body-last-line initial-width
					nil nil initial-column)
		       (bc-analyze-comment first-line last-line))
;;;		 body-range (bc-analyze-comment first-line last-line)
;;;		 body-first-line (car body-range)
;;;		 body-last-line (car (cdr body-range))
	     (setq
	      save-point (bc-save-excursion current-point first-line 
					    body-first-line body-last-line
					    last-line)
	      initial-width (min bc-right-margin initial-width))
	     
	     (cond
	      ((or no-tab no-reshape)
	       (setq
		initial-column (max 1 initial-column)))
	      (t
	       (setq
		initial-column (max 1 (min initial-column
					   (- bc-G-rm-minus-stuff 
					      (max 1 initial-width))))
;;;		initial-delta (mod initial-column bc-tab-spacing)
		initial-column
		(or (bc-next-tab-stop 'left initial-column 'maybe) 1)
;;;		initial-column (max 1 (- initial-column initial-delta))
		)))
	     
	     (set-marker old-first-marker first-line)
	     (bc-get-text body-first-line body-last-line)
	     (unwind-protect
		 (progn
		   (cond
		    ((eq this-command 'bc-fill) nil)
		    ((memq last-command bc-format-commands)
		     (setq initial-column bc-phantom-column))
		    ((memq this-command
			   '(bc-raise bc-raise-flow bc-lower bc-lower-flow))
		     (setq bc-phantom-column initial-column)))

		   ;; The first-line of a comment must sometimes be preserved
		   ;; across invocations. For example, if a comment is
		   ;; narrowed, then immediately widened, it should return to
		   ;; its original location. Since the narrowing could have
		   ;; raised the first line, we must save the inital first
		   ;; line as bc-pref-first-line.  Since lines will get
		   ;; screwed up when we clean up inserted newlines, use a
		   ;; marker.  We will convert this to a line number at the
		   ;; end of the function.

		   (set-marker bc-start-marker	; This will get changed set by
			       first-line)	; bc-moveit if successful.

		   (goto-char first-line)
		   (if (and (bobp) (< motion 0))
		       (throw 'failure "Beginning of buffer"))
		   (forward-line motion)	
		   (setq lim-top (if (> motion 0) (point)))
		   (bc-msetq (column top lines-needed where why)
			     (bc-fit-move initial-column 
					  first-line last-line
					  lim-top squeeze no-tab no-reshape
					  (if (> motion 0) 
					      no-code ; down
					    (not no-code)))) ; up
		   
		   ;; The treadmill problem when motion is down: If the line
		   ;; of code just below the comment is very wide (e.g. all
		   ;; the way to the right margin), `bc-fit-move' will
		   ;; determine that we must push that line down (i.e. we
		   ;; must insert blank lines before it).  The number of
		   ;; blank lines to insert will be the same as the number of
		   ;; lines we are trying to move down, because in such cases
		   ;; we never reshape the comment.
		   ;;
		   ;; As `bc-lower' or `bc-lower-flow' is called repeatedly,
		   ;; the comment eventually sits on lines by itself, with
		   ;; no code to its left anywhere.  The very wide line has
		   ;; been pushed down the number of lines equal to the
		   ;; height of the comment.  
		   ;; 
		   ;; A subsequent `lower' operation would push the wide line
		   ;; down further if we took the advice of `bc-fit-move',
		   ;; but this is not the behavior we want.  At this point,
		   ;; we want to jump over the wide line so that we can
		   ;; continue moving the comment down.
		   ;; 
		   ;; We handle this as a special case: when the comment is
		   ;; initially on lines by itself (i.e. no-code is t) and
		   ;; the number of lines-needed returned by `bc-fit-move'
		   ;; equals the number of lines we are trying to move down. 

		   (cond
		    ((and no-code (> motion 0) (= lines-needed motion))
		     (cond
		      ((eq why 'comment-below)
		       (error "There is another comment in the way."))
		      ((eq why 'line-too-wide)
		       (goto-char last-line)
		       (forward-line 2)
		       (setq top (point)
			     lines-needed (bc-count-lines-1 first-line
							    last-line)
			     where 0)
		       (if (bc-oparse-line)
			   (setq lines-needed (1+ lines-needed))))
		      ((eq why 'bottom-of-buffer)
		       (error "End of buffer."))
		      (t
		       (error "Here's one: %s" why)))))

		   (if (eq this-command 'bc-fill)
		       (setq bc-phantom-column column))
		   
		   (bc-msetq
		    (new-first new-last change-beginning change-end)
		    (bc-moveit top first-line last-line column
			       lines-needed where t))
;;;		    first-line-char (car new-range)
;;;		    last-line-char (nth 1 new-range))
		   nil);; end progn
	       
	       (set-buffer bc-G-code-buffer);; cleanup
	       (bc-restore-excursion save-point new-first new-last)
	       (set-marker new-first-marker new-first)
	       )));; end setq result

	  (if result				; was there a problem?
	      (progn
		(beep t)			; signal failure
		(message "%s%s" result
			 (if bc-show-fill-count 
			     (format " (Fill count: %d)" bc-fill-count) "")))
	    (save-excursion
	      (bc-cleanup-newlines change-beginning change-end)
	      (bc-set-pref-first-line))
	    (if bc-show-fill-count (message "Fill count: %d" bc-fill-count)
	      (message "Column: %d" column))
	    ))
      (set-marker old-first-marker nil)
      (set-marker new-first-marker nil)
      (set-marker bc-start-marker nil))))

;;;;======================================================================
;;;_    | bc-narrow-engine
(defun bc-narrow-engine (narrow squeeze)
  "Block-fill the block comment surrounding the current line, and
either narrow (if NARROW is positive) or widen the number of columns
that the comment text occupies.  The comment is taken to consist of
all the text in the right-margin comments on concecutive lines
adjacent to the current line.  Narrowing or widening attempts to
anchor the first line of the comment to its current line in the
buffer, with the rest of the comment moving up or down to accomodate
the change.  If this is not possible, the comment may be moved up some
lines.  If a series of bc-taller and bc-shorter commands are executed
sequentially, the original first line of the comment is remembered and
the comment will be placed as close as possible to there each time.

If SQUEEZE is non-nil, squeeze the resulting text to a rectangle,
otherwise place it at the nearest tab stop."

  (unwind-protect
      (let*
	  (
;;;	   (code-buffer (current-buffer))
	   (current-point (point))
	   save-point
	   first-line last-line first-line-char last-line-char
	   first-line-line
	   body-range body-first-line body-last-line
	   initial-width initial-column column indent
	   narrowed-area result new-range 
;;;	   no-code range lim-top 
	   )

	(setq 
	 result
	 (catch 'failure
	   (bc-flush-caches)
	   (bc-bump-counters)
	   (bc-msetq (first-line last-line) (bc-find-comment))
	   (setq indent (bc-compute-indent first-line))
;;;	   (if no-code (setq indent 20))
	   (setq 
	    ;;	range (bc-find-comment)
            ;;;	narrow (if (> narrow 0) 1 -1)
	    ;;	first-line (car range)
	    first-line-char first-line
	    ;;	last-line (cdr range)
	    last-line-char last-line
	    body-range (bc-analyze-comment first-line last-line)
	    body-first-line (car body-range)
	    body-last-line (car (cdr body-range))
	    save-point (bc-save-excursion current-point first-line
					  body-first-line
					  body-last-line last-line)
	    initial-width (min bc-right-margin (nth 2 body-range))
	    initial-column (nth 5 body-range))

	   (unwind-protect
	       (progn

		 (bc-get-text body-first-line body-last-line)

		 ;; With the current version of `bc-fit-narrow', I'm not sure
		 ;; if `bc-pref-first-line' does anything anymore. This
		 ;; function will not relocate the comment's first line under
		 ;; any circumstances, so `first-line' should always equal
		 ;; pref-first-line anyway.
		 (setq first-line-line (bc-count-lines-1-fast 1 first-line))
		 (if (memq last-command bc-format-commands)
		     (if (= bc-pref-first-line first-line-line) nil
		       (ding)
		       (message "bc-pref-first-line != first-line"))
		   (setq bc-pref-first-line first-line-line))

		 (set-buffer bc-G-code-buffer)
		 (bc-goto-line-fast bc-pref-first-line)
		 (set-marker bc-start-marker (point))
		 (beginning-of-line)
		 (setq 
		  narrowed-area 
		  (bc-fit-narrow 
		   narrow last-line initial-column first-line last-line
		   squeeze indent)
		  column (car narrowed-area)
		  
		  ;; The width of a block comment must sometimes be
		  ;; preserved. For example, if a comment is narrowed, then
		  ;; immediately raised, the user probably wants the width
		  ;; preserved. On the other hand, if a comment is simply
		  ;; filled, then subsequently raised, the width should not
		  ;; be restricted, except by comment-column. Thus we need a
		  ;; phantom comment column which manifests itself anytime we
		  ;; are making a series of sequential raises, lowers,
		  ;; narrows, and widens. The phantom column comes into
		  ;; existance when narrow or widen is executed, and
		  ;; dissapears when a command is executed that is not one of
		  ;; the four.

		  bc-phantom-column column
		  
		  new-range (bc-moveit 
			     (nth 1 narrowed-area) first-line last-line 
			     column (nth 2 narrowed-area)
			     (nth 3 narrowed-area))
		  first-line-char (car new-range)
		  last-line-char (nth 1 new-range))
		 nil);; end progn
	     (set-buffer bc-G-code-buffer);; cleanup
	     (bc-restore-excursion save-point first-line-char
				   last-line-char)
	     )));; end setq result
	
	(if result			; was there a problem?
	    (progn
	      (set-buffer bc-G-code-buffer)
	      (if initial-width
		  (setq bc-phantom-column
			(- bc-G-rm-minus-stuff (max 1 initial-width))))
	      (goto-char current-point)
	      (beep t)			; signal failure
	      (message "%s%s" result
		       (if bc-show-fill-count 
			   (format " (Fill count: %d)" bc-fill-count) "")))
	  (save-excursion
	    (bc-cleanup-newlines (nth 2 new-range) (nth 3 new-range))
	    (bc-set-pref-first-line))
	  (if bc-show-fill-count (message "Fill count: %d" bc-fill-count)
	    (message "Column: %d" column))
	  ))
    (set-marker bc-start-marker nil)))

;;;;======================================================================
;;;_   o Moving comment to/from scratch buffer or new position
;;;_    | bc-moveit
(defun bc-moveit (new-comment-beg-char old-comment-beg-char 
		  old-comment-end-char column &optional lines-needed
		  where set-start-marker)
  "Remove the right-margin comments from the lines between
OLD-COMMENT-BEG-CHAR and OLD-COMMENT-END-CHAR inclusive, including the
comment delimiters.  Then insert the contents of `bc-G-scratch-buffer' as
right-margin comments beginning on the line containing
NEW-COMMENT-BEG-CHAR.  \(All are buffer positions.\)  Pass nil for
OLD-COMMENT-BEG-CHAR if no comments to be removed.

If COLUMN is an integer, it is the column at which to place the newly
formatted comment.  If it is a list, use delimiter and column
information from that \(See `bc-analyze-comment' for format\).

If LINES-NEEDED is an integer, that many blank lines are added to
accomodate the bottom of the comment at a location WHERE lines from
the new beginning.  If WHERE is negative, the blank lines are added at
NEW-COMMENT-BEG-CHAR, and the comment is placed -WHERE lines below
that. 

Returns \(new-first new-last change-beginning change-end\), all buffer
positions."

  (let (;new-comment-beg-line			; Cannot use positions to
;	new-comment-end-line			; indicate lines now, because
;;; old-comment-beg-line old-comment-end-line	; we are about to delete some
	change-beg-char change-end-char		; text.
	(before-change-funs before-change-functions)
	(before-change-fun nil);jkh before-change-function)
	(after-change-funs after-change-functions)
	(after-change-fun nil);jkh after-change-function)
;;;	(code-buffer (current-buffer))
;;;	(scratch-buffer (bc-get-buffer-create))
;;;     (old-beg-marker (make-marker))
	(old-end-marker (make-marker))
	comment-rows
	(new-beg-marker (make-marker))
	(new-end-marker (make-marker))
	)
    (if (not where) (setq where 0))
    (unwind-protect
	(save-excursion
	  (set-buffer bc-G-scratch-buffer)
	  (setq
	   comment-rows
	   (if (listp column) (length column)
					; Minus one because bc-scratch-buffer
	     (+ (bc-count-lines-1-fast	; has an extra newline at the end.
		 1 (point-max))		; Minus another because we want the
		-2 bc-extra-lines)))	; beginning of the last line of the new
					; comment, not the following line.

	  (set-buffer bc-G-code-buffer)
;	   (setq new-comment-beg-line (bc-count-lines-1-fast
;				       1 new-comment-beg-char))
      
	  (if old-comment-beg-char
;;;	    (set-marker old-beg-marker old-comment-beg-char)
	      (set-marker 
	       old-end-marker
	       (progn (goto-char old-comment-end-char) (end-of-line) (point))))

	  (goto-char new-comment-beg-char)
	  (set-marker new-beg-marker (point))
	  (forward-line comment-rows)
	  (set-marker new-end-marker (point))

;	   (cond
;	    ((listp column)
;	     (setq new-comment-end-line (+ new-comment-beg-line
;					   (length column))))
;	    (t
;	     (setq new-comment-end-line		; Minus one because
;		   (+ new-comment-beg-line	; bc-scratch-buffer has an
;		      (bc-count-lines-1-fast	; extra newline at the end.
;		       1 (point-max))		; Minus another because we want
;		      -2			; the beginning of the last
;		      bc-extra-lines))))	; line of the new comment, not
;						; the following line.
;	   (set-buffer bc-G-code-buffer)

	  (setq change-beg-char			; Now figure out the minimum
		(if old-comment-beg-char	; buff pos that is about to be
		    (min old-comment-beg-char	; changed.
			 new-comment-beg-char)
		  new-comment-beg-char))
	  
	  (setq change-end-char
		(progn (goto-char new-end-marker)
		       (end-of-line) (point)))
	  (if old-comment-beg-char		; Likewise for the max buff
	      (setq change-end-char		; pos.
		    (max change-end-char
			 (progn (goto-char old-comment-end-char) 
				(end-of-line) (point)))))
      
	  (let (
		(after-change-function nil)	; Leaving these on makes
		(after-change-functions nil)	; comment formatting crawl.
		(before-change-function nil)
		(before-change-functions nil))

	    (if (and before-change-fun
		     (fboundp before-change-fun))
		(funcall before-change-fun
			 change-beg-char change-end-char))

							; Instead we will call
							; these hooks once for
	    (while before-change-funs			; the whole changed
	      (if (fboundp (car before-change-funs))	; region.
		  (funcall (car before-change-funs)
			   change-beg-char change-end-char))
	      (setq before-change-funs (cdr before-change-funs)))
	
	    (if old-comment-beg-char			; Now actually cut the
		(bc-delete-text old-comment-beg-char	; old comment.
				old-comment-end-char))	

	    (if (and lines-needed (< 0 lines-needed))	; Add blank lines if
		(let (newlines)				; needed.
		  (goto-char new-beg-marker)
		  (if (> where 0) (forward-line where))
		  (setq newlines (make-string lines-needed ?\n))
		  (add-text-properties
		   0 lines-needed
		   (list 'bc-added-newline bc-added-newlines-counter
			 'rear-nonsticky '(bc-added-newline))
		   newlines)
		  (insert newlines)))

	    (goto-char new-beg-marker)
	    (if (>= where 0) nil
	      (forward-line (- where))
	      (set-marker new-beg-marker (point))
;;;	      (forward-line (- where 1))
	      )
;	      (setq new-comment-beg-line (1+ new-comment-beg-line))))
	    (if set-start-marker (set-marker bc-start-marker (point)))

	    (bc-put-text (point) column)	; Insert the new comment.

	    (setq change-end-char
		  (progn (goto-char new-end-marker) (end-of-line) (point)))

	    (if old-comment-beg-char		; The end-of-line is different
		(setq change-end-char		; now, so measure it again.
		      (max change-end-char
			   (marker-position old-end-marker))))
	    
	    (if (and after-change-fun
		     (fboundp after-change-fun))
		(funcall after-change-fun
			 change-beg-char change-end-char 0))
	    
	    				; Now call the after-change-hooks.
	    (while after-change-funs	; Since this is a discontinuous change,
	      				; send zero for the before- length to
	      				; make it look like an insertion.
	      
	      (if (fboundp (car after-change-funs))
		  (funcall (car after-change-funs)
			   change-beg-char change-end-char 0))
	      
	      (setq after-change-funs (cdr after-change-funs)))
	    )
	  
	  (if bc-show-fill-count (message "Fill count: %d" bc-fill-count)
	    (if (integerp column) (message "Column: %d" column)))
	  
	  (list
	   (progn
	     (goto-char new-beg-marker)
	     (point))				; Return the char pos of the
	   (progn				; beginnings of the new first
	     (goto-char new-end-marker)		; and last lines and the change
	     (point))				; region.
	   change-beg-char change-end-char)
	  );; end save-excursion
      (set-marker old-end-marker nil)  ;; cleanup
      (set-marker new-beg-marker nil)
      (set-marker new-end-marker nil))
    ))


;;;;======================================================================
;;;_    | bc-cleanup-newlines
(defun bc-cleanup-newlines (start end)
  "Delete any blank lines we inserted that are no longer needed."
 
  (save-excursion
    (save-restriction
      (goto-char start)
      (forward-line -3)
      (setq start (point))
      (goto-char end)
      (forward-line 2)
      (end-of-line)
      (narrow-to-region start (point))
      (goto-char (point-min))
      (forward-line 1)
	
      (while (not (eobp))
	(let ((plist (memq 'bc-added-newline (text-properties-at (point))))
	      (next-change
	       (or (next-single-property-change 
		    (point) 'bc-added-newline (current-buffer))
		   (point-max)))
	      del-beg del-end
	      prior-is-comment next-is-comment ;;; current-is-blank
	      )
	  (if (and plist (/= (nth 1 plist)
			     bc-added-newlines-counter))
	      (progn
		(beginning-of-line)			; This text was added
		(while (and (< (point) next-change)	; by BC on a previous
			    (not (eobp)))		; command.
		  (setq
		   del-beg (point)
		   prior-is-comment (save-excursion
				      (forward-line -1) (bc-oparse-line))
		   next-is-comment (save-excursion
				     (forward-line 1) (bc-oparse-line)))

		  (if (or (and prior-is-comment next-is-comment)
			  (not (looking-at "[ \t]*$")))
		      (forward-line 1)
		    (forward-line 1)
		    (setq del-end (point))
		    (delete-region del-beg del-end)
		    (bc-flush-parse-cache)
		    (setq next-change 
			  (- next-change (- del-end del-beg)))))))
	  (goto-char next-change))))))

;;;;======================================================================
;;;_    | bc-get-text
(defun bc-get-text (first-line last-line)
  "Extracts the text of a block comment from the lines between
FIRST-LINE and LAST-LINE inclusive.  Both args should be buffer
positions of the beginnings of lines.  Text is copied to the buffer
indicated by 'bc-G-scratch-buffer'.

The text copied has no whitespace at the beginning or end of any line.
This is a consequence of using `bc-parse-line' to find the extents of
the text on the line."

  (let (
;;;	(code-buffer (current-buffer))
;;;	(scratch-buffer (bc-get-buffer-create))
	com-body-start com-body-end 
;;;	com-delim-end comment-start-col comment-end-col
	)

    (save-excursion
      (set-buffer bc-G-scratch-buffer)
      (erase-buffer)					; erase any old stuff
      (set-buffer bc-G-code-buffer)

      (goto-char first-line)				; copy all the text to
							; the scratch
      (while (<= (point) last-line)
	(bc-oparse-line nil nil 'com-body-start 'com-body-end
		       'com-delim-end)

	(set-buffer bc-G-scratch-buffer)
	(insert-buffer-substring
	 bc-G-code-buffer com-body-start com-body-end)
	(insert "\n")
	(set-buffer bc-G-code-buffer)
	(forward-line 1))

;;;      (set-buffer scratch-buffer)
;;;      (set-text-properties 1 (point-max) nil)
;;;      (set-buffer bc-G-code-buffer)
      nil)))

;;;;======================================================================
;;;_    | bc-put-text

;;; For profiling:  elp can't profile builtins, so a lisp wrapper is
;;; needed.  For release code these should all be `defsubst's.  For
;;; profiling make them `defun's

(defsubst bc-insert-buffer-substring (buffer bosl eosl)
  (insert-buffer-substring buffer bosl eosl))

(defsubst bc-set-buffer (buffer)
  (set-buffer buffer))

(defsubst bc-indent-to (col)
  (indent-to col))

(defsubst bc-delete-region (b e)
  (delete-region b e))

(defsubst bc-internal-end-of-line ()
  (end-of-line))

(defsubst bc-forward-line (&optional arg)
  (forward-line arg))

;;;------------------------------------------------------------------
(defun bc-put-text (first-line column-or-delims)
  "Copies the text of a block comment from the scratch buffer to the
current buffer at the location specified by FIRST-LINE and COLUMN.
The scratch buffer is the buffer pointed to by bc-G-scratch-buffer.

If COLUMN-OR-DELIMS is an integer, use delimiters `bc-effective-start'
and `bc-effective-end', and align entire comment to that column.  If it
is a list, use delimiter and column information from that \(See
`bc-analyze-comment' for format\).

Returns the buffer position of the last char inserted."

  (let (
;;;	(code-buffer (current-buffer))
;;;	(scratch-buffer (bc-get-buffer-create))
	bosl eosl
	
	(right-end (- bc-right-margin (length bc-effective-end)))
	banner-width banner-start banner-end
	delims delim-whitespace
;;;	eocl com-start-len com-end-len
	)
						;==============================
    (save-excursion				;  If this is to be a banner
      (goto-char first-line)			;  comment, insert the top
						;  banner.
      (if (and (not (listp column-or-delims))	;==============================
	       bc-banner-top-border)
	  (progn
	    (setq
	     delim-whitespace (string-match "[ \t]" bc-effective-start)
	     banner-start (if delim-whitespace (substring bc-effective-start 0 
							  delim-whitespace)
			    bc-effective-start)
	     delim-whitespace (string-match "[^ \t]" bc-effective-end)
	     banner-end (if (equal bc-effective-end "") ""
			  (if delim-whitespace (substring bc-effective-end 
							  delim-whitespace)
			    bc-effective-end))
	     banner-width (- bc-right-margin column-or-delims
			     (length banner-start) (length banner-end)))
	    (bc-internal-end-of-line)
	    (delete-horizontal-space)
	    (bc-indent-to column-or-delims)
	    (insert banner-start)
	    (insert (make-string banner-width (car bc-banner-top-border)))
	    (insert banner-end)
	    (bc-forward-line 1)
	
	    (if (> bc-banner-top-space 0)	; If this is a banner, it may
		(progn				; need a blank line at the top.
		  (bc-internal-end-of-line)
		  (delete-horizontal-space)
		  (bc-indent-to column-or-delims)
		  (insert banner-start)
		  (if (equal bc-effective-end "")
		      nil
		    (bc-indent-to right-end)
		    (insert bc-effective-end))
		  (bc-forward-line 1)))))

						;==============================
      (bc-set-buffer bc-G-scratch-buffer)	;  Now actually insert the text
      (goto-char 1)				;  of the comment.
      (if (eobp)				;==============================
	  (progn
	    (insert ?\n)			; Hack to prevent obliterating
	    (goto-char 1)))			; empty comments.
      (while (not (eobp))
	(setq bosl (point))
	(bc-internal-end-of-line)		; Find the extent of the line
	(setq eosl (point))			; in the scratch buffer.
	(bc-set-buffer bc-G-code-buffer)
	(bc-internal-end-of-line)
	(delete-horizontal-space)
	(cond
	 ((listp column-or-delims)
	  (setq delims (car column-or-delims)
		column-or-delims (cdr column-or-delims))
	  (bc-indent-to (car delims))
	  (insert (nth 1 delims))
	  (bc-insert-buffer-substring bc-G-scratch-buffer bosl eosl)
	  (insert (nth 2 delims)))
	 (t
	  (bc-indent-to column-or-delims)
	  (insert bc-effective-start)
	  (bc-insert-buffer-substring bc-G-scratch-buffer bosl eosl)
	  (if (equal bc-effective-end "")
	      nil
	    (bc-indent-to right-end)
	    (insert bc-effective-end))))
	
	(bc-forward-line 1)
	(bc-set-buffer bc-G-scratch-buffer)
	(bc-forward-line 1));; end while

      (bc-set-buffer bc-G-code-buffer)		;==============================
      (cond					;  If this is to be a banner
       ((listp column-or-delims)		;  comment, insert the bottom
	(while column-or-delims			;  banner.
	  (setq delims (car column-or-delims)	;==============================
		column-or-delims (cdr column-or-delims))
	  (bc-internal-end-of-line)
	  (delete-horizontal-space)
	  (bc-indent-to (car delims))
	  (insert (nth 1 delims) (nth 2 delims))
	  (bc-forward-line 1)
	  ))
       (bc-banner-top-border
	(if (> bc-banner-bottom-space 0)

	    (progn
	      (bc-internal-end-of-line)
	      (delete-horizontal-space)
	      (bc-indent-to column-or-delims)	; If this is a banner, it may
	      (insert bc-effective-start)	; need a blank line at the
	      (if (equal bc-effective-end "")	; bottom.
		  nil
		(bc-indent-to right-end)
		(insert bc-effective-end))
	      (bc-forward-line 1)))
	
	(bc-internal-end-of-line)
	(delete-horizontal-space)
	(bc-indent-to column-or-delims)
	(insert banner-start)
	(insert (make-string banner-width (car bc-banner-top-border)))
	(insert banner-end)))

      (setq bc-parse-line-cache nil)
      (point) ; return position of last char
      ) ; end save-excursion
    ))

;;;;======================================================================
;;;_    | bc-delete-text
(defun bc-delete-text (first-line last-line)
  "Deletes the text of a block comment from the lines specified by 'range'.

The comment is taken to consist of all the text and comment characters of
the right-margin comments on the lines in 'range'.  Whitespace to the left
of the comments, and everything to the right of them is also removed.

Unlike 'kill-comment' this function only deletes the rightmost comment
on each line."
  
  (let* ((num-lines (bc-count-lines-1 first-line last-line))
	 code-end-pos delim-e
;;;	 eol 
	 )

  (save-excursion
    (goto-char last-line)			; Go backwards so we can still
						; use bc-parse-line-cache
    (beginning-of-line)
    (while (> num-lines 0)
      (setq num-lines (1- num-lines))
      (bc-internal-end-of-line)
;;;      (setq eol (point))
      (bc-oparse-line 'code-end-pos nil nil nil 'delim-e)
      (bc-delete-region code-end-pos delim-e)
      (bc-forward-line -1))
    (bc-flush-parse-cache)
    )))

;;;;======================================================================

; (defmacro bc-fill-or-tab-macro ()
;   `(progn
;      (set-buffer scratch-buffer)
;      (cond
;       (no-reshape)				; do nothing if no reshape
;       (squeeze
;        (setq fill-column orig-avail-width)
;        (bc-query-fill)
;        (setq column (- bc-G-rm-minus-stuff bc-fb-width)))
;       (no-tab
;        (setq fill-column orig-avail-width)
;        (bc-query-fill)
;        (setq column (- bc-G-rm-minus-stuff orig-avail-width)))
;       (t
;        (setq
; 	column
; 	(car
; 	 (bc-tab-buffer 0
; 			(- bc-G-rm-minus-stuff orig-avail-width))))))))

;;;;======================================================================
;;;_   o Fitting
;;;_    | bc-fit-move
(defun bc-fit-move (initial-column &optional comm-range-beg comm-range-end
		    lim-top squeeze no-tab no-reshape insert-after)

  "Find a comment shape and a place it will fit.

INITIAL-COLUMN specifies the placement (and therefore width) of the
first try.  If this is less than the width of the code on the current
line, the first attempt places the comment just past the code.

COMM-RANGE-BEG and COMM-RANGE-END should specify the extent of any
comment presently in the search range.  These are passed to
`bc-find-room'. 

If LIM-TOP is non-nil, it should be a buffer position of the beginning
of the line limiting the upper range of the search.  (The comment may
be placed on this line but not above.)

The value of INSERT-AFTER is used only if we are forced to insert a
blank line to support the first line of the comment.  If non-nil, that
blank line will be inserted after the line containing point, otherwise
before it.  Additional blank lines will be inserted as needed,
adjacent to the first one.

Returns '\(comment-column top lines-needed where why\)."

  ;; Let me start by saying that this particular function is a
  ;; God-awful beast.  It's complexity has gotten *way* out of hand
  ;; and even I don't really know how it works anymore.  I would love
  ;; to rewrite it.  I think that one reason for its complexity is
  ;; that it tries to be exhaustive in its search for a place to put
  ;; the comment.  This is partly a holdover from the days before BC
  ;; acquired the ability to insert blank lines to make room for the
  ;; comment.

  ;; The algorithm is as follows. A series of trials are performed. On each
  ;; trial, the comment is filled to a given fill-column and its height is
  ;; taken. bc-find-room is then called to find that many lines and returns
  ;; the width of the available right-margin space in those lines. If this
  ;; is wide enough for the fill-column of the comment text, we are
  ;; done. If not, the comment is re-filled at that width and the process
  ;; is repeated. Thus, in this series of trials the comment is repeatedly
  ;; narrowed to see if a fit can be found.
  ;;
  ;; During these trials no minimum required width is passed to
  ;; bc-find-room. At some point in these trials (unless we have
  ;; success), the comment will eventually reach its minimum fillable
  ;; width. When this occurs, max-code-width is set and a new set of
  ;; trials is begun. On each of these, max-code-width is repeatedly
  ;; reduced which allows the comment fill-column to increase and its
  ;; height to decrease. If max-code-width drops below the width of
  ;; the code on the current line, an exception is thrown to 'failure.

  (let* (
;;;	 (code-buffer (current-buffer))
;;;	 (local-tab-stops bc-tab-stops)
;;;	 (scratch-buffer (bc-get-buffer-create))
	 (orig-top (point))
	 (top (point))
	 (bottom nil)
	 (column initial-column)
;;;	 (bc-G-rm-minus-stuff-1 (1- bc-G-rm-minus-stuff))
	 (max-code-width
	  (if no-reshape initial-column (1- bc-G-rm-minus-stuff)))
;;;	    bc-G-rm-minus-stuff-1))
;;;	    (orig-max-code-width
;;;	     (if no-reshape initial-column
;;;	       (min
;;;		bc-G-rm-minus-stuff-1
;;;		(max (bc-code-width-this-line) initial-column))))
	 (orig-max-code-width (min
			       max-code-width
			       (max (bc-code-width-this-line) initial-column)))
	 (avail-width (- bc-G-rm-minus-stuff orig-max-code-width))
	 (orig-avail-width avail-width)
	 avail-area try-again try-adding-lines need-blank-line
	 lines-needed where why no-code-this-line
	 comment-width comment-height failure new-column
;;;	 delta
	 )
    
    (setq 
     need-blank-line
     (catch 'failure

       (if (< avail-width 2)
	   (throw 'failure 'line-too-wide))
    
       (save-excursion
	 (bc-prepare-text)
	 (beginning-of-line)
      
	 (setq 
	  try-adding-lines
	  (catch 'try-adding-lines
	    (while
		(progn
		  (setq comment-width (- bc-G-rm-minus-stuff column))
		  (set-buffer bc-G-scratch-buffer)
		  (setq
		   try-again
		   (catch 'try-again			; Fill the comment to
							; the new width.
		     (cond
		      ((or squeeze no-tab no-reshape)	; ** Just filling.
		       (setq fill-column comment-width)
		       (bc-query-fill)
		       (if (<= bc-fb-width comment-width)
			   (setq comment-width bc-fb-width)
			 (setq max-code-width		; If we have reached
			       (- bc-G-rm-minus-stuff	; the narrowest this
				  bc-fb-width)		; comment can go, start
			       column			; over using its min
			       (min max-code-width	; width.
				    orig-max-code-width))
			 (throw 'try-again t)))
		      (t
		       (setq				; ** Tabbing
			failure (catch 'failure
				  (setq column
					(car (bc-tab-buffer 0 column)))
				  nil))
		       (if (not failure)
			   (setq comment-width (- bc-G-rm-minus-stuff column))
			 (setq max-code-width (bc-next-tab-stop 'left column))
			 (if (not max-code-width)
			     (throw 'failure 'comment-wider-than-margins))
			       

; 			  delta (mod column		 ; If we have reached
; 				     bc-tab-spacing)	 ; the rightmost tab at
; 			  delta (if (zerop delta)	 ; which this comment
; 				    bc-tab-spacing delta); can fit, start over
; 			  max-code-width		 ; using the tab stop
; 			  (max 1 (- column delta))	 ; to the left of that.
			 (setq column
			       (min max-code-width orig-max-code-width))
			 (throw 'try-again t))))
	       
		     (setq comment-height (+ bc-fb-lines bc-extra-lines))
		     (set-buffer bc-G-code-buffer)
		     (setq				; See if it will fit
		      avail-area			; now.
		      (bc-find-room
		       comment-height column max-code-width top bottom
		       comm-range-beg comm-range-end lim-top))

		     (if (not (eq (car avail-area) 'failure)) nil
		       (cond
			(no-reshape			; If not enough lines
			 (throw 'try-adding-lines t))	; with required space,
							; try to reduce the
			((or squeeze no-tab)		; number of lines
			 (setq				; needed by widening
			  max-code-width		; the comment.
			  (1- (min max-code-width column))
			  avail-width orig-avail-width
			  column (min max-code-width
				      orig-max-code-width)))
			(t
			 (if (and (eq max-code-width 1)
				  (eq column 1))		; Same, but
			     (throw 'try-adding-lines t))	; reduce by tab
			 (setq					; stops.
			  column (min max-code-width column)
			  max-code-width (or (bc-next-tab-stop 'left column) 1)
; 			  delta (mod column bc-tab-spacing)
; 			  delta (if (zerop delta)
; 				    bc-tab-spacing delta)
; 			  max-code-width (max 1 (- column delta))
			  column (min max-code-width orig-max-code-width))))

		       (if (< max-code-width
			      orig-max-code-width)
			   (throw 'try-adding-lines t)
			 (throw 'try-again t)))
		  
		     (setq column (nth 2 avail-area)
			   avail-width (- bc-G-rm-minus-stuff column))

		     (if (< 1 avail-width)		; If we have run into a
			 nil				; very wide code line,
		       (set-buffer bc-G-scratch-buffer)	; start over using the
		       (setq fill-column 1)		; comment's min width
		       (bc-query-fill)			; to dictate
		       (setq max-code-width		; max-code-width.
			     (- bc-G-rm-minus-stuff bc-fb-width)
			     column max-code-width)
		       (throw 'try-again t))
		  
		     (setq top (car avail-area)
			   bottom (nth 1 avail-area))
		     nil));; end setq try-again
		  (if try-again
		      (setq bottom nil
			    top orig-top);; `while' condition
		    (< avail-width comment-width))))
	    ));; end setq try-adding-lines
      
	 (cond
	  (try-adding-lines
;	   (bc-fill-or-tab-macro)
	   (set-buffer bc-G-scratch-buffer)
	   (cond
	    (no-reshape)			; Do nothing if no reshape.
	    (squeeze
	     (setq fill-column orig-avail-width)
	     (bc-query-fill)
	     (setq column (- bc-G-rm-minus-stuff bc-fb-width)))
	    (no-tab
	     (setq fill-column orig-avail-width)
	     (bc-query-fill)
	     (setq column (- bc-G-rm-minus-stuff orig-avail-width)))
	    (t
	     (setq
	      column
	      (car
	       (bc-tab-buffer 0 (- bc-G-rm-minus-stuff orig-avail-width))))))

	   (if (<= bc-fb-width fill-column) nil
	     (throw 'failure "No room there.."))
	
	   (setq comment-height (+ bc-fb-lines bc-extra-lines))
	   (set-buffer bc-G-code-buffer)
	   (setq avail-area (bc-find-room comment-height column column
					  orig-top nil comm-range-beg 
					  comm-range-end lim-top))
	
	   (if (eq (car avail-area) 'failure)
	       (setq
		why (nth 1 avail-area)
		lines-needed (- comment-height (nth 2 avail-area))
		top (nth 3 avail-area)
		column (nth 4 avail-area)
		where 
		(cond
		 ((memq why '(comment-on-first-line comment-above))
		  (throw 'failure why))
			   ;"There is another comment in the way."))
		 ((eq why 'comment-below) 
		  (- comment-height -1 lines-needed))
		 ((zerop (nth 2 avail-area))
		  (throw 'failure "No room there"))
		 ((memq why '(bottom-of-buffer line-too-wide top-reached))
		  (- comment-height lines-needed))
		 ))
	     (error 
	      "bc-mode internal error 003.  Please submit a full bug report."))
	   )
	  (t
	   (setq lines-needed 0)
	   ))
	 ) nil)) ; end setq need-blank-line

    (cond
     ((eq need-blank-line 'comment-wider-than-margins)
      (error "That comment is too wide for the margins."))
     ((eq need-blank-line 'line-too-wide)
      (goto-char orig-top)
      (forward-line -1)
      (if (bc-oparse-line) (setq need-blank-line 'comment-above)))
     )

    (cond
     ;; If need-blank-line is set, all above attemts at finding space
     ;; have failed.  This means either that the first line of the
     ;; destination is too wide, or that there is another comment in
     ;; the way.

     ((eq need-blank-line 'comment-on-first-line)
      (throw 'failure "There is another comment on the first line."))

     (need-blank-line
      ;; In this case, we will add a blank line before or after the
      ;; first line of the destination, then try again to find space. 

      (set-buffer bc-G-scratch-buffer)
      (setq fill-column (- bc-G-rm-minus-stuff initial-column))
      (bc-query-fill)
      (setq column (- bc-G-rm-minus-stuff (max bc-fb-width orig-avail-width))
	    comment-height (+ bc-fb-lines bc-extra-lines)
	    lines-needed comment-height
	    top orig-top
	    where 0)

      (set-buffer bc-G-code-buffer)
      (cond

       ;; If inserting lines after the destination, we may not need to add
       ;; `comment-height' of them, because there may be some room after the
       ;; first line.  We must look for room again, this time starting on the
       ;; second line, and assuming that we already have one of the lines we
       ;; need.

       (insert-after
	(setq top
	      (save-excursion (goto-char orig-top) (forward-line 1) (point))
	      avail-area
	      (bc-find-room (1- comment-height) initial-column column top nil
			    comm-range-beg comm-range-end top))
	(if (eq (car avail-area) 'failure)
	    (setq 
	     why (nth 1 avail-area)
	     lines-needed (- comment-height (nth 2 avail-area))
	     new-column (nth 4 avail-area)
	     )
	  (setq lines-needed 1
		new-column (nth 2 avail-area)))
	(cond
	 ((eq need-blank-line 'comment-above)
	  (goto-char orig-top)
	  (if (<= (bc-code-width-this-line) column)
	      (setq
	       top orig-top
	       where -1
	       column (max new-column (bc-code-width-this-line)))
	    (setq column new-column)))))

       ;; If inserting a blank line before the destination, we will
       ;; have to insert as many blank lines as comment-height if the
       ;; destination line is too wide.  Those values are already set by now
       ;; so do nothing.  If the reason is because there is a comment above,
       ;; then we may not have to insert that many lines.

       ((eq need-blank-line 'comment-above)
	(setq where -1)
	(goto-char orig-top)
	(bc-oparse-line nil nil nil nil nil nil 'no-code-this-line)
	(if no-code-this-line
	    (throw 'failure "There is another comment above."))
	(if (> (bc-code-width-this-line) column)
	    (setq
	     lines-needed (1+ comment-height)
	     column initial-column)
	  (setq top
		(save-excursion (goto-char orig-top) (forward-line 1) (point))
		avail-area
		(bc-find-room (1- comment-height) column column top nil
			      comm-range-beg comm-range-end top))
	  (if (eq (car avail-area) 'failure)
	      (setq 
	       why (nth 1 avail-area)
	       lines-needed (- comment-height (nth 2 avail-area) -2)
	       )
	    (setq lines-needed 1)))
	(setq top orig-top))
;;;	(throw 'failure "There is another comment above."))
       )))    ; end cond

    (set-buffer bc-G-scratch-buffer)			; If we are supposed to
    (if squeeze						; squeeze, do it now.
	(setq avail-area
	      (bc-squeeze-buffer
	       0 column (1- bc-G-rm-minus-stuff))
	      column (car avail-area)))
    
    (setq fill-column (- bc-G-rm-minus-stuff column))
    (bc-fill-buffer-maybe)
    
    (set-buffer bc-G-code-buffer)
;;    (setq bc-pref-first-line (bc-count-lines-1 1 top))
    (list column top lines-needed where why)
    ))

;;;;======================================================================
;;;_    | bc-fit-narrow
(defun bc-fit-narrow (narrow last-line initial-column
		      comm-range-beg comm-range-end squeeze indent)
  "Find a new width for the comment and a place it will fit. 
The width will be one stop wider or narrower, depending on the values
of NARROW and SQUEEZE.  If narrow is 1, narrowing is performed, if -1,
widening.  If SQUEEZE is non-nil, the comment is narrowed or widened
to make it one line taller or shorter, and squeezed as narrow as
possible at that height.  If SQUEEZE is nil, the comment is narrowed
or widened to the next tab stop.

Also calls `bc-fill-buffer-maybe' to make sure scratch buffer is
filled and ready to be used.

Returns '\(comment-column top lines-needed where\)."
  (let (
;;;	(scratch-buffer (bc-get-buffer-create))
;;;	(code-buffer (current-buffer))
;;;	(local-tab-stops bc-tab-stops)
	(top (point))
	column lines-needed narrowed-area avail-area comment-height
;;;	where
	)
    (save-excursion
      (bc-prepare-text)

      (setq column initial-column)
      (set-buffer bc-G-scratch-buffer)
      (setq narrowed-area
	    (if squeeze
		(bc-squeeze-buffer narrow column (1- bc-G-rm-minus-stuff))
	      (bc-tab-buffer narrow column indent))
	    column (car narrowed-area)
	    comment-height (+ (car (cdr narrowed-area)) bc-extra-lines))

      (setq fill-column (- bc-G-rm-minus-stuff column))
      (bc-fill-buffer-maybe)

      (set-buffer bc-G-code-buffer)
      (setq
       avail-area (bc-find-room comment-height 0 column top nil
				comm-range-beg comm-range-end top))
      (if (eq (car avail-area) 'failure)
	  (setq lines-needed (- comment-height (nth 2 avail-area)))
;;;		where (- comment-height lines-needed))
	(setq lines-needed nil))

      ;; Return 0 for `where'. This causes blank lines to be added at the
      ;; beginning rather than at the end. When the user does a widen followed
      ;; immediately by a narrow, these blank lines remain. Because of this,
      ;; the two operations are not inverses of each other. With these lines at
      ;; the beginning of the comment rather than at the end, it is possible to
      ;; restore the buffer easily by doing `bc-lower' a few times.
      (list column top lines-needed 0))))
;;;      (list column top lines-needed where))))
      
;;;;======================================================================
;;;_    | bc-find-room
(defmacro bc-check-adj-comment (catch-tag exception)
  "Check the results of a call to `bc-parse-line'. 
If the line contains a comment, we need to decide if this is the
comment begin processed or an adjacent one.  In the latter case, we
need to throw an exception.

Point must be in the same line as when `bc-parse-line' was called.

Uses free variables `comment-p', `comm-range-beg' and `comm-range-end'."
  (`
   (if (or (not comment-p)
	   (and comm-range-beg
		(or (beginning-of-line) 1)
		(<= comm-range-beg (point))
		(<= (point) comm-range-end)))
		 nil
	       (throw (, catch-tag) (, exception)))))

;;;;======================================================================
(defun bc-find-room (lines-needed code-width max-code-width 
				  first-line last-line
				  &optional comm-range-beg comm-range-end
				  lim-top)
  "Finds available space for right-margin comments in the neighborhood
of 'first-line'.

The function tries to expand the range of lines from FIRST-LINE to
LAST-LINE to include LINES-NEEDED lines such that the widths of
those lines are all less than or equal MAX-CODE-WIDTH.  LAST-LINE
may be nil to indicate that no range has been acquired yet.

If successful, function returns \(first-line last-line code-width\), where
`first-line' and `last-line' indicate the new range, and `code-width'
indicates the width of the widest line of code in the range.

If the search area contains a comment that is currently being
formatted, we don't want this to be mistaken as being in the way.  
COMM-RANGE-BEG COMM-RANGE-END must be supplied to indicate where the
comment currently lies.  Both are buffer positions of beginnings of
lines.

If there is not enough room (because of buffer limits, wide code, or
adjacent comments), function returns ('failure reason lines-avail
first-line code-width), where 'lines-avail' is a count of how many
lines did fit."

  (let ((num-lines (if last-line (bc-count-lines-1 first-line last-line) 0))
	(new-first first-line)
	(new-last last-line)
	result
	code-width-this-line code-end-pos no-more-down
	comment-p 
;;;	(no-more-up nil) current-line
	)

    (setq lim-top (or lim-top (point-min)))
    (save-restriction
      (setq
       result					;==============================
       (catch 'failure				; If no range exists yet,
	 (goto-char first-line)			; examine the first line.
	 (if last-line nil			;==============================
	   (setq comment-p (bc-oparse-line 'code-end-pos))
	   (bc-check-adj-comment 'failure 'comment-on-first-line)
	   (if (bobp) nil
	     (forward-line -1)			; Make sure there is a
	     (setq comment-p (bc-oparse-line))	; comment-free line above.
	     (bc-check-adj-comment 'failure 'comment-above))
	   (goto-char first-line)
	   (if (eobp) nil
	     (forward-line 1)
	     (setq comment-p (bc-oparse-line))
	     (bc-check-adj-comment 'failure 'comment-below))
	   (goto-char first-line)
	   (setq code-width-this-line (bc-code-width-this-line))
	   (if (> code-width-this-line max-code-width)
	       (throw 'failure 'line-too-wide))
	   (setq code-width (max code-width-this-line code-width))
	   (setq num-lines 1)
	   (setq new-last first-line))

	 ;; There are three things that make us stop searching in a particular
	 ;; direction: end-of-buffer, a line with too-wide code, or a line with
	 ;; another comment. The last one is special because we must have an
	 ;; extra line to separate that comment from the one being formatted.

	 (setq
	  no-more-down
					;;===================================;;
					;; First search downward to see if   ;;
	  (catch 'no-more-down		;; there is enough space in that     ;;
	    (goto-char new-last)	;; direction.			     ;;
					;;===================================;;
	    (while (< num-lines lines-needed)
	      (save-excursion
		(end-of-line)
		(if (eobp) (throw 'no-more-down 'bottom-of-buffer)))

	      (forward-line 1)
	      (bc-oparse-line 'code-end-pos nil nil nil nil 
			     'code-width-this-line)

					; This check must be made before the
					; next one because if we end up adding
					; lines we need to know why so we can
					; add them in the right place.
	      (if (<= code-width-this-line max-code-width) nil
		(throw 'no-more-down 'line-too-wide))

	      (if (eobp) nil			; Make sure there is a
		(forward-line 1)		; comment-free line below.
		(setq comment-p (bc-oparse-line))
		(bc-check-adj-comment 'no-more-down 'comment-below))

	      (goto-char code-end-pos)
	      (setq code-width (max code-width-this-line code-width))
	      (beginning-of-line)
	      (setq new-last (point))
	      (setq num-lines (1+ num-lines)))))


	 (if (or (= num-lines lines-needed)	;;=============================
		 (eq no-more-down		;; If we still need more room,
		     'bottom-of-buffer))	;; try looking up.
	     nil				;;=============================
;;;	   (setq
;;;	    no-more-up
	    (catch 'no-more-up 
	      (goto-char new-first)
	      (while (< num-lines lines-needed)
		(if (or (<= new-first lim-top) (bobp))
		    (progn
		      (throw 'no-more-up 'top-reached)))
		(forward-line -1)
;;;		(setq current-line (point))

		(bc-oparse-line 'code-end-pos nil nil nil nil 
			       'code-width-this-line)
;;;		(goto-char code-end-pos)
;;;		(setq code-width-this-line (bc-code-width-this-line))
		(if (<= code-width-this-line max-code-width) nil
		  (throw 'no-more-up 'line-too-wide))

		(if (bobp) nil
		  (forward-line -1)
		  (setq comment-p (bc-oparse-line))
		  (bc-check-adj-comment 'no-more-up 'comment-before))
		 
		(goto-char code-end-pos)
		(setq code-width (max code-width-this-line code-width))
		(beginning-of-line)
		(setq new-first (point))
		(setq num-lines (1+ num-lines))) nil))

	 (if (<= lines-needed num-lines) nil
	   (throw 'failure no-more-down))
;;;	      (if (or (not no-more-up) (eq no-more-up 'top-reached))
;;;		  (throw 'failure no-more-down)	; Use reason at bottom.
;;;	      (throw 'failure no-more-up)))
	 ));; end setq result
      ) ; end save-restriction
    (if result
	(list 'failure result num-lines new-first code-width)
      (list new-first new-last code-width))
    ) ; end let
  )

;;;;======================================================================
;;;_   o Filling
;;;_    | bc-squeeze-buffer
(defun bc-squeeze-buffer (narrow current-column max-width)
  "Adjust the fill-width of the buffer so the text is as close to a 
rectangular block as possible, and optionally narrow or widen the 
rectangle so that the block becomes one line more or less than before.

A zero NARROW argument causes only rectangular blocking.  Negative
values cause the fill-column to be increased to make the text occupy
one less line.  Positive values cause the fill-column to be reduced,
and, if the text was initially rectangular-blocked, the text will
subsequently occupy one more line, otherwise it will only be
rectangular-blocked.

CURRENT-COLUMN is the column in the code buffer to which the text in
the scratch buffer is currently fit.  This means that the text in the
scratch buffer is filled to \(- bc-G-rm-minus-stuff CURRENT-COLUMN\).

Returns \(column height\).

Throws to 'failure if narrowing or widening cannot be done within
allowable margins."

 ; Note: calling functions depend on this function to either modify the buffer
 ; or throw an exception. If this function were to decide that it must leave
 ; the buffer as is, but return without throwing, a calling function will
 ; probably get stuck in an infinite loop.

  (let (height orig-height 
	width old-width
	new-height
	(just-blocking-p nil)
	fill-width fill-next-width
	at-max-width 
;;;	fill-lines widest-is-one current-width
	)

    (setq fill-column (- bc-G-rm-minus-stuff current-column))
    (bc-query-fill)
    (setq ;;; current-width bc-fb-width
	  width bc-fb-width
	  height bc-fb-lines
	  fill-next-width bc-fb-next-width
	  orig-height bc-fb-lines
	  new-height bc-fb-lines
	  old-width bc-fb-width)		; Can't use forward-word
						; because `fill-region' won't
;;;    (goto-char (point-min))			; break on characters like `-',
;;;    (skip-chars-forward "^ \t\n")		; thus we define a `word' as
;;;    (skip-chars-forward " \t\n")		; something delimited by
;;;    (cond					; whitespace.
;;;	((eobp)
;;;	 (goto-char (point-min))		; This comment is only one
;;;	 (skip-chars-forward "^ \t\n")		; word, so it can't be widened
;;;	 (setq width (current-column))		; or narrowed.
;;;	 (setq new-height 1)
;;;	 )

    (cond					; If widening, set the
     ((< narrow 0)				; fill-column progressively
      (while (and (= new-height height)		; wider and wider until the
		  (not at-max-width))		; height decreases, then we are
	(if (and (< fill-next-width max-width)	; done.
		 (not (= 1 height)))
	    (setq fill-column fill-next-width)
	  (setq fill-column max-width)		; If we reach full-page width,
	  (setq at-max-width t))		; don't require rectangular
						; blocking.
	(bc-query-fill)
	(setq fill-width bc-fb-width
	      new-height bc-fb-lines
	      fill-next-width bc-fb-next-width));; end while
      (if (and at-max-width (= fill-width old-width))
	  (throw 'failure "No room to widen."))
      (setq width
	    (if at-max-width max-width fill-width))
      )

     (t						; If narrowing or blocking, set
      (while (and (= new-height height)		; the fill-column progressively
		  (not bc-fb-widest-is-one))	; narrower until the height
	(if (< width 1)				; increases.
	    (throw 'failure "No room to narrow"))
	(setq fill-column (1- width))
	(setq old-width width)
	(bc-query-fill)
	(setq width bc-fb-width)
	(setq new-height bc-fb-lines)
	(setq fill-next-width bc-fb-next-width))

						; At this point, we have either
      (if (and bc-fb-widest-is-one		; succeeded in increasing the
	       (not (= narrow 0)))		; number of lines or gotten
	  (setq fill-next-width width))		; stuck on a long one-word
						; line.

      (if (or (= narrow 0))			; If we were asked to narrow,
;;;(not (= current-width old-width)))		; and the text was already
	  (setq just-blocking-p t)		; blocked, then we need
	(setq height new-height)		; continue narrowing until the
	(while (and (= new-height height)	; height increases by one more.
		    (not bc-fb-widest-is-one))
	  (if (< width 1)
	      (throw 'failure "No room to narrow."))
	  (setq fill-column (1- width))
	  (bc-query-fill)
	  (setq width bc-fb-width)
	  (setq new-height bc-fb-lines)
	  (setq fill-next-width bc-fb-next-width)))

						; Again, we have either
						; succeeded in increasing the
						; number of lines or gotten
						; stuck on a long one-word
      (setq fill-column				; line.
	    (if just-blocking-p
		(if (= orig-height new-height) width fill-next-width)
	      (cond
	       ((= new-height orig-height)
		(throw 'failure "That is as narrow as it goes."))
	       ((= new-height (1+ orig-height))
		width)
	       (t
		fill-next-width))))

      (bc-query-fill)
      (if just-blocking-p nil
	(if (= orig-height bc-fb-lines)
	    (throw 'failure "No room to narrow...")))

      (setq width bc-fb-width)
      (setq new-height bc-fb-lines)
      )
     );; end cond
    
    (list (- bc-G-rm-minus-stuff width) new-height)
    );; end let
  )

;;;;======================================================================
;;;_    | bc-next-tab-stop
(defun bc-next-tab-stop (direction column &optional maybe indent)
  "Find the next tab stop to the right or left of COLUMN.
DIRECTION should be either of the symbols `right' or `left'.  

If COLUMN happens to already be at a tab stop, the return value can be
controlled by the optional argument MAYBE.  If this is the symbol
`maybe', then the same column is returned.  If it is nil or the symbol
`force', the next tab stop in the appropriate direction is returned.

The global variable `bc-G-tab-stops' will be used as the list of tab
stops.  The optional argument INDENT can specify an additional tab
stop.

Returns nil if there is no stop in the desired direction."

  (let (stop
	(stoplist bc-G-tab-stops)
	new-column delta thresh
	(min-delta 9999)
	)
    (if (eq maybe 'force) (setq maybe nil))
    (setq thresh (if maybe -1 0))

    (if (or (not indent) (zerop indent)) nil	;; First check the optional
      (setq delta (if (eq direction 'left)	;; stop. We do this here
		      (- column indent)		;; because someday we may take
		    (- indent column)))		;; advantage of the fact that
      (if (< thresh delta)			;; STOPLIST is ordered.
	  (setq new-column indent
		min-delta delta)))

    (while stoplist				;; Now check the stoplist.
      (setq stop (bc-pop stoplist)		;; Someday the stoplist should
	    delta (if (eq direction 'left)	;; probably be an arry and this
		      (- column stop)		;; should do a binary search.
		    (- stop column)))
      (if (and (< thresh delta)
	       (< delta min-delta)
	       (< stop bc-G-rm-minus-stuff)
	       (not (zerop stop)))
	  (setq new-column stop min-delta delta)))
    new-column))

;;;;======================================================================
;;;_    | bc-tab-buffer
(defun bc-tab-buffer (narrow current-column &optional indent) 

  "Fill `bc-G-scratch-buffer' to the next nearest tab stop.  Tab stops
are those of the code buffer -- tab stops farther to the right mean
fill the scratch buffer narrower.

If NARROW is zero, do nothing if CURRENT-COLUMN is on a tab stop.
Otherwise fill to next tab stop to right.

If NARROW is positive, fill to next tab stop to right.  If negative,
fill to next tab stop to left.

Optional INDENT can specify an additional tab stop.

Returns \(column height\)."

  (let* (;;(mod-delta (- (mod current-column bc-tab-spacing)))
	 ;;(on-tab-stop (or (= current-column 1)
	;;		  (= mod-delta 0)))
;	 (stops (append tab-stops (list indent)))
	 new-column new-height
;;;	 (prev-stop-delta 9999) stop-column stop-delta
	 )
    
    (cond
     ((< narrow 0)				; Widening the comment.

      (setq new-column (bc-next-tab-stop 'left current-column 'force indent))
;       (while stops
; 	(setq stop-column (car stops)
; 	      stops (cdr stops))
; 	(if (not stop-column) nil
; 	  (setq stop-delta (- current-column stop-column))	; Search for
; 	  (if (and (< 0 stop-delta)				; the nearest
; 		   (< stop-delta prev-stop-delta)		; stop to the
; 		   (not (= stop-column 0)))			; left.
; 	      (progn (setq new-column stop-column)
; 		     (setq prev-stop-delta stop-delta)))))

      (if (not new-column) 
	  (throw 'failure "Can't widen that."))
      )

     (t					; Narrowing the comment.
      (setq new-column (bc-next-tab-stop 'right current-column
					 (if (zerop narrow) 'maybe 'force)
					 indent))

;       (while stops
; 	(setq stop-column (car stops)
; 	      stops (cdr stops))				; Search for
; 	(if (not stop-column) nil				; the nearest
; 	  (setq stop-delta (- stop-column current-column))	; stop to the
; 	  (if (and (or (and (zerop narrow) (zerop stop-delta))	; right.
; 		       (< 0 stop-delta))
; 		   (< stop-delta prev-stop-delta)
; 		   (not (= stop-column 0)))
; 	      (setq new-column stop-column
; 		    prev-stop-delta stop-delta))))
      (if (not new-column)
	  (throw 'failure "Can't narrow that."))
      )
     ) ; cond

     (cond
      ((< narrow 0)				; Widening
       (if (<= current-column 1)
	   (throw 'failure "Can't widen that.")))
      ((<= 0 narrow)				; Narrowing
       (if (< bc-G-rm-minus-stuff new-column)
	   (throw 'failure "Can't narrow that."))))

;     (setq fill-column (- bc-G-rm-minus-stuff current-column))
;     (bc-query-fill)
;     (setq orig-height bc-fb-lines)

    (setq new-column (max 1 new-column))	; Don't use column zero.
    (setq fill-column (- bc-G-rm-minus-stuff new-column))
    (bc-query-fill)
    (setq new-height bc-fb-lines)

    (if (and (<= 0 narrow) (< bc-G-rm-minus-stuff (+ new-column bc-fb-width)))
	(throw 'failure "Can't narrow that"))
    
    (list new-column new-height)))

;;;;======================================================================
;;;_    | bc-query-fill
(defun bc-query-fill ()
  "Retrieve fill information for current value of `fill-column'.
If `bc-fill-buffer' has been called with the same value of
`fill-column' on the same text before, the shape information will be
in `bc-fill-buffer-cache'.  If not, `bc-fill-buffer' is called to
fill the buffer and compute shape info.

If `bc-fb-debug' is non-nil, always call `bc-fill-buffer'.

Sets global variables: `bc-fb-lines', `bc-fb-width', `bc-fb-widest-is-one',
`bc-fb-next-width'."

  (let (fill-info 
	(column fill-column)
	)
    (if (not bc-fill-buffer-cache)
	(setq bc-fill-buffer-cache (make-vector
				    (* 2 bc-G-rm-minus-stuff) nil)))
    (setq fill-info (aref bc-fill-buffer-cache column))
    (if (and fill-info (not bc-fb-debug))
	(progn
	  (setq bc-fb-lines (nth 0 fill-info)
		bc-fb-width (nth 1 fill-info)
		bc-fb-widest-is-one (nth 2 fill-info)
		bc-fb-next-width (nth 3 fill-info)))
      (bc-fill-buffer)
      (setq fill-info
	    (list bc-fb-lines bc-fb-width bc-fb-widest-is-one
		  bc-fb-next-width))
      (aset bc-fill-buffer-cache column fill-info)
      (setq column (1- column))
      (while (<= bc-fb-width column)	; If the result was actually narrower
	(aset bc-fill-buffer-cache	; than fill-column, set all array
	      column fill-info)		; elements in between because they will
	(setq column (1- column))))))	; have the same results.

;;;;======================================================================
;;;_    | bc-prepare-text
(defun bc-prepare-text ()
  "Clean up the text in the scratch buffer in preparation for
formatting.  This is stuff that could be done in `bc-fill-buffer', but
it only needs to be done once, so doing it here streamlines multiple
calls to `bc-fill-buffer'."

  (save-excursion					; tabs in the text will
    (set-buffer bc-G-scratch-buffer)			; screw things up when
							; its copied back to
    (subst-char-in-region 1 (point-max) ?\t ?\  )	; the code

    (goto-char 1)					; Delete all whitespace
    (skip-chars-forward " \t\n")			; at beginning of
    (delete-region 1 (point))				; buffer.

    (while (not (eobp))
      (forward-paragraph)				; Delete paragraph
      (insert ?\n)					; indentations and
      (delete-blank-lines)				; preserve only a
      (skip-chars-forward " \t\n\r")			; single blank line
      (delete-horizontal-space))			; between paragraphs.

    (goto-char (point-max))				; Make sure the scratch
    (insert ?\n)					; buffer ends with a
    (goto-char (point-max))				; single newline.
    (delete-blank-lines)
    ))

;;;;======================================================================
;;;_    | bc-fill-buffer-maybe
(defun bc-fill-buffer-maybe ()
  (if (and bc-fb-prev-fill-column (= bc-fb-prev-fill-column fill-column))
      nil
    (bc-fill-buffer)))

;;;;======================================================================
;;;_    | bc-fill-buffer
(defun bc-fill-buffer ()
  "Fill each of the paragraphs in the buffer.
Addapted from `fill-region' from fill.el

This does a minimum of cleanup, so usually `bc-prepare-text' should be
called once on the text before the first call to this function.  Only
one call to that function is necessary prior to multiple calls to this
function. 

Sets global vars: `bc-fb-lines', `bc-fb-width', `bc-fb-widest-is-one',
`bc-fb-next-width', `bc-fill-count', `bc-fb-prev-fill-column'.

`bc-fb-next-width' is set with the next value of fill-column greater
than the current that would cause the region to be filled differently
\(i.e. wider\).  This is found by looking at the width of each
resulting line, plus the length of the first word on the next line.
The minimum of these sums is the value of next-width."  

  (setq bc-fill-count (1+ bc-fill-count)
	bc-fb-prev-fill-column fill-column

	bc-fb-lines 0
	bc-fb-width 0
	bc-fb-widest-is-one nil
	bc-fb-next-width 65535)

  (goto-char (point-min))
  (while (not (eobp))
    (let ((initial (point))
	  (end (progn
		 (forward-paragraph 1) (point))))
      (forward-paragraph -1)				; Preserve the blank
      (skip-chars-forward " \t\n")			; line between
      (if (< (point) initial)				; paragraphs
	  (goto-char end)
	
;;; fill-paragraph
	(save-restriction 
	  (narrow-to-region (point) end)
	  
	  (subst-char-in-region (point-min)		; Change all newlines
				(point-max) ?\n ?\ )	; to spaces.
	    
	  (goto-char (point-min))			; Flush excess spaces.
	  (while (re-search-forward "   *" nil t)
	    (delete-region
	     (+ (match-beginning 0) 1)
	     (match-end 0)))
	  (goto-char (point-max))
	  (delete-horizontal-space)
	  (insert " ")
	  (goto-char (point-min))
	    
	  ;; This is the actual filling loop.
	  (let (linebeg)
	    (while (not (eobp))
	      (setq linebeg (point))
	      (move-to-column (1+ fill-column))
	      ;; Move back to start of word.
	      (skip-chars-backward "^ \n" linebeg)

	      (if (save-excursion			; Keep at least one
		    (skip-chars-backward " " linebeg)	; word even if fill
		    (bolp))				; prefix exceeds
		  (progn				; margin.
		    (beginning-of-line)
		    (delete-horizontal-space)
		    (skip-chars-forward "^ \n"))
							; Normally, move back
							; over the single space
		(forward-char -1))			; between the words.
	
	      (skip-chars-backward " ")			; Check for widest
	      (cond					; line. If this is,
	       ((= bc-fb-width (current-column))	; also check if it is
		(if (save-excursion			; unbreakable.
		    (skip-chars-backward "^ " linebeg)
		    (bolp))
		    (setq bc-fb-widest-is-one t)))
	       ((<= bc-fb-width (current-column))
		(setq bc-fb-widest-is-one 
		      (save-excursion
			(skip-chars-backward "^ " linebeg) (bolp)))
		(setq bc-fb-width (current-column))))
	      
	      (save-excursion				; Find out what
		(skip-chars-forward " " (point-max))	; fill-column would
		(if (eobp)				; have to be to make
		    nil					; this line wider.
		  (skip-chars-forward "^ " (point-max))
		  (setq bc-fb-next-width (min bc-fb-next-width
					      (current-column)))))

	      (insert ?\n)				; Replace all
	      (setq bc-fb-lines (1+ bc-fb-lines))	; whitespace here with
	      (delete-horizontal-space))		; one newline.

	    );; end let (linebeg)
  
;;; end fill-paragraph
	  
	  (setq bc-fb-lines (1+ bc-fb-lines)))
	)));; end while
  (setq bc-fb-lines (1- bc-fb-lines));; end let
  bc-fb-lines) 

;
;;;_   o Scanning
;;;_    | bc-find-comment
(defun bc-find-comment (&optional below-only)
  "Finds the number of lines adjacent to the current one which contain
right- margin comments.

Returns (comment-top comment-bottom no-code) where the first two
values each indicate the character position of the beginning of that
line.  `no-code' is non-nil if no line of the comment contains any
code to its left, i.e., this is not a right-margin-comment.

If BELOW-ONLY is non-nil, the current line will be considered the top
line of the comment.

Comments which start in column 1, and comments which span more than one line
are not considered to be right-margin comments.

Throws to 'failure if no block comment found."

  (let  (
	 first-line last-line
	 range-bottom
;;;	 code-end-pos com-delim-start-pos
;;;	 com-body-start-pos com-body-end-pos 
;;;	 com-delim-end-pos
	 no-code-this-line (no-code t)
;;;	 range-top top-is-comment bottom-is-comment 
	 )

    (if selective-display				; Is an outline mode
	(save-excursion					; on?
	  (end-of-line)
	  (setq range-bottom (point))
	  (beginning-of-line)
	  (if (search-forward "\C-M" range-bottom 'noerror)
	      (throw 'failure
		     "Can't format comments inside closed outline body."))))

    (if (not (bc-oparse-line nil nil nil nil nil nil	; Is there a comment on
			    'no-code-this-line))	; this line?

	(save-excursion
	  (beginning-of-line)
	  (if (looking-at bc-comment-looking-at-regexp)
	      (throw 'failure 
"No block comment found. (Because the comment starts at the left margin.)")
	    (throw 'failure "No block comment found."))))

    (setq no-code (and no-code no-code-this-line))

    (save-excursion
      (if below-only
	  (setq first-line (point))	; find the first line of this  	       
	(while (and			; block			       	       
		(setq first-line (point))
		(= (forward-line -1) 0)	; watch for beginning of buffer
		(bc-oparse-line nil nil nil nil nil nil 'no-code-this-line)
		(or (setq no-code (and no-code no-code-this-line)) 1)
		)))
      (goto-char first-line)
      (beginning-of-line)		; find the beginning of that line
      (setq first-line (point)))

    (save-excursion
      (while (and			; find the last line of this block
	      (setq last-line (point))
	      (forward-line 1)
	      (not (eobp))		; watch for end of buffer
	      (bc-oparse-line nil nil nil nil nil nil 'no-code-this-line)
	      (or (setq no-code (and no-code no-code-this-line)) 1)
	      ))
      (goto-char last-line)
      (beginning-of-line)		; find the beginning of that line
      (setq last-line (point)))

    (list first-line last-line no-code)))	; return range

;;;;======================================================================
;;;_    | bc-find-just-range
(defun bc-find-just-range ()
  "Finds the number of lines adjacent to the current one which do not
contain right-margin comments.  

Returns (range-top . range-bottom), where each value indicates the
character position of the beginning of that line.  Note that
range-bottom is really the beginning of the line just below the actual
range.  This is so that if you narrow-to-region on this range, you
will have all the allowable lines.

Comments which start in column 1, and comments which span more than
one line are not considered to be right-margin comments.

Throws to 'failure if either the current line, the line above, or the
line below contains a block comment."

  (let  (
;;;	 first-line last-line this-line
	 range-top range-bottom
	 top-is-comment 
;;;	 com-body-start-pos com-body-end-pos 
;;;	 com-delim-end-pos
;;;	 bottom-is-comment code-end-pos com-delim-start-pos 
	 )
    
    (if (bc-oparse-line)	  
	(throw 'failure "No room."))	; Already a comment on this line.

    (save-excursion
      (while (and
	      (setq range-top (point))	; watch for beginning
	      (= (forward-line -1) 0)	; of buffer	       
	      (not (setq top-is-comment (bc-oparse-line)))))
      (goto-char range-top)
      (if top-is-comment (forward-line 1))
      (beginning-of-line)		; find the beginning of that line
      (setq range-top (point)))

    ;;      (if (= range-top this-line) (throw 'failure "No room."))

    (save-excursion			; find the top line of the
      (while (and			; range below containing a
	      (setq range-bottom (point)) ; rt-margin comment       
	      (forward-line 1)
	      (not (eobp))		; watch for end
	      (not (bc-oparse-line)))) ; of buffer    
      (goto-char range-bottom)
      (beginning-of-line)		; find the beginning of that line
      (setq range-bottom (point)))

    (if (<= range-bottom range-top) (throw 'failure "No room."))

    (cons range-top range-bottom)))	; return list

;;;;======================================================================
;;;_    | bc-find-range
(defun bc-find-range ()
  "Finds the number of lines adjacent to the current one which contain right-
margin comments.  Additionally, the function finds the local range of lines
which contain no other right-margin comments.

Returns ((comment-top . comment-bottom) . (range-top . range-bottom)),
where each value indicates the character position of the beginning of
that line.  Note that range-bottom is really the beginning of the line
just below the actual range.  This is so that if you narrow-to-region
on this range, you will have all the allowable lines.

Comments which start in column 1, and comments which span more than one line
are not considered to be right-margin comments.

Throws to 'failure if no block comment found."

  (let (
;;;	(current-point (point))
	first-line last-line
	range-top range-bottom
	top-is-comment 
;;;	bol bottom-is-comment
	)

    (if (not (bc-oparse-line))		     ; Is there a comment on this line?
	(throw 'failure "No block comment found."))

    (save-excursion				; Find the first line of this
      (while (and				; block.
	      (setq first-line (point))
	      (= (forward-line -1) 0)		; Watch for beginning of buffer.
	      (bc-oparse-line)))
      (goto-char first-line)			; Find the beginning of that
      (beginning-of-line)			; line.
      (setq first-line (point))
						; Find the last line of the
      (while (and				; range above containing a
	      (setq range-top (point))		; rt-margin comment.

	      (= (forward-line -1) 0)		; Watch for beginning of buffer.
	      (not (setq top-is-comment (bc-oparse-line)))))
      (goto-char range-top)
      (if top-is-comment (forward-line 1))
      (beginning-of-line)			; Find the beginning of that
      (setq range-top (point)))			; line.

    (save-excursion
      (while (and				; Find the last line of this
	      (setq last-line (point))		; block.
	      (forward-line 1)
	      (not (eobp))			; Watch for end of buffer.
	      (bc-oparse-line)))
      (goto-char last-line)			; Find the beginning of that
      (beginning-of-line)			; line.
      (setq last-line (point))
						; Find the top line of the
      (while (and				; range below containing a
	      (setq range-bottom (point))	; rt-margin comment.
	      (forward-line 1)
	      (not (eobp))			; Watch for end of buffer.
	      (not (bc-oparse-line))))
      (goto-char range-bottom)
      (beginning-of-line)			; Find the beginning of that
      (setq range-bottom (point)))		; line.

    (cons (cons first-line last-line)
	  (cons range-top range-bottom))))	; Return list.

;;;;======================================================================
;;;_    | bc-analyze-line
(defmacro bc-analyze-line ()
  (`
   (save-excursion 
     (bc-oparse-line nil 'start-delim-b 'body-b 'body-e 'end-delim-e)
     (goto-char body-b)
     (skip-chars-backward " \t")
     (setq start-delim-e (point))
     (goto-char body-e)
     (skip-chars-forward " \t" end-delim-e)
     (setq end-delim-b (point)))))

;;;;======================================================================
;;;_    | bc-analyze-banner
(defmacro bc-analyze-banner ()
  "Analyze comment on current line to see if it is a banner.
Returns the banner character if so, nil if not.

`bc-analyze-line' must be called on the current line first."
  (`
   (save-excursion 
     (cond
      ((or				 ; Soft borders made of delimiter chars
	(= start-delim-e end-delim-b)	 ; /**/ 
	(progn (goto-char start-delim-e) ; /*****/
	       (looking-at "[ \t]*$")))	 ; ;;;;;;;;;;
       (list (char-after (1- start-delim-e)) nil))
    
					 ; These are not banners because of
      ((or (/= start-delim-e body-b)	 ; spaces:
	   (/= body-e end-delim-b))	 ; ;;;;; ======
       nil)				 ; /*======= */

					 ; Hard borders made of other chars
      ((progn				 ; /*======*/
	 (goto-char body-b)		 ; ;;=======
	 (skip-chars-forward 
	  (buffer-substring body-b (1+ body-b))
	  (save-excursion (end-of-line) (point)))
	 (= (point) body-e))
       (list (char-after body-b) 'hard))
    
      (t nil)))				 ; Not a banner after all.
   ))

;;;;======================================================================
;;;_    | bc-save-delims
(defmacro bc-save-delims ()
  (`
   (let ((start-delim-string (buffer-substring start-delim-b body-b))
	 (end-delim-string (buffer-substring body-e end-delim-e))
	 )
     (setq 
      bc-saved-delims (cons 
		       (list (progn (goto-char start-delim-b) (current-column))
			     start-delim-string end-delim-string)
		       bc-saved-delims))
     (set-text-properties 0 (length start-delim-string) nil start-delim-string)
     (set-text-properties 0 (length end-delim-string) nil end-delim-string)
     )))

;;;;======================================================================
;;;_    | bc-save-banners
(defmacro bc-save-banners ()
  (`
   (let ((start-delim-string (buffer-substring start-delim-b body-e))
	 (end-delim-string (buffer-substring body-e end-delim-e))
	 )
     (setq 
      bc-saved-delims (cons 
		       (list (progn (goto-char start-delim-b) (current-column))
			     start-delim-string end-delim-string)
		       bc-saved-delims))
     (set-text-properties 0 (length start-delim-string) nil start-delim-string)
     (set-text-properties 0 (length end-delim-string) nil end-delim-string)
     )))

;;;;======================================================================
;;;_    | bc-analyze-comment
(defun bc-analyze-comment (start end &optional save-delims)
  "Examine the right-margin comment on the lines between START and END.
Determines the effective comment delimiters.  For example, in Lisp the
comment-start string is a single semicolon, but a comment may start
with more than one.  If so we want the effective comment-start string
to have that many semis.

This function also determines whether the comment has a banner.

Returns \(new-start new-end width new-first new-last column\), where
`new-start' and `new-end' indicate the range of the comment minus
banner and empty lines.  `width' indicates the width of the comment,
defined to be maximum of the widths of the text \(excluding
delimiters\) on each line.  `new-first' and `new-last' indicate the
range of the comment minus banner lines.  `column' indicates the
starting column of the comment, as determined by the start delimiter
of the first text line of the comment.

If SAVE-DELIMS is non-nil, delimiters and their column positions are
recorded in the global variable `bc-saved-delims'.  The format is a
list of elements, each recording delimiter information for one line.
The first element of the list corresponds to START.  

Two types of elements are used: one for banners and one for the rest
of the comment.  For banners, each element has the form \(column
banner\),where `column' indicates the column in which the start
delimiter begins on that line, and `banner' contains all of the
comment on that line, including delimiters.  

For the rest of the comment, each element has the format \(column
start end\), where `start' and `end' are the effective delimiters,
including whitespace.

Sets global variables: `bc-effective-start', `bc-effective-end',
`bc-G-rm-minus-stuff', `bc-banner-top-border', `bc-banner-top-space',
`bc-banner-bottom-space', `bc-banner-bottom-border', `bc-extra-lines', 
`bc-saved-delims', `bc-delims-length'."

  (let (start-delim-b 
	start-delim-e
	body-b body-e
	end-delim-b end-delim-e
	new-start new-end 
	(new-first start)
	(new-last end)
	this-width (max-width 0)
	column first-line-column
	)
    (save-excursion
      (setq bc-banner-top-space 0
	    bc-banner-bottom-space 0)
      
      (goto-char start)
      (bc-analyze-line)
      (setq first-line-column (save-excursion
				(goto-char start-delim-b)
				(current-column))	; First decide if this
	    bc-banner-top-border (bc-analyze-banner))	; is a banner comment.
      (if (not bc-banner-top-border) nil
	(forward-line 1)				
	(setq new-first (point))			; If so, check for
	(if (< end (point)) nil				; initial blank line.
	  (bc-oparse-line nil nil 'body-b 'body-e)
	  (if (/= body-b body-e) nil
	    (setq bc-banner-top-space 1)
	    (forward-line 1))))
      
      (bc-oparse-line nil nil 'body-b 'body-e)
      (while (and (<= (point) end) (= body-b body-e))	; Skip blank lines.
	(forward-line 1)
	(bc-oparse-line nil nil 'body-b 'body-e))
      (setq new-start (min (point) end))
      
      (if (not (bc-oparse-line nil 'start-delim-b 'body-b))
	  (progn
	    (setq new-end end
		  new-last end				; If we fell off the
		  max-width 0				; end of the comment,
		  bc-effective-start bc-comment-start	; then it contained
		  bc-effective-end bc-comment-end	; only blank lines.
		  bc-banner-bottom-border nil
		  bc-banner-bottom-space 0
		  column first-line-column
		)					; If top border was
	    (if (nth 1 bc-banner-top-border) nil	; soft, then it
	      (setq bc-banner-top-border nil))		; probably wasn't a
	    (goto-char end))				; banner after all.

	(bc-analyze-line)				; Figure out the
	(setq						; effective start and
	 bc-effective-start (buffer-substring		; end delimiters.
			     start-delim-b body-b) 
	 bc-effective-end (if (and (equal bc-comment-end "")
				   (not bc-faux-comment-end)) ""
			    (save-excursion
			      (goto-char body-b)
			      (skip-chars-backward "\t ")
			      (concat (buffer-substring (point) body-b)
				      (buffer-substring
				       end-delim-b  end-delim-e)))))
	(if (string-match "^[ \t]+$" bc-effective-end)
	    (setq bc-effective-end ""))

	(goto-char start-delim-b)                   
	(setq column (current-column))

	(goto-char end)					; See if bottom line is
	(bc-analyze-line)				; a banner border.
	(if (not (setq bc-banner-bottom-border (bc-analyze-banner)))
	    (setq new-last (point))
	  (forward-line -1)
	  (setq new-last (point)		; If the comment was entirely
						; empty, new-start will have
		new-start			; snagged on the bottom banner.
		(min new-start new-last))	; This isn't comment text,
						; though, so back it up.
	  (if (< (point) new-start) nil
	    (bc-oparse-line nil nil 'body-b 'body-e)
	    (if (/= body-b body-e) nil			; If so, is there a
	      (setq bc-banner-bottom-space 1)		; final blank line?
	      (forward-line -1))))
	
	(bc-oparse-line nil nil 'body-b 'body-e)
	(while (and (< new-start (point)) (= body-b body-e))
	  (forward-line -1)
	  (bc-oparse-line nil nil 'body-b 'body-e))	; Skip blank lines at
	(setq new-end (max (point) new-start))		; bottom.

	);; end (if (not (bc-oparse-line)
      
      (setq bc-extra-lines				; Scan the comment
	    (+ (if bc-banner-top-border 2 0)		; bottom-up, so that
	       bc-banner-top-space			; saved-delims will be
	       bc-banner-bottom-space))			; in order.
      
      (if (not save-delims) nil				; Save delims in the
	(setq bc-saved-delims nil)			; bottom banner border.
	(goto-char end)
	(while (< new-end (point))
	  (bc-oparse-line nil 'start-delim-b 'body-b 'body-e 'end-delim-e)
	  (bc-save-banners)
	  (forward-line -1)))

      (while						; Figure out comment
	  (and						; width.
	   (<= new-start (point))
	   (progn
	     (bc-oparse-line nil 'start-delim-b 'body-b 'body-e 'end-delim-e)
	     (setq this-width (- (progn (goto-char body-e) (current-column))
				 (progn (goto-char body-b) (current-column))))
	     (if (< this-width max-width) nil
	       (setq max-width this-width))
	     (if save-delims (bc-save-delims))
	     (= 0 (forward-line -1)))))


;;;	 (and (= new-start new-end)			; If text is only one
;;;	      (setq					; line, we mustn't use
;;;	       nonwhite					; all the trailing
;;;	       (string-match "[^ \t]" bc-effective-end)); whitespace as part of
;;;	      (setq					; the end delim.
;;;	       bc-effective-end
;;;	       (concat " " (substring bc-effective-end nonwhite))))
;;;
;       (while (and save-delims (<= start (point)))
; 	(bc-oparse-line nil 'start-delim-b 'body-b
; 		       'body-e 'end-delim-e)
; 	(bc-save-banners)
; 	(forward-line -1))

      (if (and save-delims		; Save delims in the top banner border.

					; Make sure the code to figure out the
	       (< (point) new-start))	; comment width didn't bump into the
					; top of the buffer.
	  (while (and (<= start (point))
		      (progn	       
			(bc-oparse-line nil 'start-delim-b 'body-b
				       'body-e 'end-delim-e)
			(bc-save-banners)
			(= 0 (forward-line -1))))))	; Stop if we bump into
							; top of buffer.

      
      (setq bc-delims-length (+ (length bc-effective-start)
				(length bc-effective-end))
	    bc-G-rm-minus-stuff (- bc-right-margin bc-delims-length))
      
      (list new-start new-end max-width new-first new-last column)
      )))

;;;;======================================================================
;;;_    | bc-default-analysis-vars
(defun bc-default-analysis-vars ()

  "Initialize global variables set by `bc-analyze-comment' to defaults
for formatting commands that don't call that function."

  (setq bc-banner-top-border nil
	bc-banner-top-space 0
	bc-banner-bottom-space 0
	bc-extra-lines 0
	bc-effective-start bc-comment-start
	bc-effective-end bc-comment-end
	bc-G-rm-minus-stuff (- bc-right-margin (length bc-effective-start)
			     (length bc-effective-end))
	))

;;;;======================================================================
;;;_    | bc-parse-line
(defun bc-parse-line (&rest args)
  (error "The function bc-parse-line is just a placeholder right now."))

(defun bc-oparse-line (&optional bpl-code-end bpl-delimiter-start 
				 bpl-body-start bpl-body-end 
				 bpl-delimiter-end
				 bpl-code-width bpl-no-code)
  "Parse the current line to find buffer positions of beginnings and
endings of code, comment and delimiters.

ARGS should all be symbols, values will be set into them  Pass nil for
args you don't need.

Returns non-nil if line contains a block-comment."

  ;; The bpl- and bpli- prefixes are necessary because this function
  ;; `set's values into arguments (which must be symbols).  All the
  ;; local variables must have wierd names so as not to match any
  ;; symbols that the caller may pass.
  ;; 
  ;; This is horrible!  I will never do this again!  This should just
  ;; return a list and let callers assign with `bc-msetq'

  (save-excursion 
    (let* (
	   (bpli-eol (progn (end-of-line) (point)))
	   (bpli-bol (progn (beginning-of-line) (point)))
	   (bpli-line-info (assq bpli-bol bc-parse-line-cache))
	   bpli-commentp bpli-danger
	   bpli-code-end bpli-delim-start bpli-body-start
	   bpli-body-end bpli-delim-end bpli-code-width bpli-no-code
	   )
      
      (if bpli-line-info		; Cache hit? If not, scan the line.
	  nil
	(setq bpli-commentp
	      (if (eolp)		; Is this a blank line?
		  nil
		(re-search-forward bc-comment-regexp bpli-eol 'move))

	      ;; We won't match comments starting in column zero
	      ;; because bc-comment-regexp must match one
	      ;; character preceding the start-comment delimiter.
	      ;; Therefore, we do not need to test for column zero.

					; Add one because regexp matches one
	      bpli-delim-start (and	; char before the delimiter.
				bpli-commentp
				(1+ (match-beginning 0)))
	      bpli-body-start (and bpli-commentp (match-beginning 1))

	      bpli-body-end
	      (and
	       bpli-commentp
	       (progn
		 (cond
		  (bc-faux-comment-end
		   (goto-char (match-end 0))
		   (save-match-data
		     (re-search-backward bc-faux-comment-end
					 bpli-body-start t)))
		  (t
		   (goto-char (match-end 1))))
		 (skip-chars-backward (char-to-string	     ; Might be extra
				       (following-char)))    ; end delim chars
		 (skip-chars-backward " \t")		     ; that got matched
		 (max (point) (match-beginning 1))))	     ; by the .*

	      bpli-delim-end (and bpli-commentp
				  (progn
				    (goto-char (match-end 0))
				    (skip-chars-backward " \t")
				    (max (point) bpli-body-end)))
	      bpli-code-end (progn
			      (if (not bpli-commentp) (end-of-line)
				(goto-char (1+ (match-beginning 0))))
			      (skip-chars-backward " \t")
			      (if (and bc-start-first-char
				       (eq (preceding-char)	; Check for
					   bc-start-first-char)); code ending
				  (setq bpli-danger t)		; in dangerous
				(setq bpli-danger nil))		; characters.
			      (point))
	      bpli-code-width (if bpli-danger (1+ (current-column))
				(max 1 (current-column)))

	      bpli-no-code (= (current-column) 0)


	      bpli-line-info (list bpli-bol bpli-delim-start bpli-body-start
				   bpli-body-end bpli-delim-end
				   bpli-code-end bpli-code-width
				   bpli-no-code)

	      bc-parse-line-cache (cons bpli-line-info bc-parse-line-cache)))
					     
      (if bpl-delimiter-start
	  (set bpl-delimiter-start (nth 1 bpli-line-info)))
      (if bpl-body-start
	  (set bpl-body-start (nth 2 bpli-line-info)))
      (if bpl-body-end
	  (set bpl-body-end (nth 3 bpli-line-info)))
      (if bpl-delimiter-end
	  (set bpl-delimiter-end (nth 4 bpli-line-info)))
      (if bpl-code-end 
	  (set bpl-code-end (nth 5 bpli-line-info)))
      (if bpl-code-width
	  (set bpl-code-width (nth 6 bpli-line-info)))
      (if bpl-no-code
	  (set bpl-no-code (nth 7 bpli-line-info)))

      (nth 1 bpli-line-info))))		; return comment-p
    
;;;;======================================================================
;;;_    | bc-compute-indent
(defun bc-new-compute-indent (bufpos)
  "Return an appropriate indentation for the line containing BUFPOS.
Calls the function designated by `indent-line-function', if that is
non-nil.  In this way, the indentation is computed by the major mode.

Returns the column of the indentation, not a buffer position."
  (if (and (boundp 'indent-line-function)
	   (fboundp indent-line-function))
      (let (
	    (mstart (make-marker))
	    (mend (make-marker))
	    column save-line
	    before-change-function before-change-functions
	    after-change-function after-change-functions
	    )
	;; We must calculate the correct indentation for the current
	;; line.  Unfortunately, the only function we can count on
	;; having is `indent-line-function', which will actually
	;; intent the the line.  So, we will first save the current
	;; line in a string, call `indent-line-function', then put the
	;; line back the way it was.
	(save-excursion
	  (unwind-protect
	      (progn
		(goto-char bufpos)
		(end-of-line)
		(set-marker mend (point))
		(beginning-of-line)
		(set-marker mstart (point))
		(setq save-line (buffer-substring mstart mend))
		(funcall indent-line-function)
		(back-to-indentation)
		(setq column (current-column))
		(delete-region mstart mend)
		(goto-char mstart)
		(insert save-line))
	    (set-marker mstart nil)
	    (set-marker mend nil))
	  column))))


(defun bc-compute-indent (bufpos)
  "Return an appropriate indentation for the line containing BUFPOS.
Calls the function designated by `indent-line-function', if that is
non-nil.  In this way, the indentation is computed by the major mode.

Returns the column of the indentation, not a buffer position."
  (if (and (boundp 'indent-line-function)
	   (fboundp indent-line-function))
      (let (
	    (mstart (make-marker))
	    (mend (make-marker))
	    column
	    before-change-function before-change-functions
	    after-change-function after-change-functions
	    )
	(save-excursion
	  (unwind-protect
	      (progn
		(goto-char bufpos)
		(beginning-of-line)
		(set-marker mstart (point))
		(insert "\n")
		(set-marker mend (point))
		(forward-char -1)
		(funcall indent-line-function)
		(setq column (current-column))
		(delete-region mstart mend))
	    (set-marker mstart nil)
	    (set-marker mend nil))
	  column))))


;;;;======================================================================
;;;_    | bc-save-excursion
(defun bc-save-excursion (current-point first-line body-first body-last
					last-line &optional
					stick-to-delims)
  "Saves the location of point so that it can be put back in a sensible fasion
after a bc formatting command.  If point is not inside the comment
when the command is called (i.e. it is in the code), we want point to end up
in exactly that place in the code afterward.  If, on the other hand, point is
inside the comment, then we want it to end up at the same place in the comment
afterward.  Both of these will be, in general, quite different from simply
returning point to the same character position in the buffer \(as with
save-excursion\).

CURRENT-POINT is the buffer position to be saved.  The other four
arguments are the buffer positions of the beginnings of lines.

If STICK-TO-DELIMS is non-nil, it effects the count of non-whitespace
characters returned when the result is ('in-comment 'in-end-delimiter
count).  Normally the characters are counted to the center of the
comment text on the line.  If this argument is non-nil they are
counted to the end of the comment text on the line.

This function returns a structure which can be used by
`bc-restore-excursion'.  Its structure is a list of three elements:
\(`vert' `horiz' `pos'\).

`vert' will be one of the following: 'in-top-banner, 'in-bottom-banner
'in-top-space, 'in-bottom-space, 'in-comment, 'in-code, or
'past-comment.

If `vert' is 'in-code, `horiz' is the line number and `pos' is the
column. If `vert' is 'past-comment, `horiz' is the line number and
`pos' is nil.

Otherwise, `horiz' will be one of the following: 'in-start-delimiter,
'in-text, 'in-end-delimiter, or 'past-end.

If `vert' is one of 'in-top-banner, 'in-bottom-banner 'in-top-space,
or 'in-bottom-space, `pos' has a value depending on `horiz':

       HORIZ                                  POS
-------------------	----------------------------------------------------
'in-start-delimiter	Count of character columns from beginning of start
'in-text		delimiter to point.

'in-end-delimiter	Count of columns from beginning of end delimiter.


If `vert' is 'in-comment, `pos' has a value depending on `horiz':

       HORIZ                                  POS
-------------------	----------------------------------------------------
'in-start-delimiter	\(`count'. `column'\), where `count' indicates how far
'in-end-delimiter	point is into the comment as a count of non-whitespace
			characters, and `column' indicates in which column of
			the delimiter point is.

			\(`count' . `whitep'\) where `count' indicates how far
'in-text		point is into the comment as a count of non-whitespace
			characters, and `whitep' is non-nil if point is before
                        the first nonwhite char following whitespace."

  ;; TODO: I should be able to simplify this by using a couple of
  ;; markers rather than character counts. bc-get-text, bc-put-text,
  ;; etc. would have to be updated to duplicate the marker to and from
  ;; the scratch buffer.

  (let
      (vert horiz pos point-column count
	    start-delim-b start-delim-e body-b body-e end-delim-b end-delim-e
	    (first-line-e (progn (goto-char first-line) (end-of-line) (point)))
	    (body-last-e (progn (goto-char body-last) (end-of-line) (point)))
;;;	    code-end-pos 
	    )
    (bc-dummy start-delim-e) ; quiet the byte compiler

						; Make sure we are not stranded
    (save-excursion				; in that no-man's-land of
						; extra space to the right of
      (goto-char current-point)			; the comment body. We want to
      (if (looking-at "[ \t]*$")		; treat such cases as though
	  (skip-chars-backward " \t"))		; point is `in-comment, so move
      (setq current-point (point))		; to just past the rightmost
      (setq point-column (current-column))	; nonwhite.
      
						;==============================
						; Point is outside the comment.
      (bc-analyze-line)				;==============================
      (cond
       ((< current-point start-delim-b)		; Point is in the code.
	(setq vert 'in-code
	      horiz (bc-count-lines-1-fast 1 current-point)
	      pos point-column))
						  
       ((< end-delim-e current-point)		; Point is past the comment.
	(setq vert 'past-comment
	      horiz (bc-count-lines-1-fast 1 current-point)	
	      pos nil))
       
       (t	; Point is somewhere in the comment. Figure out on what line of
		; the comment we are. If this is not a banner comment then
		; first-line == body-first and last-line == body-last

	(setq vert 'in-top-banner)
	(if (< first-line-e current-point) (setq vert 'in-top-space))
	(if (<= body-first current-point)  (setq vert 'in-comment))
	(if (= body-last last-line)
	    nil
	  (if (< body-last-e current-point)  (setq vert 'in-bottom-space))
	  (if (<= last-line current-point)   (setq vert 'in-bottom-banner)))

						;==============================
	(cond					; Point is in the body of the
	 ((eq vert 'in-comment)			; comment.
	  (cond					;==============================
	   
	   ((< current-point body-b)		; Point in start delimiter.
	    (goto-char start-delim-b)
	    (setq horiz 'in-start-delimiter
		  count (car (bc-count-nonwhite
			      body-first		; Stick to the middle
			      (/ (+ start-delim-b	; of the text.
				    end-delim-e) 2)))
		  pos (cons count (- point-column (current-column)))))
	   
	   ((or (< current-point end-delim-b)	; Point in text of comment.
		
						; If the language uses newline
						; as the ending delimiter, then
		(equal bc-comment-end ""))	; anything to the right of the
						; comment text should be
						; considered to be in the body
						; of the comment.
	    (if (< body-e current-point) (setq current-point body-e))
	    (setq horiz 'in-text
		  count (bc-count-nonwhite body-first current-point)
		  pos 
		  (cons (car count)				  ; Is point on
			(or (= current-point body-b)		  ; the first
			    (and (not (cdr count))		  ; nonwhite
				 (progn (goto-char current-point) ; char after
					(backward-char)		  ; some white?
					(looking-at "[ \t]")))))))
	   
	   (t					; Point in end delimiter.
	    (goto-char end-delim-e)
	    (setq horiz 'in-end-delimiter	; Normally the characters are
						; counted to the center of the
						; comment text on the line.
		  count (car			; This is so that point will
			 (bc-count-nonwhite	; appear to stay in roughly the
			  body-first		; same place in the comment
			  (if stick-to-delims	; after a series of formatting
			      end-delim-e	; commands. This is
			    (/			; undesirable, however, when
			     (+			; processing `do-auto-fill'. If
			      start-delim-b	; STICK-TO-DELIMS is non-nil,
			      end-delim-e)	; count non-whitespace
			     2))))		; characters to the end of the
						; comment text on the line.

		  pos (cons count (- (current-column) point-column))))))
	 
						;==============================
						; Point is in the banner.
	 (t					;==============================
	  (cond	
	   ((or (< current-point body-e)	; Point in start or text.
		(equal bc-comment-end ""))
	    (goto-char start-delim-b)
	    (setq horiz 'in-text
		  pos (- point-column (current-column))))
	   
	   (t					; Point in end delimiter.
	    (goto-char end-delim-e)
	    (setq horiz 'in-end-delimiter
		  pos (- (current-column) point-column))))))))
      
      
      (list vert horiz pos))))			; return value

;;;;======================================================================
;;;_    | bc-restore-excursion
(defun bc-restore-excursion (save-point first-line last-line)
  "Takes a structure, SAVE-POINT created by bc-save-excursion
and places point at the position indicated in the structure.  

FIRST-LINE is the character position of the first line of the new
location of the comment.

LAST-LINE is the character position of the last line of the new
location of the comment."

  (let ((vert (car save-point))
	(horiz (nth 1 save-point))
	(pos (nth 2 save-point))
	com-delim-start-pos com-delim-end-pos com-body-start-pos
	com-body-end-pos body-first
	)

    (goto-char first-line)
    (if bc-banner-top-border (forward-line (+ 1 bc-banner-top-space)))
    (setq body-first (point))
						;==============================
						; Point is outside the comment.
    (cond					;==============================
     ((eq vert 'in-code)
      (bc-goto-line-fast horiz)				; Point is in the code.
      (move-to-column pos))
     
     ((eq vert 'past-comment)			; Point is past the comment.
      (bc-goto-line-fast horiz)
      (end-of-line))
						;==============================
     (t						; Point is in the body of the
      (cond					; comment.
       ((eq vert 'in-comment)			;==============================
	(cond
	 ((eq horiz 'in-start-delimiter)	; Point in start delimiter.
	  (if (bc-forward-nonwhite body-first (car pos)) nil
	    (bc-oparse-line nil 'com-delim-start-pos 'com-body-start-pos)
	    (goto-char com-delim-start-pos)
	    (forward-char (cdr pos))
	    (if (< com-body-start-pos (point))
		(goto-char com-body-start-pos))))

	 ((eq horiz 'in-text)			; Point in comment text.
	  (bc-forward-nonwhite body-first (+ (car pos) (if (cdr pos) 1 0)))
	  (if (cdr pos) (backward-char)))

	 (t					; Point in end delimiter.
	  (bc-forward-nonwhite body-first (car pos))
	  (bc-oparse-line nil nil nil nil 'com-delim-end-pos)
	  (goto-char com-delim-end-pos)
	  (backward-char (cdr pos)))))

       (t					;==============================
	(cond					; Point is in the banner.
	 ((eq vert 'in-top-banner)		;==============================
	  (goto-char first-line))
	 ((eq vert 'in-top-space)
	  (goto-char first-line)
	  (forward-line 1))
	 ((eq vert 'in-bottom-banner)
	  (goto-char last-line))
	 ((eq vert 'in-bottom-space)
	  (goto-char last-line)
	  (forward-line -1)))
	  
	(bc-oparse-line nil 'com-delim-start-pos nil 'com-body-end-pos
		       'com-delim-end-pos)
	(cond
	 ((memq horiz '(in-start-delimiter in-text))
	  (goto-char com-delim-start-pos)
	  (move-to-column (+ (current-column) pos))
	  (goto-char (min (point) com-body-end-pos)))
	 (t						; 'in-end-delimiter
	  (goto-char com-delim-end-pos)
	  (move-to-column (- (current-column) pos))
	  (goto-char (min (point) com-delim-end-pos))))))))
    ))
   
;;;;======================================================================
;;;_    | bc-count-lines-1
(defun bc-count-lines-1 (start end)
  "This function is like `count-lines' only it counts lines inclusively.
So, if START == END, or if END is at the beginning of the line, this
function returns one more than `count-lines' would."

  (save-excursion
    (save-restriction
      (widen)
      (1+ (count-lines
	   (progn (goto-char start) (beginning-of-line) (point))
	   (progn (goto-char end) (beginning-of-line) (point)))))))

;  (save-excursion
;    (save-restriction
;      (widen)
;      (let ((new-start (progn (goto-char start) (beginning-of-line) (point)))
; 	   (new-end (progn (goto-char end) (beginning-of-line) (point)))
; 	   )
;        (narrow-to-region new-start new-end)
;        (goto-char (point-min))
;        (- (buffer-size) (forward-line (buffer-size)) -1)))))

;;;;======================================================================
;;;_    | bc-count-nonwhite
(defun bc-count-nonwhite (first-line target-point)
  "Counts the number of non-whitespace characters contained in the
block comment starting on FIRST-LINE up to TARGET-POINT.  TARGET-POINT
is assumed to be in the block comment.

Returns \(count . end-is-whitespace-p\)

  *** Moves point ***"

  (let
      ((sum 0)
       limit
       (done nil)
       count-this-line
;;;       code-end-pos 
       com-body-start-pos com-body-end-pos 
       )

    (save-excursion
      (goto-char first-line)
      (while (not done)
	(bc-oparse-line nil nil 'com-body-start-pos 'com-body-end-pos)
	(if (< com-body-end-pos target-point)
	    (setq limit com-body-end-pos)
	  (setq done t)
	  (setq limit target-point))
	(setq count-this-line (bc-count-nonwhite-chars 
			       com-body-start-pos limit)
	      sum (+ sum (car count-this-line)))
	(forward-line 1)))

    (cons sum (cdr count-this-line))))		; return the total

;;;;======================================================================
;;;_    | bc-forward-nonwhite
(defun bc-forward-nonwhite (first-line count)
  "Moves point forward by COUNT non-whitespace characters contained in the
block comment starting on FIRST-LINE."

  (let
      (done
;;;       code-end-pos com-delim-start-pos com-delim-end-pos
       com-body-start-pos
       com-body-end-pos
       (last (point)) failure
       )
    (goto-char first-line)
    (while (not done)
      (if (not (bc-oparse-line nil nil 'com-body-start-pos 'com-body-end-pos))
	  (progn
	    (setq done t
		  failure t)
	    (goto-char last))
	(setq count (- count (bc-forward-nonwhite-chars
			      com-body-start-pos count com-body-end-pos))))
      (setq last (point))
      (if (= count 0)
	  (setq done t)
	(forward-line 1)))
    failure))

;;;;======================================================================
;;;_    | bc-count-nonwhite-chars
(defun bc-count-nonwhite-chars (begin end)
  "Counts the number of non-whitespace characters between BEGIN and END.

Returns \(count> . <end-is-whitespace-p\)

  *** Moves point ***"

  (let ((counter 0) prev done)
    (goto-char begin)

    (while (and (not done) (not (eolp)))
      (skip-chars-forward " \t")
      (if (< end (point))
	  (setq done t)
	(setq prev (point)) 
	(skip-chars-forward "^ \t\n")
	(if (< end (point))
	    (setq done t
		  counter (+ counter (- end prev)))
	  (setq counter (+ counter (- (point) prev))))))
    (cons counter (looking-at "[ \t\n]"))
    ))
;;;	
;;;    (while (< (point) end)			; if point is before whitespace
;;;	 (if (looking-at "[ \t\n]")		; don't increment counter
;;;	     nil
;;;	   (setq counter (1+ counter)))		; otherwise count this as
;;;	 (forward-char 1))				; nonwhite
;;;    (cons counter (looking-at "[ \t\n]"))))

;;;;======================================================================
;;;_    | bc-forward-nonwhite-chars
(defun bc-forward-nonwhite-chars (begin count end)
  "Moves point to BEGIN and then forward by COUNT non-whitespace
characters, but never past END.  

Returns the distance actually moved.

  *** Moves point ***"

  (let ((counter 0) prev done)
    (goto-char begin)

    (while (and (not done) (not (eolp)))
      (skip-chars-forward " \t")
      (if (< end (point))
	  (progn (setq done t) (goto-char end))
	(setq prev (point)) 
	(skip-chars-forward "^ \t\n")
	(setq counter (+ counter (- (point) prev)))
	(cond
	 ((< end (point))
	  (setq done t) (goto-char end))
	 ((<= count counter)
	  (setq done t)
	  (forward-char (- count counter))
	  (setq counter count)))))
    counter))

;;;    (while (and (< counter count)
;;;		   (< (point) limit))		; if point is before whitespace
;;;	 (if (looking-at "[ \t\n]")		; don't increment counter
;;;	     nil
;;;	   (setq counter (1+ counter)))		; otherwise count this as
;;;	 (forward-char 1))				; nonwhite
;;;    counter))

;;;;======================================================================
;;;_   o Miscellaneous
;;;_    | bc-get-buffer-create
(defun bc-get-buffer-create ()
   (let ((scratch-buffer (get-buffer-create bc-scratch-buffer)))
     (buffer-disable-undo scratch-buffer)
     scratch-buffer))

;;;;======================================================================
;;;_    | bc-set-pref-first-line
(defun bc-set-pref-first-line ()
  (setq bc-pref-first-line (bc-count-lines-1-fast
			    1 
			    (marker-position bc-start-marker)))
  (set-marker bc-start-marker nil))

;;;;======================================================================

(provide 'bc-mode)

;;;_ & 

;;; Local Variables:
;;; kept-new-versions: 999
;;; oe-header-prefix: ";;;_"
;;; mode: outline-minor
;;; End:

;;; bc-mode.el ends here
