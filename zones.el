;;; zones.el --- Zones of text - like multiple regions
;;
;; Filename: zones.el
;; Description:  Zones of text - like multiple regions
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2010-2017, Drew Adams, all rights reserved.
;; Created: Sun Apr 18 12:58:07 2010 (-0700)
;; Version: 2015-08-16
;; Package-Requires: ()
;; Last-Updated: Sun Jan  1 12:02:07 2017 (-0800)
;;           By: dradams
;;     Update #: 1728
;; URL: http://www.emacswiki.org/zones.el
;; Doc URL: http://www.emacswiki.org/Zones
;; Doc URL: http://www.emacswiki.org/MultipleNarrowings
;; Keywords: narrow restriction widen region zone
;; Compatibility: GNU Emacs 20.x, 21.x, 22.x, 23.x, 24.x, 25.x,
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;     Zones of text - like multiple regions.
;;
;;    More description below.
;;
;;    Bug reports etc.: (concat "drew" ".adams" "@" "oracle" ".com")
;;      
 
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
;;  http://www.emacswiki.org/emacs/download/linkd.el.
;;
;;  (@> "Things Defined Here")
;;  (@> "Documentation")
;;    (@> "Compatibility")
;;    (@> "Coalesced (United) Zones")
;;    (@> "Izone Commands")
;;    (@> "Izone List Variables")
;;    (@> "Keys")
;;    (@> "Command `zz-narrow-repeat'")
;;    (@> "Define Your Own Commands")
;;  (@> "Change log")
 
;;(@* "Things Defined Here")
;;
;;  Things Defined Here
;;  -------------------
;;
;;  Commands defined here:
;;
;;    `zz-add-zone', `zz-add-zone-and-coalesce',
;;    `zz-add-zone-and-unite', `zz-clone-and-coalesce-zones',
;;    `zz-clone-and-unite-zones', `zz-clone-zones',
;;    `zz-coalesce-zones', `zz-delete-zone', `zz-narrow',
;;    `zz-narrow-repeat', `zz-select-region',
;;    `zz-select-region-repeat', `zz-set-izones-var',
;;    `zz-unite-zones'.
;;
;;  Non-interactive functions defined here:
;;
;;    `zz-buffer-of-markers', `zz-car-<', `zz-every',
;;    `zz-izone-has-other-buffer-marker-p', `zz-izone-limits',
;;    `zz-izone-limits-in-bufs', `zz-izones', `zz-izones-from-zones',
;;    `zz-izones-p', `zz-izones-renumber', `zz-marker-from-object',
;;    `zz-markerize', `zz-max', `zz-min', `zz-narrowing-lighter',
;;    `zz-number-or-marker-p', `zz-rassoc-delete-all',
;;    `zz-readable-marker', `zz-readable-marker-p',
;;    `zz-read-any-variable', `zz-read-bufs', `zz-regexp-car-member',
;;    `zz-remove-if', `zz-remove-if-not',
;;    `zz-remove-izones-w-other-buffer-markers',
;;    `zz-remove-zones-w-other-buffer-markers', `zz-repeat-command',
;;    `zz-set-intersection', `zz-set-union', `zz-some',
;;    `zz-string-match-p', `zz-two-zone-intersection',
;;    `zz-two-zone-union', `zz-zones-complement',
;;    `zz-zone-has-other-buffer-marker-p', `zz-zone-intersection',
;;    `zz-zone-intersection-1', `zz-zone-ordered',
;;    `zz-zones-overlap-p', `zz-zones-same-buffer-p',
;;    `zz-zone-union', `zz-zone-union-1'.
;;
;;  Internal variables defined here:
;;
;;    `zz-izones', `zz-izones-var', `zz-lighter-narrowing-part',
;;    `zz-add-zone-anyway-p'.
;;
;;
;;  ***** NOTE: This EMACS PRIMITIVE has been ADVISED HERE:
;;
;;    `narrow-to-region'.
;;
;;
;;  ***** NOTE: The following functions defined in `lisp.el' and
;;              `page.el' have been REDEFINED here:
;;
;;    `narrow-to-defun', `narrow-to-page'.
 
;;(@* "Documentation")
;;
;;  Documentation
;;  -------------
;;
;;  Library `zones.el' lets you easily define and subsequently act on
;;  multiple zones of buffer text.  You can think of this as enlarging
;;  the notion of "region".  In effect, it can remove the requirement
;;  of target text being a contiguous sequence of characters.  A set
;;  of buffer zones is, in effect, a (typically) noncontiguous
;;  "region" of text.
;;
;;
;;(@* "Compatibility")
;;  ** Compatibility **
;;
;;  Some of the functions defined here are not available for Emacs
;;  versions prior to Emacs 22.  Others are not available for versions
;;  prior to Emacs 23.  This is mentioned where applicable.
;;
;;
;;(@* "Zones")
;;  ** Zones **
;;
;;  A "zone" is a basic zone or an izone.  A zone represents the text
;;  between its two positions, just as an Emacs region is the text
;;  between point and mark.
;;
;;  A "basic zone" is a list of two buffer positions followed by a
;;  possibly empty list of extra information: (POS1 POS2 . EXTRA).
;;
;;  An "izone" is a list whose first element is an identifier that is
;;  is a natural number (1, 2, 3,...)  and whose cdr is a basic zone:
;;  (ID POS1 POS2 . EXTRA).
;;
;;  The positions of a zone can be natural numbers (1, 2, 3,...),
;;  markers for the same buffer, or readable markers for the same
;;  buffer.  (Behavior is undefined if a zone has markers for
;;  different buffers.)  Each position of a given zone can take any of
;;  these forms.
;;
;;  A "readable marker" is a list (marker BUFFER POSITION), where
;;  BUFFER is a buffer name (string) and where POSITION is a buffer
;;  position (number only).
;;
;;  The positions of a zone can be in either numeric order.  The
;;  positions are also called the zone "limits".  The lower limit is
;;  called the zone "beginning"; the upper limit is called its "end".
;;
;;
;;(@* "Coalesced (United) Zones")
;;  ** Coalesced (United) Zones **
;;
;;  A list of zones can contain zones that overlap or are adjacent
;;  (the end of one is one less than the beginning of the other).
;;
;;  Basic-zone union and intersection operations (`zz-zone-union',
;;  `zz-zone-intersection') each act on a list of zones, returning
;;  another such list, but which has POS1 <= POS2 in each of its
;;  zones, and which lists its zones in ascending order of their cars.
;;  For basic-zone union, the resulting zones are said to be
;;  "coalesced", or "united".
;;
;;  The extra info in the zones that result from zone union or
;;  intersection is just the set union or set intersection of the
;;  extra info in the zones that are combined.
;;
;;  After a list of zones has been altered by `zz-zone-union' or
;;  `zz-zone-intersection':
;;
;;  * Each zone in the result list is ordered so that its first
;;    element is smaller than its second.
;;
;;  * The zones in the result list have been sorted in ascending order
;;    by their first elements.
;;
;;  * The zones in the result list are not adjacent and do not
;;    overlap: there is some other buffer text (i.e., not in any zone)
;;    between any two zones in the result.
;;
;;
;;(@* "Izone Commands")
;;  ** Izone Commands **
;;
;;  Commands that manipulate lists of zones generally use izones,
;;  because they make use of the zone identifiers.
;;
;;  Things you can do with zones:
;;
;;  * Sort them.
;;
;;  * Unite (coalesce) adjacent or overlapping zones (which includes
;;    sorting them).
;;
;;  * Intersect them.
;;
;;  * Narrow the buffer to any of them.  Cycle among narrowings.  If
;;    you use library `icicles.el' then you can also navigate among
;;    them in any order, and using completion against BEG-END range
;;    names.
;;
;;  * Select any of them as the active region.  Cycle among regions.
;;
;;  * Search them (they are automatically coalesced first).  For this
;;    you need library `isearch-prop.el'.
;;
;;  * Make a set of zones or its complement (the anti-zones)
;;    invisible.  For this you also need library `isearch-prop.el'.
;;
;;  * Highlight and unhighlight them.  For this you need library
;;    `highlight.el' or library `facemenu+.el' (different kinds of
;;    highlighting).
;;
;;  * Add the active region to a list of zones.
;;
;;  * Add the region to a list of zones, and then unite (coalesce) the
;;    zones.
;;
;;  * Delete an izone from a list of zones.
;;
;;  * Clone a zones variable to another one, so the clone has the same
;;    zones.
;;
;;  * Clone a zones variable and then unite the zones of the clone.
;;
;;  * Make an izone variable persistent, in a bookmark.  Use the
;;    bookmark to restore it in a subsequent Emacs session.  For this
;;    you need library `bookmark+.el'.
;;
;;
;;(@* "Izone List Variables")
;;  ** Izone List Variables **
;;
;;  Commands that use izones generally use a variable that holds a
;;  list of them.  By default, this is the buffer-local variable
;;  `zz-izones'.  But such a variable can be buffer-local or global.
;;  If it is global then it can use markers and readable markers for
;;  different buffers.
;;
;;  The value of variable `zz-izones-var' is the variable currently
;;  being used by default for izone commands.  The default value is
;;  `zz-izones'.
;;
;;  You can have any number of izones variables, and they can be
;;  buffer-local or global variables.
;;
;;  You can use `C-x n v' (command `zz-set-izones-var') anytime to set
;;  `zz-izones-var' to a variable whose name you enter.  With a prefix
;;  argument, the variable is made automatically buffer-local.  Use
;;  `C-x n v' to switch among various zone variables for the current
;;  buffer (if buffer-local) or globally.
;;
;;  Sometimes another zone command prompts you for the izones variable
;;  to use, if you give it a prefix argument.  The particular prefix
;;  arg determines whether the variable, if not yet bound, is made
;;  buffer-local, and whether `zz-izones-var' is set to the variable
;;  symbol:
;;
;;   prefix arg         buffer-local   set `zz-izones-var'
;;   ----------         ------------   -------------------
;;    Plain `C-u'        yes            yes
;;    > 0 (e.g. `C-1')   yes            no
;;    = 0 (e.g. `C-0')   no             yes
;;    < 0 (e.g. `C--')   no             no
;;
;;  For example, `C-u C-x n a' (`zz-add-zone') prompts you for a
;;  different variable to use, in place of the current value of
;;  `zz-izones-var'.  The variable you enter is made buffer-local and
;;  it becomes the new default izones variable for the buffer; that
;;  is, `zz-izones-var' is set to the variable symbol.
;;
;;  As another example, suppose that `zz-izones-var' is `zz-izones',
;;  the default value and buffer-local by design.  If you then use
;;  `C-- C-x n s' and enter a variable name at the prompt, that
;;  variable is not made buffer-local, and `zz-izones-var' is not set
;;  to that variable.  The active region is pushed to the variable,
;;  but because `zz-izones-var' is unchanged, a subsequent `C-x n s'
;;  (no prefix arg) pushes to `zz-izones'.
;;
;;
;;(@* "Keys")
;;  ** Keys **
;;
;;  Most of the commands that manipulate izones are bound on keymap
;;  `narrow-map'.  They are available on prefix key `C-x n', along
;;  with the narrowing/widening keys `C-x n d', `C-x n n', `C-x n p',
;;  and `C-x n w':
;;
;;  C-x n a   `zz-add-zone' - Add to current izones variable
;;  C-x n A   `zz-add-zone-and-unite' - Add izone, then unite izones
;;  C-x n c   `zz-clone-zones' - Clone zones from one var to another
;;  C-x n C   `zz-clone-and-unite-zones' - Clone then unite zones
;;  C-x n d   `narrow-to-defun'
;;  C-x n C-d `zz-delete-zone' - Delete an izone from current var
;;  C-x n h   `hlt-highlight-regions' - Highlight izones
;;  C-x n H   `hlt-highlight-regions-in-buffers' - in multiple buffers
;;  C-x n n   `narrow-to-region'
;;  C-x n p   `narrow-to-page'
;;  C-x n r   `zz-select-region-repeat' - Cycle as active regions
;;  C-x n u   `zz-unite-zones' - Unite (coalesce) izones
;;  C-x n v   `zz-set-izones-var' - Set `zz-izones-var' to a variable
;;  C-x n w   `widen'
;;  C-x n x   `zz-narrow-repeat' - Cycle as buffer narrowings
;;
;;
;;(@* "Command `zz-narrow-repeat'")
;;  ** Command `zz-narrow-repeat' **
;;
;;  Library `zones.el' modifies commands `narrow-to-region',
;;  `narrow-to-defun', and `narrow-to-page' (`C-x n n', `C-x n d',
;;  and `C-x n p') so that the current buffer restriction
;;  (narrowing) is added to the izone list the current buffer (by
;;  default, buffer-local variable `zz-izones').
;;
;;  You can then use `C-x n x' to cycle among previous buffer
;;  narrowings.  Repeating `x' repeats the action: `C-x n x x x x'
;;  etc.  Each time you hit `x' a different narrowing is made current.
;;  This gives you an easy way to browse your past narrowings.
;;
;;  If the izone variable is not buffer-local then `zz-narrow-repeat'
;;  can cycle among the narrowings in different buffers, switching the
;;  buffer accordingly.
;;
;;  Invoking `C-x n x' with a prefix argument changes the behavior
;;  as follows:
;;
;;  * A plain prefix arg (`C-u') widens the buffer completely.
;;
;;  * A zero numeric prefix arg (e.g `C-0') widens completely and
;;    resets (empties) the current izone variable.
;;
;;  * A numeric prefix arg N takes you directly to the abs(N)th
;;    previous buffer narrowing.  That is, it widens abs(N) times.
;;    Positive and negative args work the same, except that a negative
;;    arg also pops entries off the ring: it removes the ring entries
;;    from the most recent back through the (-)Nth one.
;;
;;  By default, `C-x n x' is bound to command `zz-narrow-repeat'.
;;  (For Emacs versions prior to 22 it is bound by default to
;;  `zz-narrow', which is a non-repeatable version.  Repeatability is
;;  not available before Emacs 22.)
;;
;;  The mode-line lighter `Narrow' is still used for the ordinary
;;  Emacs narrowing commands.  But for `zz-narrow-repeat' (`C-x n x')
;;  the current narrowing is indicated in the lighter by an
;;  identifying number: `Narrow-1', `Narrow-2', and so on.  `mouse-2'
;;  on the `Narrow' part still widens completely, but `mouse-2' on the
;;  `-NUM' part uses `zz-narrow-repeat' to cycle to the next
;;  narrowing.
;;
;;
;;(@* "Define Your Own Commands")
;;  ** Define Your Own Commands **
;;
;;  Pretty much anything you can do with the Emacs region you can do
;;  with a set of zones (i.e., with a non-contiguous "region").  But
;;  existing Emacs commands that act on the region do not know about
;;  non-contiguous regions.  What you will need to do is define new
;;  commands that take these into account.
;;
;;  You can define your own commands that iterate over a list of
;;  izones in a given buffer, or over such lists in a set of buffers.
;;  Utility functions `zz-izone-limits', `zz-izone-limits-in-bufs',
;;  and `zz-read-bufs' can help with this.
;;
;;  As examples of such commands, if you use library `highlight.el'
;;  then you can use `C-x n h' (command `hlt-highlight-regions') to
;;  highlight the izones recorded for the current buffer.  You can use
;;  `C-x n H' (command `hlt-highlight-regions-in-buffers') to do the
;;  same across a set of buffers that you specify (or across all
;;  visible buffers).  If option `hlt-auto-faces-flag' is non-nil then
;;  each region gets a different face.  Otherwise, all of the regions
;;  are highlighted with the same face.  Complementary (unbound)
;;  commands `hlt-unhighlight-regions' and
;;  `hlt-unhighlight-regions-in-buffers' unhighlight.
;;
;;  Defining your own command can be simple or somewhat complex,
;;  depending on how the region is used in the code for the
;;  corresponding region-action Emacs command.  The definition of
;;  `hlt-highlight-regions' just calls existing function
;;  `hlt-highlight-region' once for each recorded region:
;;
;; (defun hlt-highlight-regions (&optional regions face msgp mousep
;;                                         buffers)
;;   "Apply `hlt-highlight-region' to regions in `zz-izones'."
;;   (interactive (list (zz-izone-limits) nil t current-prefix-arg))
;;   (dolist (start+end  regions)
;;     (hlt-highlight-region (nth 0 start+end) (nth 1 start+end)
;;                           face msgp mousep buffers)))
;;    
;;  That's it - just iterate over `zz-izones' with a function that
;;  takes the region as an argument.  What `zones.el' offers in this
;;  regard is a way to easily define a set of buffer zones.
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;(@* "Change log")
;;
;; 2016/02/09 dadams
;;     zz-zones-complement: Removed unused optional BUFFER arg.  Use zz-marker-from-object on BEG and END.
;; 2015/09/07 dadams
;;     zz-some: Return cons (ELEMENT . VALUE).
;;     zz-buffer-of-markers: Adjust for new zz-some behavior.
;; 2015/08/23 dadams
;;     Added: zz-clone-zones, zz-clone-and-unite-zones, zz-clone-and-coalesce-zones (alias).
;;     Bind zz-clone-zones to C-x n c, zz-clone-and-unite-zones to C-x n C.
;;     Added: zz-add-zone-and-unite, zz-unite-zones.  Alias zz-add-zone-and-coalesce, zz-coalesce-zones to them.
;;     Bind zz-unite-zones to C-x n u, not C-x n c.
;;     zz-set-izones-var: Corrected interactive spec.
;;     zz-remove-zones-w-other-buffer-markers: Typo: RESTR -> ZONE.
;; 2015/08/22 dadams
;;     Added: zz-set-izones-var.  Bind it to C-x n v.
;; 2015/08/18 dadams
;;     Renamed: zz-izone-add              to zz-add-zone,
;;              zz-izone-add-and-coalesce to zz-add-zone-and-coalesce,
;;              zz-izone-delete           to zz-delete-zone,
;;              zz-izones-coalesce        to zz-coalesce-zones,
;;              zz-izone-add-anyway-p     to zz-add-zone-anyway-p.
;;     Command doc strings: Mention "zone" instead of "izone" (simpler).
;;     Changed binding of zz-coalesce-zones        to C-x n c from C-x n u.
;;     Changed binding of zz-add-zone              to C-x n a from C-x n s.
;;     Changed binding of zz-add-zone-and-coalesce to C-x n A from C-x n S.
;;     zz-izones-from-zones: Renamed arg ZONES to BASIC-ZONES.
;;     zz-narrowing-lighter: Moved mode-line-modes boundp guard here.
;;     zz-narrow: Removed mode-line-modes boundp guard.  OK to set zz-lighter-narrowing-part here.
;;     zz-select-region: Removed unneeded binding of zz-add-zone-anyway-p.
;; 2015/08/16 dadams
;;     Merged content of wide-n.el here (wide-n.el is obsolete now - this replaces it).
;;     Added: zz-zone-has-other-buffer-marker-p.
;;     Renamed:
;;      wide-n                                 to zz-narrow
;;      wide-n-add-to-union                    to zz-izone-add-and-coalesce
;;      wide-n-delete                          to zz-izone-delete
;;      wide-n-highlight-lighter               to zz-narrowing-lighter
;;      wide-n-lighter-narrow-part             to zz-lighter-narrowing-part
;;      wide-n-limits                          to zz-izone-limits
;;      wide-n-limits-in-bufs                  to zz-izone-limits-in-bufs
;;      wide-n-marker-from-object              to zz-marker-from-object
;;      wide-n-markerize                       to zz-markerize
;;      wide-n-mem-regexp                      to zz-regexp-car-member
;;      wide-n-number-or-marker-p              to zz-number-or-marker-p
;;      wide-n-other-buffer-marker-p           to zz-izone-has-other-buffer-marker-p
;;      wide-n-push                            to zz-izone-add
;;      wide-n-push-anyway-p                   to zz-izone-add-anyway-p
;;      wide-n-rassoc-delete-all               to zz-rassoc-delete-all
;;      wide-n-readable-marker                 to zz-readable-marker
;;      wide-n-readable-marker-p               to zz-readable-marker-p
;;      wide-n-read-any-variable               to zz-read-any-variable
;;      wide-n-read-bufs                       to zz-read-bufs
;;      wide-n-remove-if                       to zz-remove-if
;;      wide-n-remove-if-other-buffer-markers  to zz-remove-izones-w-other-buffer-markers
;;      wide-n-remove-if-not                   to zz-remove-if-not
;;      wide-n-renumber                        to zz-izones-renumber
;;      wide-n-repeat                          to zz-narrow-repeat
;;      wide-n-repeat-command                  to zz-repeat-command
;;      wide-n-restrictions                    to zz-izones
;;      wide-n-restrictions-from-zones         to zz-izones-from-zones
;;      wide-n-restrictions-p                  to zz-izones-p
;;      wide-n-restrictions-var                to zz-izones-var
;;      wide-n-select-region                   to zz-select-region
;;      wide-n-select-region-repeat            to zz-select-region-repeat
;;      wide-n-string-match-p                  to zz-string-match-p
;;      wide-n-unite                           to zz-izones-coalesce
;;      zzz-buffer-of-markers                  to zz-buffer-of-markers
;;      zzz-car-<                              to zz-car-<
;;      zzz-every                              to zz-every
;;      zzz-max                                to zz-max
;;      zzz-min                                to zz-min
;;      zzz-ordered-zone                       to zz-zone-ordered
;;      zzz-remove-if                          to zz-remove-if
;;      zzz-remove-if-other-buffer-markers     to zz-remove-zones-w-other-buffer-markers
;;      zzz-set-union                          to zz-set-union
;;      zzz-set-intersection                   to zz-set-intersection
;;      zzz-some                               to zz-some
;;      zzz-two-zone-intersection              to zz-two-zone-intersection
;;      zzz-two-zone-union                     to zz-two-zone-union
;;      zzz-zone-complement                    to zz-zones-complement
;;      zzz-zone-intersection(-1)              to zz-zone-intersection(-1)
;;      zzz-zones-overlap-p                    to zz-zones-overlap-p
;;      zzz-zones-same-buffer-p                to zz-zones-same-buffer-p
;;      zzz-zone-union(-1)                     to zz-zone-union(-1)
;;
;;     Added Emacs 20 compatibility.
;;     zz-narrow: If only one narrowing and buffer is narrowed, widen.  (Do not widen if already widened.)
;;                Protect Emacs 20 from wide-n-highlight-lighter call with (boundp 'mode-line-modes).
;;     narrow-to-region: Explicitly provide START and END for Emacs 20, because they are nil.
;;     zz-izone-delete: Fixed free variable vAR (to VARIABLE).
;;     Bind C-x n r to zz-select-region, not zz-select-region-repeat, if < Emacs 22.
;; 2015/08/15 dadams
;;     wide-n-delete: VAR -> VARIABLE (typo, free var).
;;     wide-n-(delete|push): Fixed for 1-based, not 0-based, since removed "all" entry on 8/12.
;;     wide-n-read-any-variable: Added REQUIRE-MATCH arg.  Provide both DEFAULT-VALUE and SYMB as defaults.
;;     wide-n-(delete|push|unite|add-to-union): Provide wide-n-restrictions-var as default when reading var.
;; 2015/08/14 dadams
;;     Added: wide-n-remove-if-other-buffer-markers, wide-n-remove-if, wide-n-other-buffer-marker-p.
;;     Added: zzz-remove-if, zzz-remove-if-other-buffer-markers (in file zones.el).
;;     zzz-zone-union: Added optional arg BUFFER.  Filter with zzz-remove-if-other-buffer-markers.
;;     wide-n-select-region, wide-n: pop-to-buffer of restriction when appropriate.
;;     wide-n-push, wide-n-delete: Added args NOT-BUF-LOCAL-P, SET-VAR-P.  Changed prefix arg behavior.
;;     wide-n-add-to-union, narrow-to-(region|defun|page):
;;       Add nil args for NOT-BUF-LOCAL-P, SET-VAR-P in call to wide-n-push.
;;     wide-n-restrictions-p: Test identifier with numberp, not wide-n-number-or-marker-p.
;;     wide-n-limits: Added optional args BUFFER, ONLY-ONE-BUFFER-P. Use wide-n-remove-if-other-buffer-markers.
;;     wide-n-limits-in-bufs:
;;       Changed optional arg from RESTRICTIONS to VARIABLE (default: wide-n-restrictions-var).
;;       Corrected case when BUFFERS is nil.
;;       Pass buffer and non-nil ONLY-ONE-BUFFER-P to wide-n-limits.
;; 2015/08/13 dadams
;;     Version 2014.08.13.
;;     Added: wide-n-marker-from-object.
;;     wide-n-markerize: Convert also readable-marker objects.
;;     wide-n-restrictions-p: Us wide-n-number-or-marker-p, not number-or-marker-p (support readable markers).
;;     wide-n-push, wide-n-add-to-union, interactive spec: VARIABLE defaults to wide-n-restrictions-var value.
;;     wide-n-add-to-union: VARIABLE defaults to wide-n-restrictions-var value non-interactively too.
;; 2015/08/12 dadams
;;     wide-n-restrictions, wide-n-select-region, wide-n, wide-n-markerize, wide-n-push, wide-n-restrictions-p,
;;       wide-n-delete, wide-n-renumber, wide-n-limits, wide-n-restrictions-from-zones, wide-n-unite:
;;         INCOMPATIBLE CHANGE: wide-n-restrictions no longer has an "all" entry.
;; 2015/08/10 dadams
;;     wide-n-markerize: Corrected for format change - second marker is caddr, not cddr.
;; 2015/08/09 dadams
;;     Added: zzz-zone-complement (in file zones.el).
;; 2015/08/08 dadams
;;     Added: wide-n-unite, wide-n-add-to-union, wide-n-restrictions-from-zones.
;;     Bind wide-n-unite to C-x n u and wide-n-add-to-union to C-x n S.
;;     wide-n-push, wide-n-delete: Return new value of VARIABLE.
;;     wide-n-push: Change optional arg NOMSG to MSGP (invert the sense).
;;     Soft-require zones.el.
;;     Bind hlt-highlight-regions to C-x n h and hlt-highlight-regions-in-buffers to C-x n H.
;; 2015/08/07 dadams
;;     Added: wide-n-select-region, wide-n-select-region-repeat.
;;     Bind wide-n-select-region-repeat to C-x n r.
;;     wide-n-push, wide-n-delete: Prefix arg >= 0: make var buffer-local; <= 0: set wide-n-restrictions-var.
;; 2015/08/05 dadams
;;     Added: wide-n-restrictions-p, wide-n-restrictions-var, wide-n-read-any-variable.
;;     wide-n-restrictions (function): Now returns the value of the current wide-n-restrictions-var variable.
;;     wide-n: Use wide-n-restrictions-var, not wide-n-restrictions.
;;     wide-n-push, wide-n-delete:
;;       Added optional arg VARIABLE.  Prefix arg reads it.  Use it and maybe set wide-n-restrictions-var to it.
;;       Raise error if var is not wide-n-restrictions-p.
;;     wide-n-renumber: Added optional arg VARIABLE.
;;     wide-n-limits(-in-bufs): Added optional arg RESTRICTIONS.
;;     wide-n, wide-n-renumber, wide-n-markerize: FIX: (car (cddr...)), not cddr.
;; 2015/08/01 dadams
;;     wide-n-start+end: Fix: use list, not cons.
;; 2015/07/31 dadams
;;     Renamed: wide-n-start.end to wide-n-start+end.  Added: function wide-n-restrictions.
;;     wide-n-restrictions: INCOMPATIBLE CHANGE: The format is now (NUM START END), not (NUM START . END).
;; 2015/07/11 dadams
;;     Added: wide-n-limits, wide-n-limits-in-bufs, wide-n-start.end, wide-n-read-bufs, wide-n-remove-if-not.
;;     Made wide-n-push interactive.
;;     Bind wide-n-delete to C-x n C-d and wide-n-push to C-x n s.
;; 2014/08/12 dadams
;;     Added: wide-n-delete, wide-n-renumber.
;;     wide-n: Added optional arg MSGP.
;;     wide-n-push: Added optional arg NOMSG.
;; 2014/05/30 dadams
;;     Added: wide-n-lighter-narrow-part, wide-n-highlight-lighter, wide-n-string-match-p, wide-n-mem-regexp,
;;            wide-n-rassoc-delete-all.
;;     wide-n-restrictions: INCOMPATIBLE CHANGE: The format is now (NUM START . END), not (START . END).
;;     wide-n: Set wide-n-lighter-narrow-part.  Use wide-n-highlight-lighter.  Bind wide-n-push-anyway-p
;;             around narrow-to-region.
;;     wide-n-markerize, wide-n-push: Use new wide-n-restrictions format.
;;     wide-n-push: Added message about restriction.
;; 2011/04/09 dadams
;;     narrow-to-region defadvice:
;;       Use ad-get-arg - don't refer to args by name (work around Emacs bug #8457).
;; 2011/01/04 dadams
;;     Added autoload cookies (for commands).
;; 2010/04/26 dadams
;;     Added: wide-n-push, wide-n-push-anyway-p.
;;     narrow-to-*: Call wide-n-push when interactive or wide-n-push-anyway-p.
;; 2010/04/24 dadams
;;     Added: wide-n-markerize.
;;     Use non-destructive operations (again, as initially).
;;       wide-n-restrictions: Use (all) cons as init value.
;;       wide-n, narrow-to-region: Don't initialize to (all).
;;       wide-n: Use append, not nconc.
;;       narrow-to-region: Use remove, not delete.
;;     wide-n: Use wide-n-markerize.
;; 2010/04/21 dadams
;;     Bind non-repeatable version, wide-n, in Emacs 21.
;; 2010/04/19 dadams
;;     wide-n, narrow-to-region, wide-n-restrictions:
;;       Use nil default val & use make-local-variable, so can use destructive ops.
;;     narrow-to-region: Use delete, not remove.
;;     Zero prefix arg now widens completely and empties ring.
;;     Negative prefix arg now pops the ring.
;;     Added: standard definitions of narrow-to-(defun|page).
;;     Use narrow-map if defined.
;; 2010/04/18 dadams
;;     Created.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;; Quiet the byte-compiler.
(defvar mode-line-modes)                ; Emacs 22+
(defvar narrow-map)                     ; Emacs 23+

;;;;;;;;;;;;;;;;;;;;;;;;;;
 
(defgroup zones nil
  "Zones of text - like multiple regions."
  :prefix "zz-"
  :group 'editing
  :link `(url-link :tag "Send Bug Report"
                   ,(concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=\
zones.el bug: \
&body=Describe bug here, starting with `emacs -Q'.  \
Don't forget to mention your Emacs and library versions."))
  :link '(url-link :tag "Download" "http://www.emacswiki.org/zones.el")
  :link '(url-link :tag "Description" "http://www.emacswiki.org/Zones")
  :link '(emacs-commentary-link :tag "Commentary" "zones"))

(defvar zz-lighter-narrowing-part ""
  "String to append to \" Narrow\" in mode-line lighter or messages.")
(make-variable-buffer-local 'zz-lighter-narrowing-part)

(defvar zz-izones-var 'zz-izones
  "The izones variable currently being used.
The variable can be buffer-local or not.  If not, then its value can
include markers from multiple buffers.
See also variable `zz-izones'.")

(defvar zz-izones ()
  "List of izones.
Each entry is a list (NUM START END), where NUM is a counter
identifying this izone, and START and END are its limits.
This is the default value of variable `zz-izones-var'.")
(make-variable-buffer-local 'zz-izones)

;; Not used.  Could use this if really needed.
(defun zz-izones ()
  "Value of current `zz-izones-var' variable, in latest format.
If the value has elements of old format, (NUM START . END), it is
converted to use the new format, with elements (NUM START END).

This is a destructive operation.  The value of the variable is updated
to use the new format, and that value is returned."
  (let ((oldval  (symbol-value zz-izones-var))
        (newval  ()))
    (dolist (elt  oldval) (unless (consp (cddr elt)) (setcdr (cdr elt) (list (cddr elt)))))
    (symbol-value zz-izones-var)))

(defvar zz-add-zone-anyway-p nil
  "Non-nil means narrowing always updates current `zz-izones-var'.
Normally, if a narrowing command is called non-interactively then the
region limits are not pushed to the variable that is the current value
of `zz-izones-var'.  A non-nil value here overrides the push
inhibition.  You can bind this to non-nil in Lisp code to populate the
current `zz-izones-var' during narrowing.")

;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun zz-zone-ordered (zone)
  "Return ZONE or ZONE with its car and cadr reversed, so car <= cadr.
The cddr of ZONE remains as it was."
  (let ((beg    (car  zone))
        (end    (cadr zone))
        (extra  (cddr zone)))
    (if (<= beg end) zone `(,end ,beg ,@extra))))

(defun zz-zones-overlap-p (zone1 zone2)
  "Return non-nil if ZONE1 and  ZONE2 overlap.
Assumes that each zone is ordered (its car <= its cadr).
The cddrs are ignored.

Zones that use markers do not overlap if the marker buffers differ."
  (and (zz-zones-same-buffer-p zone1 zone2)
       (progn (when (< (car zone2) (car zone1)) (setq zone1 (prog1 zone2 (setq zone2 zone1))))
              (<= (car zone2) (cadr zone1)))))

(defun zz-zones-same-buffer-p (zone1 zone2)
  "Return non-nil if ZONE1 and ZONE2 apply to the same buffer.
This is the case if they do not contain markers or the markers are
from the same buffer."
  (let* ((car1   (car zone1))
         (cadr1  (cadr zone1))
         (mkr1   (or (and (markerp car1)   car1)  (and (markerp cadr1)  cadr1)))
         (car2   (car zone2))
         (cadr2  (cadr zone2))
         (mkr2   (or (and (markerp car2)   car2)  (and (markerp cadr2)  cadr2))))
    (or (not (and mkr1  mkr2))  (eq (marker-buffer mkr1) (marker-buffer mkr2)))))

(defun zz-zones-complement (zones &optional beg end buffer)
  "Return a list of zones that is the complement of ZONES, from BEG to END.
ZONES is assumed to be a union, i.e., sorted by car, with no overlaps.
Any extra info in a zone of ZONES, i.e., after the cadr, is ignored."
  (setq beg  (zz-marker-from-object (or beg  (point-min)))
        end  (zz-marker-from-object (or end  (point-max))))
  (let ((res  ()))
    (dolist (zone  zones)
      (push (list beg (car zone)) res)
      (setq beg  (cadr zone)))
    (setq res  (nreverse (push (list beg end) res)))))

(defun zz-two-zone-union (zone1 zone2)
  "Return the union of ZONE1 and ZONE2, or nil if they do not overlap.
Assumes that each zone is ordered (its car <= its cadr).

The cddr of a non-nil result (its EXTRA information, which must be a
list) is the union of the EXTRA information of each zone:

 (zz-set-union (cddr zone1) (cddr zone2))

This is a non-destructive operation: The result is a new list."
  (and (zz-zones-overlap-p zone1 zone2)  `(,(zz-min (car zone1)  (car zone2))
                                            ,(zz-max (cadr zone1) (cadr zone2))
                                            ,@(zz-set-union (cddr zone1) (cddr zone2)))))

(defun zz-zone-union (zones &optional buffer)
  "Return the union (coalescence) of the zones in list ZONES.
Each element of ZONES is a list of two zone limits, possibly followed
by extra info: (LIMIT1 LIMIT2 . EXTRA), where EXTRA is a list.

The limits do not need to be in numerical order.

Each limit can be a number or a marker, but zones with markers for
buffers other than BUFFER (default: current buffer) are ignored.

Returns a new list, which is sorted by the lower limit of each zone,
which is its car.  (This is a non-destructive operation.)

Each zone in ZONES is first ordered, so that its car <= its cadr.
The resulting zones are then sorted by their cars.

`zz-two-zone-union' is then applied recursively to coalesce
overlapping or adjacent zones.  This means also that any EXTRA info is
combined whenever zones are merged together."
  (let* ((filtered-zones  (zz-remove-zones-w-other-buffer-markers zones))
         (flipped-zones   (mapcar #'zz-zone-ordered filtered-zones))
         (sorted-zones    (sort flipped-zones #'zz-car-<)))
    (zz-zone-union-1 sorted-zones)))

(defun zz-zone-union-1 (zones)
  "Helper for `zz-zone-union'."
  (if (null (cdr zones))
      zones
    (let ((new  (zz-two-zone-union (car zones) (cadr zones))))
      (if new
          (zz-zone-union-1 (cons new (cddr zones)))
        (cons (car zones) (zz-zone-union-1 (cdr zones)))))))

(defun zz-car-< (zone1 zone2)
  "Return non-nil if car of ZONE1 < car of ZONE2.
Each car can be a number or a marker.

* Two numbers or two markers in the same buffer: Use `<'.
* Two markers in different buffers: Use `string<' of the buffer names.
* Only one is a marker:
  If its buffer is current then treat it as a number, using `<'.
  Else return false if the marker is for ZONE1 and return true if it
       is for ZONE2."
  (let* ((p1  (car zone1))
         (p2  (car zone2))
         (m1  (markerp p1))
         (m2  (markerp p2))
         (b1  (and m1  (marker-buffer p1)))
         (b2  (and m2  (marker-buffer p2))))
    (cond ((and (not m1)  (not m2)) (< p1 p2))
          ((and m1  m2)   (if (eq b1 b2)
                              (< p1 p2)
                            (string< (buffer-name b1) (buffer-name b2))))
          (m1             (and (eq (current-buffer) b1)  (< p1 p2)))
          (m2             (or (not (eq (current-buffer) b2))  (< p1 p2))))))

(defun zz-two-zone-intersection (zone1 zone2)
  "Return intersection of ZONE1 and ZONE2.
\(The result is nil if they do not overlap.)
Assumes that each zone is ordered (its car <= its cadr).

The cddr of a non-nil result (its EXTRA information) is
the intersection of the EXTRA information of each zone:

 (zz-set-intersection (cddr zone1) (cddr zone2))

This is a non-destructive operation: The result is a new list."
  (and (zz-zones-overlap-p zone1 zone2)  `(,(zz-max (car zone1)  (car zone2))
                                            ,(zz-min (cadr zone1) (cadr zone2))
                                            ,@(zz-set-intersection (cddr zone1) (cddr zone2)))))

(defun zz-zone-intersection (zones)
  "Return the intersection of the zones in list ZONES.
Each element of ZONES is a list of two zone limits, possibly followed
by extra info: (LIMIT1 LIMIT2 . EXTRA), where EXTRA is a list.

The limits do not need to be in numerical order.

Returns a new list, which is sorted by the lower limit of each zone,
which is its car.  (This is a non-destructive operation.)

Each zone in ZONES is first ordered, so that its car <= its cadr.
The resulting zones are then sorted by their cars.

`zz-two-zone-intersection' is then applied recursively to combine
overlapping zones.  This means also that any EXTRA info is combined
when zones are merged together."
  (let* ((flipped-zones  (mapcar #'zz-zone-ordered zones))
         (sorted-zones   (sort flipped-zones (lambda (z1 z2) (< (car z1) (car z2))))))
    (zz-zone-intersection-1 sorted-zones)))

(defun zz-zone-intersection-1 (zones)
  "Helper for `zz-zone-intersection'."
  (if (null (cdr zones))
      zones
    (let ((new  (zz-two-zone-intersection (car zones) (cadr zones))))
      (and new  (zz-zone-intersection-1 (cons new (cddr zones)))))))

;; From `cl-seq.el', function `union', without keyword treatment.
(defun zz-set-union (list1 list2)
  "Combine LIST1 and LIST2 using a set-union operation.
The result list contains all items that appear in either LIST1 or
LIST2.  This is a non-destructive function: it copies the data if
necessary."
  (cond ((null list1)         list2)
        ((null list2)         list1)
        ((equal list1 list2)  list1)
        (t
         (unless (>= (length list1) (length list2)) (setq list1  (prog1 list2 (setq list2  list1)))) ; Swap.
         (while list2
           (unless (member (car list2) list1) (setq list1  (cons (car list2) list1)))
           (setq list2  (cdr list2)))
         list1)))

;; From `cl-seq.el', function `intersection', without keyword treatment.
(defun zz-set-intersection (list1 list2)
  "Set intersection of lists LIST1 and LIST2.
This is a non-destructive operation: it copies the data if necessary."
  (and list1  list2
       (if (equal list1 list2)
           list1
         (let ((result  ()))
           (unless (>= (length list1) (length list2)) (setq list1  (prog1 list2 (setq list2  list1)))) ; Swap.
           (while list2
             (when (member (car list2) list1) (setq result  (cons (car list2) result)))
             (setq list2  (cdr list2)))
           result))))

(defun zz-min (&rest ns)
  "Like `min', but if the args include a marker then return a marker.
Raise an error if the args include markers from different buffers."
  (let ((buf  (zz-buffer-of-markers ns))
        (min  (apply #'min ns)))
    (if (not buf)
        min
      (with-current-buffer (get-buffer-create buf)
        (set-marker (copy-marker min) min buf)))))

(defun zz-max (&rest ns)
  "Like `max', but if the args include a marker then return a marker.
Raise an error if the args include markers from different buffers."
  (let ((buf  (zz-buffer-of-markers ns))
        (max  (apply #'max ns)))
    (if (not buf)
        max
      (with-current-buffer (get-buffer-create buf) (set-marker (copy-marker max) max buf)))))

(defun zz-buffer-of-markers (ns)
  "Return the buffer of the markers in list NS, or nil if no markers.
Raise an error if NS contains markers from different buffers."
  (let ((mkr  (car (zz-some #'markerp ns))))
    (and mkr
         (progn
           (unless (zz-every (lambda (nn) (or (not (markerp nn)) (eq (marker-buffer nn) (marker-buffer mkr)))) ns)
             (error "List contains markers from different buffers"))
           t)
         (marker-buffer mkr))))

;; Similar to `every' in `cl-extra.el', without non-list sequences and multiple
;; sequences.
(defun zz-every (predicate list)
  "Return t if PREDICATE is true for all elements of LIST; else nil."
  (while (and list  (funcall predicate (car list)))  (setq list  (cdr list)))
  (null list))

;; Same as `bmkp-some' in `bookmark+-1.el'.
;; This is NOT the same as `some' in `cl-extra.el', even without non-list sequences and multiple sequences.
;;
;; If PREDICATE is satisfied by a list element ELEMENT, so that it returns a non-nil value VALUE for ELEMENT,
;; then this returns the cons (ELEMENT . VALUE).  It does not return just VALUE.
(defun zz-some (predicate list)
  "Return non-nil if PREDICATE applied to some element of LIST is true.
The value returned is a cons, (ELEMENT . VALUE), where ELEMENT is the
first list element that satisfies PREDICATE and VALUE is the value of
PREDICATE applied to ELEMENT."
  (let (elt val)
    (catch 'zz-some
      (while list
        (when (setq val  (funcall predicate (setq elt  (pop list))))
          (throw 'zz-some (cons elt val))))
      nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun zz-select-region (arg &optional msgp) ; Bound to `C-x n r', for Emacs < 22.
  "Select a region from among the current set of zones.
The zones are those in the current `zz-izones-var'.
With no prefix arg, select the previous recorded zone.
With a numeric prefix arg N, select the Nth previous zone.

Note that if the value of `zz-izones-var' is not buffer-local then you
can use this command to cycle among regions in multiple buffers."
  (interactive "p\np")
  (let* ((var   zz-izones-var)
         (val   (symbol-value var))
         (cntr  (abs arg)))
    (unless (cadr val) (error "No zone to select"))
    (let ((latest  ()))
      (while (> cntr 0)
        (push (nth (1- cntr) val) latest)
        (setq cntr  (1- cntr)))
      (setq latest  (nreverse latest))
      (setq val  (set var (append (nthcdr arg val) latest))
            val  (set var (mapcar #'zz-markerize val)))
      (let* ((izone                  (car val))
             (beg                    (nth 1 izone))
             (end                    (nth 2 izone))
             (other-buf              nil))
        (when (and (not (local-variable-p var))
                   (setq other-buf  (zz-izone-has-other-buffer-marker-p izone)) ; Returns marker or nil.
                   (or (not (markerp beg))  (not (markerp end))  (eq (marker-buffer beg) (marker-buffer end)))
                   (setq other-buf  (marker-buffer other-buf)))
          (pop-to-buffer other-buf))
        (goto-char beg)
        (push-mark end nil t)
        (when msgp
          (message "Region #%d restored%s" (caar val) (if other-buf (format " in `%s'" other-buf) "")))))))

;; This is a non-destructive operation.
;;
;;;###autoload
(defun zz-narrow (arg &optional msgp) ; Bound to `C-x n x', for Emacs < 22.
  "Widen to a previous buffer restriction (narrowing).
The candidates are the zones in the current `zz-izones-var'.

With no prefix arg, widen to the previous narrowing.
With a plain prefix arg (`C-u'), widen completely.
With a zero  prefix arg (`C-0'), widen completely and reset (empty)
 the list of zones for this buffer.
With a numeric prefix arg N, widen abs(N) times (to the abs(N)th
 previous narrowing).  Positive and negative args work the same,
 except that a negative arg also pops entries off the ring: it removes
 the ring entries from the most recent back through the (-)Nth one."
  (interactive "P\np")
  (let* ((var  zz-izones-var)
         (val  (symbol-value var)))
    (unless val (error "No previous narrowing"))
    (cond ((or (consp arg)  (and (null (cdr val))
                                 (/= (- (point-max) (point-min)) (buffer-size)))) ; = `buffer-narrowed-p'.
           (widen)
           (setq zz-lighter-narrowing-part  "")
           (zz-narrowing-lighter)
           (when msgp (message "No longer narrowed")))
          ((= (prefix-numeric-value arg) 0)
           (set var ())
           (widen)
           (setq zz-lighter-narrowing-part  "")
           (zz-narrowing-lighter)
           (when msgp (message "No longer narrowed; no more narrowings")))
          (t
           (setq arg  (prefix-numeric-value arg))
           (let ((latest  ())
                 (cntr    (abs arg)))
             (while (> cntr 0)
               (push (nth (1- cntr) val) latest)
               (setq cntr  (1- cntr)))
             (setq latest  (nreverse latest))
             (when (< arg 0) (setq arg     (abs arg)
                                   latest  ()))
             (setq val                         (set var (append (nthcdr arg val) latest))
                   val                         (set var (mapcar #'zz-markerize val))
                   zz-lighter-narrowing-part  (format "-%d" (caar val)))
             (condition-case err
                 (let* ((zz-add-zone-anyway-p  t)
                        (izone                  (car val))
                        (beg                    (nth 1 izone))
                        (end                    (nth 2 izone))
                        (other-buf              nil))
                   (when (and (not (local-variable-p var))
                              (setq other-buf  (zz-izone-has-other-buffer-marker-p izone)) ; Marker or nil.
                              (or (not (markerp beg))  (not (markerp end))
                                  (eq (marker-buffer beg) (marker-buffer end))) ; Same other buffer.
                              (setq other-buf  (marker-buffer other-buf)))
                     (pop-to-buffer other-buf))
                   (narrow-to-region beg end)
                   (zz-narrowing-lighter))
               (args-out-of-range (set var  (cdr val))
                                  (error "Restriction removed because of invalid limits"))
               (error (error "%s" (error-message-string err)))))))))

(defun zz-narrowing-lighter ()
  "Update minor-mode mode-line lighter to reflect narrowing/widening.
Put `zz-narrow' on `mouse-2' for the lighter suffix.
\(Do nothing unless `mode-line-modes' is bound (Emacs 22+).)"
  (when (boundp 'mode-line-modes)
    (let* ((%n-cons  (zz-regexp-car-member "%n\\(.*\\)\\'" mode-line-modes)))
      (when %n-cons
        (setcar %n-cons (replace-regexp-in-string
                         "%n\\(.*\\)"
                         (if (/= (- (point-max) (point-min)) (buffer-size)) ; `buffer-narrowed-p', for older Emacs
                             zz-lighter-narrowing-part
                           "")
                         (car %n-cons) nil nil 1))
        (when (> (length (car %n-cons)) 2)
          (set-text-properties 2
                               (length (car %n-cons))
                               '(local-map (keymap (mode-line keymap (mouse-2 . zz-narrow)))
                                 mouse-face mode-line-highlight
                                 help-echo "mouse-2: Next Restriction")
                               (car %n-cons)))
        ;; Dunno why we need to do this.  Tried adjusting `rear-sticky' and `front-sticky',
        ;; but without this the whole field (not just the suffix) gets changed, in effect, to the above spec.
        (set-text-properties 0 2 '(local-map (keymap (mode-line keymap (mouse-2 . mode-line-widen)))
                                   mouse-face mode-line-highlight help-echo "mouse-2: Widen")
                             (car %n-cons))))))

(defun zz-regexp-car-member (regexp xs)
  "Like `member', but tests by matching REGEXP against cars."
  (and (consp xs)  (if (and (stringp (car xs))  (zz-string-match-p regexp (car xs)))
                       xs
                     (zz-regexp-car-member regexp (cdr xs)))))

;;;###autoload
(defun zz-add-zone (start end &optional variable not-buf-local-p set-var-p msgp) ; Bound to `C-x n a'.
  "Add a zone for the text from START to END to the zones of VARIABLE.
Return the new value of VARIABLE.

This is a destructive operation: The list structure of the variable
value can be modified.

VARIABLE defaults to the value of `zz-izones-var'.
START and END are as for `narrow-to-region'.

With a prefix arg you are prompted for a different variable to use, in
place of the current value of `zz-izones-var'.  The particular prefix
arg determines whether the variable, if unbound, is made buffer-local,
and whether `zz-izones-var' is set to the variable symbol:

  prefix arg          buffer-local   set `zz-izones-var'
  ----------          ------------   -------------------
   Plain `C-u'         yes            yes
   > 0 (e.g. `C-1')    yes            no
   = 0 (e.g. `C-0')    no             yes
   < 0 (e.g. `C--')    no             no

Non-interactively:
* VARIABLE is the optional izones variable to use.
* Non-nil NOT-BUF-LOCAL-P means do not make VARIABLE buffer-local.
* Non-nil SET-VAR-P means set `zz-izones-var' to VARIABLE.
* Non-nil MSGP means echo the region size."
  (interactive (let* ((beg    (region-beginning))
                      (end    (region-end))
                      (var    (or (and current-prefix-arg  (zz-read-any-variable "Variable: " zz-izones-var))
                                  zz-izones-var))
                      (npref  (prefix-numeric-value current-prefix-arg))
                      (nloc   (and current-prefix-arg  (<= npref 0)  (not (boundp var))))
                      (setv   (and current-prefix-arg  (or (consp current-prefix-arg)  (= npref 0)))))
                 (list beg end var nloc setv t)))
  (let* ((mrk1    (make-marker))
         (mrk2    (make-marker))
         (var     (or variable  zz-izones-var))
         (IGNORE  (unless (or not-buf-local-p  (boundp var)) (make-local-variable var)))
         (IGNORE  (when set-var-p (setq zz-izones-var  var)))
         (IGNORE  (unless (boundp var) (set var ())))
         (val     (symbol-value var))
         sans-id  id-cons  id)
    (unless (zz-izones-p val) (error "Not an izones variable: `%s', value: `%S'" var val))
    (move-marker mrk1 start)
    (move-marker mrk2 end)
    (setq sans-id  (list mrk1 mrk2)
          id-cons  (rassoc sans-id val)
          id       (if id-cons (car id-cons) (1+ (length val))) ; 1-based, not 0-based.
          val      (set var (zz-rassoc-delete-all sans-id val))) ; Destructive operation.
    (unless (and (= mrk1 1)  (= mrk2 (1+ (buffer-size)))) (set var `((,id ,mrk1 ,mrk2) ,@val)))
    (when msgp (message "%s region: %d to %d" (if (interactive-p) "Recorded" "Narrowed")
                        (marker-position mrk1) (marker-position mrk2)))
    (symbol-value var)))

;;;###autoload
(defun zz-delete-zone (n &optional variable not-buf-local-p set-var-p msgp) ; Bound to `C-x n C-d'.
  "Delete the zone numbered N from VARIABLE, and renumber those remaining.
Return the new value of VARIABLE.

This is a destructive operation: The list structure of the variable
value can be modified.

You are prompted for the number N.
VARIABLE defaults to the value of `zz-izones-var'.

With a prefix arg you are prompted for a different variable to use, in
place of the current value of `zz-izones-var'.  The
particular prefix arg determines whether the variable, if unbound, is
made buffer-local, and whether `zz-izones-var' is set to the
variable symbol:

  prefix arg          buffer-local   set `zz-izones-var'
  ----------          ------------   -------------------
   Plain `C-u'         yes            yes
   > 0 (e.g. `C-1')    yes            no
   = 0 (e.g. `C-0')    no             yes
   < 0 (e.g. `C--')    no             no

Non-nil optional arg NOMSG means do not display a status message."
  (interactive
   (let* ((var     (or (and current-prefix-arg  (zz-read-any-variable "Variable: " zz-izones-var))
                       zz-izones-var))
          (npref   (prefix-numeric-value current-prefix-arg))
          (nloc    (and current-prefix-arg  (<= npref 0)  (not (boundp var))))
          (setv    (and current-prefix-arg  (or (consp current-prefix-arg)  (= npref 0))))
          ;; Repeat all of the variable tests and actions, since we need to have the value, for its length.
          (IGNORE  (unless nloc (make-local-variable var)))
          (IGNORE  (when setv (setq zz-izones-var var)))
          (IGNORE  (unless (boundp var) (set var ())))
          (val     (symbol-value var))
          (IGNORE  (unless (zz-izones-p val)
                     (error "Not an izones variable: `%s', value: `%S'" var val)))
          (IGNORE  (unless val (error "No zones - variable `%s' is empty" var)))
          (len     (length val))
          (num     (if (= len 1) 1 (read-number (format "Delete zone numbered (1 to %d): " len)))))
     (while (or (< num 1)  (> num len))
       (setq num  (read-number (format "Number must be between 1 and %d: " len))))
     (list num var nloc setv t)))
  (unless variable (setq variable  zz-izones-var))
  (unless (or not-buf-local-p  (boundp variable)) (make-local-variable variable))
  (when set-var-p (setq zz-izones-var variable))
  (let ((val  (symbol-value variable)))
    (unless (zz-izones-p val) (error "Not an izones variable: `%s', value: `%S'" variable val))
    (unless val (error "No zones - variable `%s' is empty" variable))
    (set variable (assq-delete-all n val)))
  (zz-izones-renumber variable)
  (when msgp (message "Deleted zone numbered %d" n))
  (symbol-value variable))

(defun zz-markerize (izone)
  "Convert IZONE to use markers.
IZONE is a list of an identifier (a number) and two buffer
positions (numbers, markers, or readable-marker objects).  Positions
that are numbers or readable-marker objects are converted to markers.

This is a non-destructive operation: it returns a new list."
  (let ((ii   1)
        buf posn)
    (while (<  ii 3)
      (setq posn  (nth ii izone))
      (when (and (not (markerp posn))  (or (numberp posn)  (zz-readable-marker-p posn)))
        (setcar (nthcdr ii  izone) (zz-marker-from-object posn)))
      (setq ii  (1+ ii))))
  izone)

(defun zz-marker-from-object (object)
  "Return an equivalent marker for OBJECT.
This is a non-destructive operation: OBJECT is not modified.

If OBJECT is a marker then return it.
If it is a number then return (copy-marker OBJECT).
If it is a readable-marker sexp then return an equivalent real marker.
Otherwise, return nil.

A readable marker is a sexp of form (marker BUFFER POSITION), where
BUFFER is a buffer name (string) and POSITION is a buffer position
\(number)."
  (cond ((markerp object) object)
        ((numberp object) (copy-marker object))
        ((zz-readable-marker-p object)
         (with-current-buffer (get-buffer-create (nth 1 object)) (copy-marker (nth 2 object))))
        (t nil)))

(defun zz-number-or-marker-p (position)
  "Return non-nil if POSITION is a number, marker, or readable-marker object."
  (or (number-or-marker-p position)  (zz-readable-marker-p position)))

(defun zz-readable-marker-p (object)
  "Return non-nil if OBJECT is a readable marker.
That is, it has form (marker BUFFER POSITION), where BUFFER is a
buffer name (string) and POSITION is a buffer position (number).
OBJECT is returned."
  (and (consp object)  (consp (cdr object))  (consp (cddr object))
       (eq 'marker (nth 0 object))  (stringp (nth 1 object))  (numberp (nth 2 object))
       object))

(defun zz-readable-marker (number-or-marker &optional buffer)
  "Return a readable-marker object equivalent to NUMBER-OR-MARKER, or nil.
Return nil if NUMBER-OR-MARKER is not `number-or-marker-p'.

This is a non-destructive operation.

Optional arg BUFFER is a buffer or a buffer name (default: name of
current buffer).  It is used as the marker buffer when
`number-or-marker-p' is a number.

A readable-marker object is a sexp of form (marker BUFFER POSITION),
where BUFFER is a buffer name (string) and POSITION is buffer
position (number)."
  (let* ((buf   (get-buffer (or buffer  (current-buffer))))
         (buf   (and buf  (buffer-name buf)))
         (mrkr  (and (number-or-marker-p number-or-marker)
                     (if (markerp number-or-marker)
                         number-or-marker
                       (with-current-buffer buf (copy-marker number-or-marker))))))
    (and mrkr  `(marker ,buf ,(marker-position mrkr)))))

(defun zz-izones-p (value)
  "Return non-nil if VALUE is a (possibly empty) list of izones.
That is, non-nil means that VALUE has the form of `zz-izones'."
  (and (listp value)  (listp (cdr (last value))) ; Proper list.
       (let ((res  t))
         (catch 'zz-izones-p
           (dolist (nn  value)
             (unless (setq res  (and (consp nn)  (condition-case nil
                                                     (and (numberp (nth 0 nn))
                                                          (zz-number-or-marker-p (nth 1 nn))
                                                          (zz-number-or-marker-p (nth 2 nn)))
                                                   (error nil))))
               (throw 'zz-izones-p nil))))
         res)))

(defun zz-rassoc-delete-all (value alist)
  "Delete from ALIST all elements whose cdr is `equal' to VALUE.
Elements of ALIST that are not conses are ignored.
Return the modified alist.
This is a destructive operation."
  (while (and (consp (car alist))  (equal (cdar alist) value)) (setq alist  (cdr alist)))
  (let ((tail  alist)
        tail-cdr)
    (while (setq tail-cdr  (cdr tail))
      (if (and (consp (car tail-cdr))  (equal (cdar tail-cdr) value))
	  (setcdr tail (cdr tail-cdr))
	(setq tail  tail-cdr))))
  alist)


(defun zz-izones-renumber (&optional variable)
  "Renumber the izones of this buffer in the current `zz-izones-var'.
This is a destructive operation: The list structure of the variable
value can be modified."
  (let* ((var   (or variable  zz-izones-var))
         (orig  (symbol-value var)))
    (set var ())
    (dolist (nn  orig) (zz-add-zone (cadr nn) (car (cddr nn)) var))))

;;; Non-destructive version.
;;;
;;; (defun zz-izone-limits-in-bufs (buffers &optional variable)
;;;   "Return a list of all `zz-izone-limits' for each buffer in BUFFERS.
;;; That is, return a list of all recorded buffer zones for BUFFERS.
;;; If BUFFERS is nil, return the zones recorded for the current buffer.
;;;
;;; This is a non-destructive operation: The list returned is independent
;;; of the `zz-izone-limits' list in each of the buffers.
;;;
;;; Optional arg VARIABLE is the izones variable to use.  If nil,
;;; use the value of `zz-izones-var'.  The variable is evaluated in each
;;; buffer (or in the current buffer, if BUFFERS is nil)."
;;;
;;;   (let ((limits  ()))
;;;     (dolist (buf  (or (reverse buffers)  (list (current-buffer)))) ; Reverse so we keep the order.
;;;       (with-current-buffer buf
;;;         (setq limits  (append (zz-izone-limits (symbol-value (or variable  zz-izones-var))
;;;                                              buf
;;;                                              'ONLY-THIS-BUFFER)
;;;                               limits))))
;;;     limits))

(defun zz-izone-limits-in-bufs (buffers &optional variable)
  "Return a list of all `zz-izone-limits' for each buffer in BUFFERS.
That is, return a list of all recorded buffer zones for BUFFERS.
If BUFFERS is nil, return the zones recorded for the current buffer.

This is a destructive operation: The list returned can have as
sublists the `zz-izone-limits' lists of BUFFERS.

Optional arg VARIABLE is the izones variable to use.  If nil, use the
value of `zz-izones-var'.  The variable is evaluated in each
buffer (or in the current buffer, if BUFFERS is nil)."
  (let ((limits  ()))
    (dolist (buf  (or buffers  (list (current-buffer))))
      (with-current-buffer buf
        (setq limits  (nconc limits
                             (zz-izone-limits (symbol-value (or variable  zz-izones-var)) buf 'THISBUF)))))
    limits))

(defun zz-izone-limits (&optional izones buffer only-one-buffer-p)
  "Return a list like IZONES, but with no identifiers.
That is, return a list of zones, (LIMIT1 LIMIT2).

This is a non-destructive operation: A new list is returned.

Each limit can be a number or a marker (but see ONLY-ONE-BUFFER-P).
The conses are new - they do not share with any conses with IZONES.

Optional input list IZONES has the same structure as `zz-izones'.  If
IZONES is nil then the variable that is the value of `zz-izones-var'
is used.  It is evaluated in BUFFER (default: current buffer) to
obtain the izones.

Non-nil optional arg ONLY-ONE-BUFFER-P means remove any izones that
contain markers for a buffer other than BUFFER."
  (unless buffer (setq buffer  (current-buffer)))
  (let ((restrs  (or izones  (with-current-buffer buffer (symbol-value zz-izones-var)))))
    (when only-one-buffer-p (setq restrs  (zz-remove-izones-w-other-buffer-markers restrs)))
    (delq nil (mapcar #'cdr restrs))))

;; Useful for commands that want to act on regions in multiple buffers.
(defun zz-read-bufs ()
  "Read names of buffers, one at a time.  `C-g' ends reading."
  (let ((bufs  ())
        buf)
    (while (condition-case nil
               (setq buf  (read-buffer "Buffer (C-g to end): "
                                       (and (not (member (buffer-name (current-buffer)) bufs))
                                            (current-buffer))
                                       t))
             (quit nil))
      (push buf bufs))
    (delq nil (mapcar #'get-buffer (nreverse bufs)))))

(defun zz-remove-zones-w-other-buffer-markers (zones &optional buffer)
  "Return ZONES, but remove any that use markers for another buffer.
BUFFER is the buffer to compare with (default: current buffer).
This is a non-destructive operation: a (shallow) copy is returned."
  (unless buffer (setq buffer  (current-buffer)))
  (zz-remove-if `(lambda (zone) (zz-zone-has-other-buffer-marker-p zone ',buffer)) zones))

(defun zz-remove-izones-w-other-buffer-markers (izones &optional buffer)
  "Return IZONES, but remove any that use markers for another buffer.
BUFFER is the buffer to compare with (default: current buffer).
This is a non-destructive operation: a (shallow) copy is returned."
  (unless buffer (setq buffer  (current-buffer)))
  (zz-remove-if `(lambda (izone) (zz-izone-has-other-buffer-marker-p izone ',buffer)) izones))

(defun zz-zone-has-other-buffer-marker-p (zone &optional buffer)
  "Return non-nil if basic ZONE has a marker for another buffer.
The first marker in the zone is returned.
BUFFER is the buffer to compare with (default: current buffer)."
  (unless buffer (setq buffer  (current-buffer)))
  (let ((m1  (nth 0 zone))
        (m2  (nth 1 zone)))
    (or (and (markerp m1)  (not (eq buffer (marker-buffer m1)))  m1)
        (and (markerp m2)  (not (eq buffer (marker-buffer m2)))  m2))))

(defun zz-izone-has-other-buffer-marker-p (izone &optional buffer)
  "Return non-nil if IZONE has a marker for another buffer.
The first marker in the izone is returned.
BUFFER is the buffer to compare with (default: current buffer)."
  (unless buffer (setq buffer  (current-buffer)))
  (let ((m1  (nth 1 izone))
        (m2  (nth 2 izone)))
    (or (and (markerp m1)  (not (eq buffer (marker-buffer m1)))  m1)
        (and (markerp m2)  (not (eq buffer (marker-buffer m2)))  m2))))

(defun zz-remove-if (pred xs)
  "A copy of list XS with no elements that satisfy predicate PRED."
  (let ((result  ()))
    (dolist (x xs) (unless (funcall pred x) (push x result)))
    (nreverse result)))

;; Useful for commands that want to act on  regions in multiple buffers (e.g., visible buffers only).
;;
;; Same as `icicle-remove-if-not' etc.
(defun zz-remove-if-not (pred xs)
  "A copy of list XS with only elements that satisfy predicate PRED."
  (let ((result  ()))
    (dolist (x xs) (when (funcall pred x) (push x result)))
    (nreverse result)))

;; Like `read-any-variable' in `strings.el', but passes REQUIRE-MATCH arg to `completing-read'.
(defun zz-read-any-variable (prompt &optional default-value require-match)
  "Read the name of a variable and return it as a symbol.
Prompts with string PROMPT.  By default, returns DEFAULT-VALUE if
non-nil.  If DEFAULT-VALUE is nil and the nearest symbol to the cursor
is a variable, then return that by default.

Unlike `read-variable', which reads only user options, this reads the
name of any variable.  If optional arg REQUIRE-MATCH is nil then it
reads any symbol, but it provides completion against variable names."
  (let ((symb                          (cond ((fboundp 'symbol-nearest-point) (symbol-nearest-point))
                                             ((fboundp 'symbol-at-point) (symbol-at-point))
                                             (t nil)))
        (enable-recursive-minibuffers  t))
    (when (and default-value  (symbolp default-value))
      (setq default-value  (symbol-name default-value)))
    (intern (completing-read prompt obarray 'boundp require-match nil 'minibuffer-history
                             (let ((var-at-pt  (and symb  (boundp symb)  (symbol-name symb))))
                               (if (and default-value  var-at-pt  (> emacs-major-version 22))
                                   (list default-value var-at-pt)
                                 (or default-value  var-at-pt)))
                             t))))

;; Same as `tap-string-match-p' in `thingatpt+.el' and `icicle-string-match-p' in `icicles-fn.el'.
(if (fboundp 'string-match-p)
    (defalias 'zz-string-match-p 'string-match-p) ; Emacs 23+
  (defun zz-string-match-p (regexp string &optional start)
    "Like `string-match', but this saves and restores the match data."
    (save-match-data (string-match regexp string start))))

(defun zz-repeat-command (command)
  "Repeat COMMAND."
 (let ((repeat-previous-repeated-command  command)
       (repeat-message-function           'ignore)
       (last-repeatable-command           'repeat))
   (repeat nil)))

;;;###autoload
(defun zz-narrow-repeat (arg)              ; Bound to `C-x n x'.
  "Cycle to the next buffer restriction (narrowing).
This is a repeatable version of `zz-narrow'.

Note that if the value of `zz-izones-var' is not buffer-local then you
can use this command to cycle among regions in multiple buffers."
  (interactive "P")
  (require 'repeat)
  (zz-repeat-command 'zz-narrow))

;;;###autoload
(defun zz-select-region-repeat (arg) ; Bound to `C-x n r'.
  "Cycle to the next region.
This is a repeatable version of `zz-select-region'."
  (interactive "P")
  (require 'repeat)
  (zz-repeat-command 'zz-select-region))

(defun zz-izones-from-zones (basic-zones)
  "Return a list of regions like `zz-izones', based on BASIC-ZONES.
Each zone in the list BASIC-ZONES has form (LIMIT1 LIMIT2 . EXTRA),
where each of the limits is a buffer position (a number or marker) and
EXTRA is a list.

This is a non-destructive operation.  A new list is returned.

\(zz-izones-from-zones (zz-izone-limits)) = zz-izones
and
\(zz-izone-limits (zz-izones-from-zones BASIC-ZONES)) = BASIC-ZONES"
  (let ((ii  0))
    (nreverse (mapcar (lambda (zz) (cons (setq ii  (1+ ii)) zz)) basic-zones))))

;;;###autoload
(defun zz-set-izones-var (variable &optional localp) ; Bound to `C-x n v'
  "Set `zz-izones-var' to VARIABLE, for which you are prompted.
With a prefix arg, make VARIABLE automatically be buffer-local."
  (interactive (list (zz-read-any-variable "Variable: " zz-izones-var) current-prefix-arg))
  (setq zz-izones-var  variable)
  (when localp (make-variable-buffer-local variable)))

;;;###autoload
(defun zz-clone-zones (from-variable to-variable &optional msgp) ; Bound to `C-x n c'
  "Clone FROM-VARIABLE to TO-VARIABLE.
That is, copy the zones of FROM-VARIABLE to (emptied) TO-VARIABLE.
A non-destructive operation: The value of TO-VARIABLE is a new list,
 with only the zones from FROM-VARIABLE.
Return the value of TO-VARIABLE.

You are prompted for FROM-VARIABLE and TO-VARIABLE.

With a non-negative (>= 0) prefix arg, make TO-VARIABLE buffer-local.
With a non-positive (<= 0) prefix arg, set `zz-izones-var' to the
TO-VARIABLE symbol.  (Zero: do both.)

FROM-VARIABLE defaults to the value of `zz-izones-var'.

Non-interactively: Non-nil MSGP means show a status message."
  (interactive
   (let ((from-var  (zz-read-any-variable "Copy variable: " zz-izones-var))
         (to-var    (zz-read-any-variable "To variable: "))
         (npref     (and current-prefix-arg  (prefix-numeric-value current-prefix-arg))))
     (when (and npref  (>= npref 0)) (make-local-variable to-var))
     (when (and npref  (<= npref 0)) (setq zz-izones-var to-var))
     (list from-var to-var t)))
  (set to-variable (copy-sequence (symbol-value from-variable)))
  (when msgp (message "Cloned `%s' to `%s'" from-variable to-variable)))

;;;###autoload
(defalias 'zz-clone-and-coalesce-zones 'zz-clone-and-unite-zones)
;;;###autoload
(defun zz-clone-and-unite-zones (from-variable to-variable &optional msgp) ; Bound to `C-x n C'
  "Clone FROM-VARIABLE to TO-VARIABLE, then unite (coalesce) TO-VARIABLE.
That is, use`zz-clone-zones' to fill TO-VARIABLE, then use
`zz-unite-zones' on TO-VARIABLE.

Use this when you do not want to unite the zones of FROM-VARIABLE (for
example, you want to use them as possibly overlapping buffer
narrowings), but you also want to act on the united zones (for
example, to search them).

FROM-VARIABLE defaults to the value of `zz-izones-var'.

Non-interactively: Non-nil MSGP means show a status message."
  (interactive
   (let ((from-var  (zz-read-any-variable "Copy variable: " zz-izones-var))
         (to-var    (zz-read-any-variable "To variable: "))
         (npref     (and current-prefix-arg  (prefix-numeric-value current-prefix-arg))))
     (when (and npref  (>= npref 0)) (make-local-variable to-var))
     (when (and npref  (<= npref 0)) (setq zz-izones-var to-var))
     (list from-var to-var t)))
  (set to-variable (copy-sequence (symbol-value from-variable)))
  (zz-unite-zones to-variable)
  (when msgp (message "Cloned `%s' to `%s' and united `%s'" from-variable to-variable to-variable)))

;;;###autoload
(defalias 'zz-coalesce-zones 'zz-unite-zones)
;;;###autoload
(defun zz-unite-zones (&optional variable msgp) ; Bound to `C-x n u'
  "Coalesce (unite) the izones of VARIABLE.
A non-destructive operation: The new value of VARIABLE is a new list.
Return the new value of VARIABLE.

VARIABLE defaults to the value of `zz-izones-var'.
With a prefix arg you are prompted for a different variable to use, in
place of the current value of `zz-izones-var'.  If the prefix arg is
non-negative (>= 0) then make the variable buffer-local.  If the
prefix arg is non-positive (<= 0) then set `zz-izones-var' to that
variable symbol.  (Zero: do both.)

Non-interactively:
* VARIABLE is the optional izones variable to use.
* Non-nil MSGP means show a status message."
  (interactive (let* ((var    (and current-prefix-arg  (zz-read-any-variable "Variable: " zz-izones-var)))
                      (npref  (prefix-numeric-value current-prefix-arg)))
                 (when (and current-prefix-arg  (>= npref 0)) (make-local-variable var))
                 (when (and current-prefix-arg  (<= npref 0)) (setq zz-izones-var var))
                 (list var t)))
  (let* ((var         (or variable  zz-izones-var))
         (IGNORE      (unless (boundp var) (set var ())))
         (val         (symbol-value var))
         (IGNORE      (unless (zz-izones-p val) (error "Not an izones variable: `%s', value: `%S'" var val)))
         (zone-union  (zz-zone-union (zz-izone-limits val))))
    (set var  (zz-izones-from-zones zone-union))
    (when msgp (message "Restrictions united for `%s'" var))
    (symbol-value var)))

;;;###autoload
(defalias 'zz-add-zone-and-coalesce 'zz-add-zone-and-unite)
;;;###autoload
(defun zz-add-zone-and-unite (start end &optional variable msgp) ; Bound to `C-x n A'.
  "Add an izone from START to END to those of VARIABLE, and coalesce.
Use `zz-add-zone', then apply `zz-unite-zones'.
Return the new value of VARIABLE.

This is a destructive operation: The list structure of the variable
value can be modified.

VARIABLE defaults to the value of `zz-izones-var'.
START and END are as for `narrow-to-region'.

With a prefix arg you are prompted for a different variable to use, in
place of the current value of `zz-izones-var'.  If the prefix arg is
non-negative (>= 0) then make the variable buffer-local.  If the
prefix arg is non-positive (<= 0) then set `zz-izones-var' to that
variable symbol.  (Zero: do both.)

Non-interactively:
* VARIABLE is the optional izones variable to use.
* Non-nil MSGP means echo the size of the added zone."
  (interactive (let ((beg    (region-beginning))
                     (end    (region-end))
                     (var    (or (and current-prefix-arg  (zz-read-any-variable "Variable: " zz-izones-var))
                                 zz-izones-var))
                     (npref  (prefix-numeric-value current-prefix-arg)))
                 (when (and current-prefix-arg  (>= npref 0)) (make-local-variable var))
                 (when (and current-prefix-arg  (<= npref 0)) (setq zz-izones-var var))
                 (list beg end var t)))
  (unless variable (setq variable  zz-izones-var))
  (zz-add-zone start end variable nil nil msgp)
  (zz-unite-zones variable msgp)
  (symbol-value variable))


;;---------------------

(cond ((boundp 'narrow-map)
       (define-key narrow-map "a"    'zz-add-zone)
       (define-key narrow-map "A"    'zz-add-zone-and-unite)
       (define-key narrow-map "c"    'zz-clone-zones)
       (define-key narrow-map "C"    'zz-clone-and-unite-zones)
       (define-key narrow-map "\C-d" 'zz-delete-zone)
       (when (fboundp 'hlt-highlight-regions)
         (define-key narrow-map "h"  'hlt-highlight-regions))
       (when (fboundp 'hlt-highlight-regions)
         (define-key narrow-map "H"  'hlt-highlight-regions-in-buffers))
       (define-key narrow-map "r"    (if (> emacs-major-version 21) 'zz-select-region-repeat 'zz-select-region))
       (define-key narrow-map "u"    'zz-unite-zones)
       (define-key narrow-map "v"    'zz-set-izones-var)
       (define-key narrow-map "x"    'zz-narrow-repeat))
      (t
       (define-key ctl-x-map "na"    'zz-add-zone)
       (define-key ctl-x-map "nA"    'zz-add-zone-and-unite)
       (define-key ctl-x-map "nc"    'zz-clone-zones)
       (define-key ctl-x-map "nC"    'zz-clone-and-unite-zones)
       (define-key ctl-x-map "n\C-d" 'zz-delete-zone)
       (when (fboundp 'hlt-highlight-regions)
         (define-key ctl-x-map "nh"  'hlt-highlight-regions))
       (when (fboundp 'hlt-highlight-regions)
         (define-key ctl-x-map "nH"  'hlt-highlight-regions-in-buffers))
       (define-key ctl-x-map "nr"    (if (> emacs-major-version 21) 'zz-select-region-repeat 'zz-select-region))
       (define-key ctl-x-map "nu"    'zz-unite-zones)
       (define-key ctl-x-map "nv"    'zz-set-izones-var)
       (define-key ctl-x-map "nx"    (if (> emacs-major-version 21) 'zz-narrow-repeat 'zz-narrow))))


;; Call `zz-add-zone' if interactive or if `zz-add-zone-anyway-p'.
;;
(defadvice narrow-to-region (before zz-add-zone activate)
  "Push the region limits to the current `zz-izones-var'.
You can use `C-x n x' to widen to a previous buffer restriction.

This is a destructive operation. The list structure of the variable
value can be modified."
  (when (or (interactive-p)  zz-add-zone-anyway-p)
    (let ((start  (ad-get-arg 0))
          (end    (ad-get-arg 1)))
      (unless start (setq start  (region-beginning))) ; Needed for Emacs 20.
      (unless end   (setq end    (region-end)))
      (zz-add-zone start end nil nil nil 'MSG))))


;; REPLACE ORIGINAL in `lisp.el'.
;;
;; Call `zz-add-zone' if interactive or `zz-add-zone-anyway-p'.
;;
;;;###autoload
(defun narrow-to-defun (&optional arg)
  "Make text outside current defun invisible.
The visible defun is the one that contains point or follows point.
Optional ARG is ignored.

This is a destructive operation. The list structure of the variable
that is the value of `zz-izones-var' can be modified."
  (interactive)
  (save-excursion
    (widen)
    (let ((opoint  (point))
	  beg end)
      ;; Try first in this order for the sake of languages with nested functions
      ;; where several can end at the same place as with the offside rule, e.g. Python.
      (beginning-of-defun)
      (setq beg  (point))
      (end-of-defun)
      (setq end  (point))
      (while (looking-at "^\n")
	(forward-line 1))
      (unless (> (point) opoint)
	;; `beginning-of-defun' moved back one defun, so we got the wrong one.
	(goto-char opoint)
	(end-of-defun)
	(setq end  (point))
	(beginning-of-defun)
	(setq beg  (point)))
      (goto-char end)
      (re-search-backward "^\n" (- (point) 1) t)
      (when (or (interactive-p)  zz-add-zone-anyway-p) (zz-add-zone beg end nil nil nil 'MSG))
      (narrow-to-region beg end))))


;; REPLACE ORIGINAL in `page.el'.
;;
;; Call `zz-add-zone' if interactive or `zz-add-zone-anyway-p'.
;;
;;;###autoload
(defun narrow-to-page (&optional arg)
  "Make text outside current page invisible.
A numeric arg specifies to move forward or backward by that many pages,
thus showing a page other than the one point was originally in.

This is a destructive operation. The list structure of the variable
that is the value of `zz-izones-var' can be modified."
  (interactive "P")
  (setq arg  (if arg (prefix-numeric-value arg) 0))
  (save-excursion
    (widen)
    (if (> arg 0)
	(forward-page arg)
      (if (< arg 0)
	  (let ((adjust  0)
		(opoint  (point)))
	    ;; If not now at the beginning of a page, move back one extra time, to get to start of this page.
	    (save-excursion
	      (beginning-of-line)
	      (or (and (looking-at page-delimiter)  (eq (match-end 0) opoint))
		  (setq adjust 1)))
	    (forward-page (- arg adjust)))))
    ;; Find the end of the page.
    (set-match-data nil)
    (forward-page)
    ;; If we stopped due to end of buffer, stay there.
    ;; If we stopped after a page delimiter, put end of restriction at the beginning of that line.
    ;; Before checking the match that was found, verify that `forward-page' actually set the match data.
    (if (and (match-beginning 0)  (save-excursion (goto-char (match-beginning 0))
                                                  (looking-at page-delimiter)))
	(goto-char (match-beginning 0)))
    (let ((beg  (point))
          (end  (progn
                  ;; Find the top of the page.
                  (forward-page -1)
                  ;; If we found beginning of buffer, stay there.
                  ;; If extra text follows page delimiter on same line, include it.
                  ;; Otherwise, show text starting with following line.
                  (when (and (eolp)  (not (bobp))) (forward-line 1))
                  (point))))
      (when (or (interactive-p)  zz-add-zone-anyway-p) (zz-add-zone beg end nil nil nil 'MSG))
      (narrow-to-region beg end))))

;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'zones)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; zones.el ends here
