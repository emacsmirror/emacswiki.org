;;; zones.el --- Zones of text - like multiple regions
;;
;; Copyright (C) 2010-2018  Free Software Foundation, Inc.
;;
;; Filename: zones.el
;; Description:  Zones of text - like multiple regions
;; Author: Drew Adams
;; Maintainer: Drew Adams <drew.adams@oracle.com>
;; Created: Sun Apr 18 12:58:07 2010 (-0700)
;; Version: 2018.11.18
;; Package-Requires: ()
;; Last-Updated: Sun Nov 18 15:03:48 2018 (-0800)
;;           By: dradams
;;     Update #: 2333
;; URL: https://elpa.gnu.org/packages/zones.html
;; URL: https://www.emacswiki.org/emacs/download/zones.el
;; Doc URL: https://www.emacswiki.org/emacs/Zones
;; Doc URL: https://www.emacswiki.org/emacs/MultipleNarrowings
;; Keywords: narrow restriction widen region zone
;; Compatibility: GNU Emacs 22.x, 23.x, 24.x, 25.x, 26.x
;;
;; Features that might be required by this library:
;;
;;   `backquote', `bytecomp', `cconv', `cl', `cl-lib', `gv',
;;   `macroexp'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;     Zones of text - like multiple regions.
;;
;;    Bug reports etc.: (concat "drew" ".adams" "@" "oracle" ".com")
;;
;;    You can get `zones.el' from Emacs Wiki or GNU ELPA:
;;
;;    * Emacs Wiki: https://www.emacswiki.org/emacs/download/zones.el
;;    * GNU ELPA:   https://elpa.gnu.org/packages/zones.html
;;
;;    The instance on Emacs Wiki might sometimes be more recent, but
;;    major changes (named ''versions'') are posted to GNU ELPA.
;;
;;    More description below.
 
;;(@> "Index")
;;
;;  Index
;;  -----
;;
;;  If you have library `linkd.el', load `linkd.el' and turn on
;;  `linkd-mode' now.  It lets you easily navigate around the sections
;;  of this doc.  Linkd mode will highlight this Index, as well as the
;;  cross-references and section headings throughout this file.  You
;;  can get `linkd.el' here:
;;  https://www.emacswiki.org/emacs/download/linkd.el.
;;
;;  (@> "Things Defined Here")
;;  (@> "Documentation")
;;    (@> "Compatibility")
;;    (@> "Zones")
;;    (@> "Coalesced (United) Zones")
;;    (@> "Noncontiguous Region and Set of Zones")
;;    (@> "Zones and Overlays")
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
;;    `zz-add-zone-and-unite', `zz-add-zones-from-highlighting',
;;    `zz-clone-and-coalesce-zones', `zz-clone-and-unite-zones',
;;    `zz-clone-zones', `zz-coalesce-zones', `zz-create-face-zones',
;;    `zz-delete-zone', `zz-narrow', `zz-narrow-repeat',
;;    `zz-query-replace-zones' (Emacs 25+),
;;    `zz-query-replace-regexp-zones' (Emacs 25+), `zz-select-region',
;;    `zz-select-region-repeat', `zz-set-izones-var',
;;    `zz-set-zones-from-highlighting', `zz-unite-zones'.
;;
;;  User options defined here:
;;
;;    `zz-narrowing-use-fringe-flag'.
;;
;;  Faces defined here:
;;
;;    `zz-fringe-for-narrowing'.
;;
;;  Non-interactive functions defined here:
;;
;;    `zz-add-key-bindings-to-narrow-map', `zz-buffer-narrowed-p'
;;    (Emacs 22-23), `zz-buffer-of-markers', `zz-car-<',
;;    `zz-do-izones', `zz-do-zones', `zz-dot-pairs', `zz-every',
;;    `zz-izone-has-other-buffer-marker-p', `zz-izone-limits',
;;    `zz-izone-limits-in-bufs', `zz-izones',
;;    `zz-izones-from-noncontiguous-region' (Emacs 25+),
;;    `zz-izones-from-zones', `zz-izones-p', `zz-izones-renumber',
;;    `zz-map-izones', `zz-map-zones', `zz-marker-from-object',
;;    `zz-markerize', `zz-max', `zz-min', `zz-narrow-advice',
;;    `zz-narrowing-lighter', `zz-noncontiguous-region-from-izones',
;;    `zz-noncontiguous-region-from-zones', `zz-number-or-marker-p',
;;    `zz-overlays-to-zones', `zz-overlay-to-zone',
;;    `zz-overlay-union', `zz-rassoc-delete-all',
;;    `zz-readable-marker', `zz-readable-marker-p',
;;    `zz-read-any-variable', `zz-read-bufs', `zz-regexp-car-member',
;;    `zz-remove-if', `zz-remove-if-not',
;;    `zz-remove-izones-w-other-buffer-markers',
;;    `zz-remove-zones-w-other-buffer-markers', `zz-repeat-command',
;;    `zz-set-intersection', `zz-set-union', `zz-some',
;;    `zz-string-match-p', `zz-two-zone-intersection',
;;    `zz-two-zone-union', `zz-zone-buffer-name',
;;    `zz-zone-has-other-buffer-marker-p', `zz-zone-intersection',
;;    `zz-zone-intersection-1', `zz-zone-ordered',
;;    `zz-zones-complement', `zz-zones-from-noncontiguous-region'
;;    (Emacs 25+), `zz-zones-overlap-p',
;;    `zz-zones-same-buffer-name-p', `zz-zones-to-overlays',
;;    `zz-zone-to-overlay', `zz-zone-union', `zz-zone-union-1'.
;;
;;  Internal variables defined here:
;;
;;    `zz--fringe-remapping', `zz-izones', `zz-izones-var', `zz-lighter-narrowing-part',
;;    `zz-add-zone-anyway-p'.
;;
;;  Macros defined here:
;;
;;    `zz-user-error'.
;;
;;
;;  ***** NOTE: These EMACS PRIMITIVES have been ADVISED HERE:
;;
;;    `narrow-to-defun', `narrow-to-page', `narrow-to-region'.
 
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
;;  prior to Emacs 23.  Still others are available only starting with
;;  Emacs 25.  This is mentioned where applicable.
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
;;  The content of a zone is any contiguous stretch of buffer text.
;;  The positions of a zone can be in either numeric order.  The
;;  positions are also called the zone "limits".  The lower limit is
;;  called the zone "beginning"; the upper limit is called its "end".
;;
;;
;;(@* "Coalesced (United) Zones")
;;  ** Coalesced (United) Zones **
;;
;;  A list of zones can include zones that overlap or are adjacent
;;  (the end of one is one less than the beginning of the other).
;;
;;  Basic-zone union and intersection operations (`zz-zone-union',
;;  `zz-zone-intersection') each act on a list of zones, returning
;;  another such list, but with the recorded positions for each zone
;;  in (ascending) buffer order, and with the zones in ascending order
;;  of their cars.  For basic-zone union, the resulting zones are said
;;  to be "coalesced", or "united".
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
;;  * The zones in the result list are disjoint: they are not adjacent
;;    and do not overlap: there is some other buffer text (i.e., not
;;    in any zone) between any two zones in the result.
;;
;;
;;(@* "Noncontiguous Region and Set of Zones")
;;  ** Noncontiguous Region and Set of Zones **
;;
;;  Starting with Emacs 25, Emacs can sometimes use a region that is
;;  made up of noncontiguous pieces of buffer content: a
;;  "noncontiguous region".  This is similar to a set of zones, but
;;  there are some differences.
;;
;;  The zones in a set (or list) of zones can be adjacent or overlap,
;;  and their order in the set is typically not important.
;;
;;  A noncontiguous region corresponds instead to what results from
;;  coalescing (uniting) a set of zones: a sequence of disjoint zones,
;;  in buffer order, that is, ordered by their cars.
;;
;;  The Lisp representation of a zone also differs from that of a
;;  segment of a noncontiguous region.  Each records two buffer
;;  positions, but a zone can also include a list of additional
;;  information (whatever you like).
;;
;;  A noncontiguous-region segment is a cons (BEGIN . END), with BEGIN
;;  <= END.  A zone is a list (LIMIT1 LIMIT2 . EXTRA) of two positions
;;  optionally followed by a list of extra stuff (any Lisp objects).
;;  And as stated above, the zone limits need not be in ascending
;;  order.
;;
;;  The last difference is that each buffer position of a zone can be
;;  a marker, which means that a list of zones can specify zones in
;;  different buffers.  A zone position can also be a readable marker,
;;  which is a Lisp sexp that can be written to disk (e.g., as part of
;;  a bookmark or saved variable), and restored in a later Emacs
;;  session by reading the file where it is saved.
;;
;;
;;(@* "Zones and Overlays")
;;  ** Zones and Overlays **
;;
;;  Zones have even more in common with Emacs overlays than they do
;;  with segments of a noncontiguous region.  An overlay has an
;;  associated buffer, two limits (start and end), and an optional
;;  list of properties.
;;
;;  Zones differ from overlays in these ways:
;;
;;  * A zone can have an identifier (izone).
;;  * A zone can have a readable Lisp form, by using numbers or
;;    readable markers.
;;  * A zone need not be specific to a particular buffer.  If a zone's
;;    positions are numbers instead of markers then you can use it in
;;    any buffer.
;;  * A set of zones can be persistent, by bookmarking it.
;;
;;  You can create zones from overlays, and vice versa, using
;;  functions `zz-overlay-to-zone', `zz-zone-to-overlay',
;;  `zz-overlays-to-zones', and `zz-zones-to-overlays'.
;;
;;  When creating zones from overlays you can specify how to represent
;;  the zone limits: using markers, readable markers, or positive
;;  integers.  And you can specify whether to create basic zones or
;;  izones.  The overlay property list becomes the list of EXTRA
;;  information of the resulting zone: (LIMIT1 LIMIT2 . EXTRA).
;;
;;  When creating overlays from zones, any list of EXTRA zone
;;  information is used as the property list of the resulting overlay.
;;  When creating a single such overlay you can optionally specify
;;  additional overlay properties, as well as arguments FRONT-ADVANCE
;;  and REAR-ADVANCE for function `make-overlay'.
;;
;;  You can use function `zz-overlay-union' to coalesce overlays in a
;;  given buffer that overlap or are adjacent.
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
;;    sorting them in ascending order of their cars).
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
;;  * Add zones of highlighted text (overlay or text-property
;;    highlighting).  For this you need library `highlight.el'.
;;    (You can highlight many ways, including dragging the mouse.)
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
;;  * Query-replace over them (Emacs 25 and later).
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
;;  Many of the commands that manipulate izones are bound on keymap
;;  `narrow-map'.  So they are available on prefix key `C-x n' (by
;;  default), along with the narrowing/widening keys `C-x n d', `C-x n
;;  n', `C-x n p', and `C-x n w'.  (If you use Emacs 22 then there is
;;  no `narrow-map', so the same `n ...' keys are bound on keymap
;;  `ctl-x-map'.)
;;
;;  If you have already bound one of these keys then `zones.el' does
;;  not rebind that key; your bindings are respected.
;;
;;  C-x n a   `zz-add-zone' - Add to current izones variable
;;  C-x n A   `zz-add-zone-and-unite' - Add izone, then unite izones
;;  C-x n c   `zz-clone-zones' - Clone zones from one var to another
;;  C-x n C   `zz-clone-and-unite-zones' - Clone then unite zones
;;  C-x n d   `narrow-to-defun'
;;  C-x n C-d `zz-delete-zone' - Delete an izone from current var
;;  C-x n h   `hlt-highlight-regions' - Highlight izones
;;  C-x n H   `hlt-highlight-regions-in-buffers' - in multiple buffers
;;  C-x n l   `zz-add-zones-from-highlighting' - Add highlighted areas
;;  C-x n L   `zz-set-zones-from-highlighting' - Set to highlighted
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
;;  If option `zz-narrowing-use-fringe-flag' is non-nil, which it is
;;  by default, then the face of the selected frame's fringe is set to
;;  `zz-fringe-for-narrowing' whenever the buffer is narrowed.  This
;;  shows you that the current buffer is narrowed even if the
;;  mode-line does not.
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
;; 2018/11/18 dadams
;;     Suggestions from Stefan Monnier:
;;       Added: zz--fringe-remapping, zz-add-key-bindings-to-narrow-map.
;;       zz-set-fringe-for-narrowing: Remap fringe face (relative) instead of (re)setting it.
;;       zz-add-zone: Use string, not Boolean MSG arg.
;;       Require repeat.el in zz-repeat-command, not in zz-*-repeat.
;;       zz-add-zone-and-unite: MSGP -> MSG (string, not Boolean).
;;       Advice for narrow-*: Use zz-narrow-advice.
;; 2018/11/13 dadams
;;     Added: zz-do-izones, zz-do-zones, zz-map-izones, zz-map-zones.
;; 2018/11/12 dadams
;;     Added: zz-create-face-zones.
;;     zz-zone-union-1: Replaced recursive version with iterative version.
;;     zz-unite-zones: Better message: give number of resulting zones.
;;     zz-(add|set)-zones-from-highlighting: Added autoload cookie.
;; 2018/10/31 dadams
;;     Do not overwrite any user key bindings on narrow-map or ctl-x-map.
;;     Simplified defadvice.
;; 2018/10/30 dadams
;;     Forked Emacs 20-21 stuff off as zones20.el.
;;       Require cl-lib.el for Emacs 23+, cl.el for Emacs 22.
;;       Added: zz-buffer-narrowed-p (for Emacs 22-23).
;;       narrow-to-(defun|page): Use defadvice instead of redefining.
;;       narrow-to-defun: Updated to Emacs 26 definition.
;; 2018/10/28 dadams
;;     Added: zz-set-zones-from-highlighting.
;;     zz-add-zones-from-highlighting: Prefix arg >=0: prompt for the face, <= 0: use font-lock-face.
;;     Bind in eval-after-load of highlight.el: zz-(add|set)-zones-from-highlighting (to C-x n [lL]),
;;                                              hlt-highlight-regions(-in-buffers).
;; 2018/10/14 dadams
;;     Added (missing) macro zz-user-error.
;;     Fixed (redefined) zz-noncontiguous-region-from-(i)zones.  Added optional arg to zz-*-from-izones.
;;     zz-clone-zones, zz-clone-and-unite-zones: Return new value of TO-VARIABLE.
;;     zz-add-zones-from-highlighting: Corrected error msg.
;;     zz-query-replace(-regexp)-zones, zz-map-query-replace-regexp-zones, zz-replace-(string|regexp)-zones:
;;       Use zz-query-replace-regexp-zones.
;; 2018/09/18 dadams
;;     Added: zz-add-zones-from-highlighting.
;; 2018/05/13 dadams
;;     Added: zz-overlays-to-zones, zz-overlay-to-zone, zz-zones-to-overlays, zz-zone-to-overlay,
;;            zz-zone-buffer-name, zz-overlay-union.
;;     Renamed: zz-zones-same-buffer-p to zz-zones-same-buffer-name-p.
;;     zz-zones-same-buffer-name-p: Use zz-zone-buffer-name.  Check also readable markers.
;;     zz-zone-ordered: Handle also readable markers.
;;     Require cl.el at compile time, for case macro.
;; 2018/04/19 dadams
;;     Added zz-map-query-replace-regexp-zones, zz-replace-string-zones, and zz-replace-regexp-zones, after fix
;;       of Emacs bug #27897.
;; 2018/01/09 dadams
;;     zz-readable-marker:
;;       If arg is already a readable marker just return it (idempotent).  If it is a marker then use its buffer.
;;       If it is a number then use it as is, using BUFFER arg or current buffer name.
;; 2017/08/02 dadams
;;     Added: zz-izones-from-noncontiguous-region, zz-zones-from-noncontiguous-region,
;;            zz-noncontiguous-region-from-izones, zz-noncontiguous-region-from-zones, zz-dot-pairs.
;; 2017/08/01 dadams
;;     Added: zz-query-replace-zones, zz-query-replace-regexp-zones - Emacs 25+ only.
;; 2017/06/05 dadams
;;     zz-set-fringe-for-narrowing: Use copy-face, not face-spec-set-2.  OK for Emacs 24.4+
;; 2017/06/04 dadams
;;     Added: zz-narrowing-use-fringe-flag, zz-fringe-for-narrowing, zz-set-fringe-for-narrowing.
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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(eval-when-compile (require 'cl)) ;; case (No `cl-case' for Emacs 22)

;; Quiet the byte-compiler.
(defvar hlt-last-face)                  ; In `highlight.el'
(defvar mode-line-modes)                ; Emacs 22+
(defvar narrow-map)                     ; Emacs 23+
(defvar region-extract-function)        ; Emacs 25+
(defvar repeat-message-function)        ; In `repeat.el'
(defvar repeat-previous-repeated-command) ; In `repeat.el'

;;;;;;;;;;;;;;;;;;;;;;;;;;
 

(defalias 'zz-user-error
  (if (fboundp 'user-error) #'user-error #'error)) ; For Emacs 22-23.

(defgroup zones nil
  "Zones of text - like multiple regions."
  :prefix "zz-"
  :group 'editing
  :link `(url-link :tag "Send Bug Report"
                   ,(concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=\
zones.el bug: \
&body=Describe bug here, starting with `emacs -Q'.  \
Don't forget to mention your Emacs and library versions."))
  :link '(url-link :tag "Download" "https://www.emacswiki.org/emacs/download/zones.el")
  :link '(url-link :tag "Description" "https://www.emacswiki.org/emacs/Zones")
  :link '(emacs-commentary-link :tag "Commentary" "zones"))

(when (>= emacs-major-version 23)       ; Emacs 23.1+
  ;; NOTE: Buffer-local face-remapping of fringe is not handled correctly until Emacs-27 (Emacs bug#33244).

  (defface zz-fringe-for-narrowing
      '((((background dark)) (:background "#FFFF2429FC15")) ; a dark magenta
        (t (:background "LightGreen")))
    "Face used for fringe when buffer is narrowed."
    :group 'zones :group 'faces)

  ;; FIXME?: This feature is really orthogonal to zones.  And should just loading this file enable the feature?
  (defcustom zz-narrowing-use-fringe-flag t
    "Non-nil means use fringe face `zz-fringe-for-narrowing' when narrowed."
    :type 'boolean :group 'zones
    :set (lambda (sym defs)
           (custom-set-default sym defs)
           (if (symbol-value sym)
               (add-hook 'post-command-hook #'zz-set-fringe-for-narrowing)
             (remove-hook 'post-command-hook #'zz-set-fringe-for-narrowing))))

  (defvar zz--fringe-remapping nil
    "Cookie from remapping face `fringe' to `zz-fringe-for-narrowing'.
Deleted by `face-remap-remove-relative' when buffer is widened.")
  (with-no-warnings (make-variable-buffer-local 'zz--fringe-remapping))

  (defun zz-set-fringe-for-narrowing ()
    "Remap face `fringe' to `zz-fringe-for-narrowing' if buffer is narrowed.
Remove remapping if not narrowed."
    (if (zz-buffer-narrowed-p)
        (unless zz--fringe-remapping
          (setq zz--fringe-remapping  (face-remap-add-relative 'fringe 'zz-fringe-for-narrowing))
          ;; FIXME: For some reason, the display is not always redrawn fully.
          (redraw-frame))
      (when zz--fringe-remapping
        (face-remap-remove-relative zz--fringe-remapping)
        ;; FIXME: For some reason, the display is not redrawn fully.
        (redraw-frame)
        (setq zz--fringe-remapping  nil))))

  )

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
  (let ((oldval  (symbol-value zz-izones-var)))
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
    (when (zz-readable-marker-p beg) (setq beg  (nth 2 beg)))
    (when (zz-readable-marker-p end) (setq end  (nth 2 end)))
    (if (<= beg end) zone `(,end ,beg ,@extra))))

(defun zz-zones-overlap-p (zone1 zone2)
  "Return non-nil if ZONE1 and ZONE2 overlap.
Assumes that each zone is ordered (its car <= its cadr).
The cddrs are ignored.

Zones that use markers do not overlap if the marker buffers differ."
  (and (zz-zones-same-buffer-name-p zone1 zone2)
       (progn (when (< (car zone2) (car zone1)) (setq zone1 (prog1 zone2 (setq zone2 zone1))))
              (<= (car zone2) (cadr zone1)))))

(defun zz-zones-same-buffer-name-p (zone1 zone2)
  "Return non-nil if ZONE1 and ZONE2 apply to the same buffer.
This is the case if `zz-zone-buffer-name' returns the same name for
each.  (A buffer with that name need not exist.)"
  (eq (zz-zone-buffer-name zone1) (zz-zone-buffer-name zone2)))

(defun zz-zone-buffer-name (zone)
  "Return the name of the buffer used by ZONE.
If the two ZONE positions specify different buffers, or if either is a
marker that points nowhere, then raise an error."
  (let* ((lim1  (car zone))
         (lim2  (cadr zone))
         (buf1  (cond ((markerp lim1)               (and (marker-buffer lim1)  (buffer-name (marker-buffer lim1))))
                      ((zz-readable-marker-p lim1)  (cadr lim1))
                      (t                            (buffer-name (current-buffer)))))
         (buf2  (cond ((markerp lim2)               (and (marker-buffer lim2)  (buffer-name (marker-buffer lim2))))
                      ((zz-readable-marker-p lim2)  (cadr lim2))
                      (t                            (buffer-name (current-buffer))))))
    (unless (and buf1  buf2) (error "Zone has marker(s) that point nowhere: %S" zone))
    (unless (equal buf1 buf2) (error "Zone has conflicting buffers: %S" zone))
    buf1))

(defun zz-do-zones (function &optional zones)
  "Like `zz-map-zones', but without returning the result of mapping.
The return value is undefined."
  (when (functionp function)
    (when (zz-izones-p zones)
      (setq zones  (zz-izone-limits zones nil 'ONLY-THIS-BUFFER)))
    (setq zones  (zz-zone-union zones))
    (dolist (zone  zones) (funcall function (car zone) (cadr zone)))))

(defun zz-map-zones (function &optional zones)
  "Map binary FUNCTION over ZONES, applying it to the limits of each zone.
ZONES can be a list of basic zones or a list like `zz-izones', that
is, zones that have identifiers.  By default, ZONES is the value of
`zz-izones'."
  (if (not (functionp function))
      (or zones  zz-izones)
    (when (zz-izones-p zones)
      (setq zones  (zz-izone-limits zones nil 'ONLY-THIS-BUFFER)))
    (setq zones  (zz-zone-union zones))
    (mapcar (lambda (zone) (funcall function (car zone) (cadr zone))) zones)))

(defun zz-do-izones (function &optional izones)
  "Like `zz-map-izones', but without returning the result of mapping.
The return value is undefined."
  (when (functionp function)
    (setq izones  (zz-unite-zones izones))
    (dolist (izone  izones) (funcall function (car izone) (cadr izone) (caddr izone)))))

(defun zz-map-izones (function &optional izones)
  "Map FUNCTION over IZONES.
Apply FUNCTION to the first three elements of each izone, that is, the
 identifier and the zone limits.
IZONES is a list like `zz-izones', that is, zones with identifiers.
By default, IZONES is the value of `zz-izones'."
  (if (not (functionp function))
      (or izones  zz-izones)
    (setq izones  (zz-unite-zones izones))
    (mapcar (lambda (izone) (funcall function (car izone) (cadr izone) (caddr izone))) izones)))

(defun zz-zones-complement (zones &optional beg end)
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
The limits need not be in numerical order.

Each limit can be a number or a marker, but zones with markers for
buffers other than BUFFER (default: current buffer) are ignored.

Any zones that use markers for a buffer other than BUFFER (default:
current buffer) are excluded.

Returns a new list, which is sorted by the lower limit of each zone,
which is its car.  (This is a non-destructive operation.)

Each zone in ZONES is first ordered, so that its car <= its cadr.
The resulting zones are then sorted by their cars.

`zz-two-zone-union' is then applied recursively to coalesce
overlapping or adjacent zones.  This means also that any EXTRA info is
combined whenever zones are merged together."
  (let* ((filtered-zones  (zz-remove-zones-w-other-buffer-markers zones buffer))
         (flipped-zones   (mapcar #'zz-zone-ordered filtered-zones))
         (sorted-zones    (sort flipped-zones #'zz-car-<)))
    (zz-zone-union-1 sorted-zones)))

;; Recursive version.
;; (defun zz-zone-union-1 (zones)
;;   "Helper for `zz-zone-union'."
;;   (if (null (cdr zones))
;;       zones
;;     (let ((new  (zz-two-zone-union (car zones) (cadr zones))))
;;       (if new
;;           (zz-zone-union-1 (cons new (cddr zones)))
;;         (cons (car zones) (zz-zone-union-1 (cdr zones)))))))

(defun zz-zone-union-1 (zones)
  "Helper for `zz-zone-union'."
  (if (null (cdr zones))
      zones
    (let ((acc  ())
          new)
      (while zones
        (setq new  (and (cdr zones)  (zz-two-zone-union (car zones) (cadr zones))))
        (if new
            (setq zones  (cons new (cddr zones)))
          (setq acc    (cons (car zones) acc)
                zones  (cdr zones))))
      (setq acc  (nreverse acc)))))

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

The cddr of a non-nil result (its list of EXTRA information) is the
intersection of the EXTRA information of each zone:

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
(defun zz-select-region (arg &optional msgp) ; Not bound.
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
(defun zz-narrow (arg &optional msgp)   ; Not bound.
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
    (cond ((or (consp arg)  (and (null (cdr val))  (zz-buffer-narrowed-p)))
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
Put `zz-narrow' on `mouse-2' for the lighter suffix."
  (let* ((%n-cons  (zz-regexp-car-member "%n\\(.*\\)\\'" mode-line-modes)))
    (when %n-cons
      (setcar %n-cons (replace-regexp-in-string "%n\\(.*\\)"
                                                (if (zz-buffer-narrowed-p) zz-lighter-narrowing-part "")
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
                           (car %n-cons)))))

(defun zz-regexp-car-member (regexp xs)
  "Like `member', but tests by matching REGEXP against cars."
  (and (consp xs)  (if (and (stringp (car xs))  (zz-string-match-p regexp (car xs)))
                       xs
                     (zz-regexp-car-member regexp (cdr xs)))))

;;;###autoload
(defun zz-add-zone (start end &optional variable not-buf-local-p set-var-p msg) ; Bound to `C-x n a'.
  "Add an izone for the text from START to END to the izones of VARIABLE.
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
* Non-nil MSG means echo the zone limits, preceded by string MSG."
  (interactive (let* ((beg    (region-beginning))
                      (end    (region-end))
                      (var    (or (and current-prefix-arg  (zz-read-any-variable "Variable: " zz-izones-var))
                                  zz-izones-var))
                      (npref  (prefix-numeric-value current-prefix-arg))
                      (nloc   (and current-prefix-arg  (<= npref 0)  (not (boundp var))))
                      (setv   (and current-prefix-arg  (or (consp current-prefix-arg)  (= npref 0)))))
                 (list beg end var nloc setv "Recorded zone: ")))
  (let* ((mrk1     (make-marker))
         (mrk2     (make-marker))
         (var      (or variable  zz-izones-var))
         (_IGNORE  (unless (or not-buf-local-p  (boundp var)) (make-local-variable var)))
         (_IGNORE  (when set-var-p (setq zz-izones-var  var)))
         (_IGNORE  (unless (boundp var) (set var ())))
         (val      (symbol-value var))
         sans-id  id-cons  id)
    (unless (zz-izones-p val) (error "Not an izones variable: `%s', value: `%S'" var val))
    (move-marker mrk1 start)
    (move-marker mrk2 end)
    (setq sans-id  (list mrk1 mrk2)
          id-cons  (rassoc sans-id val)
          id       (if id-cons (car id-cons) (1+ (length val))) ; 1-based, not 0-based.
          val      (set var (zz-rassoc-delete-all sans-id val))) ; Destructive operation.
    (unless (and (= mrk1 1)  (= mrk2 (1+ (buffer-size)))) (set var `((,id ,mrk1 ,mrk2) ,@val)))
    (when msg (message "%s%d to %d" msg (marker-position mrk1) (marker-position mrk2)))
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
   (let* ((var      (or (and current-prefix-arg  (zz-read-any-variable "Variable: " zz-izones-var))
                        zz-izones-var))
          (npref    (prefix-numeric-value current-prefix-arg))
          (nloc     (and current-prefix-arg  (<= npref 0)  (not (boundp var))))
          (setv     (and current-prefix-arg  (or (consp current-prefix-arg)  (= npref 0))))
          ;; Repeat all of the variable tests and actions, since we need to have the value, for its length.
          (_IGNORE  (unless nloc (make-local-variable var)))
          (_IGNORE  (when setv (setq zz-izones-var var)))
          (_IGNORE  (unless (boundp var) (set var ())))
          (val      (symbol-value var))
          (_IGNORE  (unless (zz-izones-p val)
                      (error "Not an izones variable: `%s', value: `%S'" var val)))
          (_IGNORE  (unless val (error "No zones - variable `%s' is empty" var)))
          (len      (length val))
          (num      (if (= len 1) 1 (read-number (format "Delete zone numbered (1 to %d): " len)))))
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
        posn)
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

(defun zz-readable-marker (number-or-marker &optional num-buffer)
  "Return a readable marker equivalent to NUMBER-OR-MARKER, or nil.
Return nil if NUMBER-OR-MARKER is not `number-or-marker-p'.
\(If NUMBER-OR-MARKER is already a readable marker then return it.)

A readable marker satisfies `zz-readable-marker-p'.  It has the form
\(marker BUFFER POSITION), where BUFFER is a buffer name (string) and
POSITION is a buffer position (number).

If NUMBER-OR-MARKER is itself a readable marker then return it.

If NUMBER-OR-MARKER is a marker then its buffer is used as BUFFER.

If NUMBER-OR-MARKER is a number then:
 If   NUM-BUFFER names an existing buffer then it is used as BUFFER.
 Else the name of the current buffer is used as BUFFER.

This is a non-destructive operation."
  (cond ((zz-readable-marker-p number-or-marker) number-or-marker)
        ((markerp number-or-marker)
         `(marker ,(marker-buffer number-or-marker) ,(marker-position number-or-marker)))
        ((numberp number-or-marker)
         `(marker
           ,(buffer-name (or (and (stringp num-buffer)  (get-buffer num-buffer))  (current-buffer)))
           ,number-or-marker))
        (t nil)))

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

;; Non-destructive version.
;;
;; (defun zz-izone-limits-in-bufs (buffers &optional variable)
;;   "Return a list of all `zz-izone-limits' for each buffer in BUFFERS.
;; That is, return a list of all recorded buffer zones for BUFFERS.
;; If BUFFERS is nil, return the zones recorded for the current buffer.
;;
;; This is a non-destructive operation: The list returned is independent
;; of the `zz-izone-limits' list in each of the buffers.
;;
;; Optional arg VARIABLE is the izones variable to use.  If nil,
;; use the value of `zz-izones-var'.  The variable is evaluated in each
;; buffer (or in the current buffer, if BUFFERS is nil)."
;;
;;   (let ((limits  ()))
;;     (dolist (buf  (or (reverse buffers)  (list (current-buffer)))) ; Reverse so we keep the order.
;;       (with-current-buffer buf
;;         (setq limits  (append (zz-izone-limits (symbol-value (or variable  zz-izones-var))
;;                                              buf
;;                                              'ONLY-THIS-BUFFER)
;;                               limits))))
;;     limits))

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
    (dolist (x  xs) (unless (funcall pred x) (push x result)))
    (nreverse result)))

;; Useful for commands that want to act on  regions in multiple buffers (e.g., visible buffers only).
;;
;; Same as `icicle-remove-if-not' etc.
(defun zz-remove-if-not (pred xs)
  "A copy of list XS with only elements that satisfy predicate PRED."
  (let ((result  ()))
    (dolist (x  xs) (when (funcall pred x) (push x result)))
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

(defalias 'zz-buffer-narrowed-p
    (if (fboundp 'buffer-narrowed-p)
        #'buffer-narrowed-p             ; Emacs 24+
      (lambda () (/= (- (point-max) (point-min)) (buffer-size)))))

(defalias 'zz-string-match-p
  (if (fboundp 'string-match-p)
      #'string-match-p                  ; Emacs 23+
   (lambda (regexp string &optional start)
     "Like `string-match', but this saves and restores the match data."
      (save-match-data (string-match regexp string start)))))

(defun zz-repeat-command (command)
  "Repeat COMMAND."
  (require 'repeat)                   ;Define its vars before we let-bind them!
  (let ((repeat-previous-repeated-command  command)
        (repeat-message-function           #'ignore)
        (last-repeatable-command           'repeat))
    (repeat nil)))

;;;###autoload
(defun zz-narrow-repeat ()              ; Bound to `C-x n x'.
  "Cycle to the next buffer restriction (narrowing).
This is a repeatable version of `zz-narrow'.

Note that if the value of `zz-izones-var' is not buffer-local then you
can use this command to cycle among regions in multiple buffers."
  (interactive)
  (zz-repeat-command 'zz-narrow))

;;;###autoload
(defun zz-select-region-repeat () ; Bound to `C-x n r'.
  "Cycle to the next region.
This is a repeatable version of `zz-select-region'."
  (interactive)
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
Return the new value of TO-VARIABLE.
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
  (prog1 (set to-variable (copy-sequence (symbol-value from-variable)))
    (when msgp (message "Cloned `%s' to `%s'" from-variable to-variable))))

;;;###autoload
(defalias 'zz-clone-and-coalesce-zones #'zz-clone-and-unite-zones)
;;;###autoload
(defun zz-clone-and-unite-zones (from-variable to-variable &optional msgp) ; Bound to `C-x n C'
  "Clone FROM-VARIABLE to TO-VARIABLE, then unite (coalesce) TO-VARIABLE.
That is, use`zz-clone-zones' to fill TO-VARIABLE, then use
`zz-unite-zones' on TO-VARIABLE.

United zones are in ascending order of their cars.
Return the new value of TO-VARIABLE.

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
  (prog1 (zz-unite-zones to-variable)
    (when msgp (message "Cloned `%s' to `%s' and united `%s'" from-variable to-variable to-variable))))

;;;###autoload
(defalias 'zz-coalesce-zones #'zz-unite-zones)
;;;###autoload
(defun zz-unite-zones (&optional variable msgp) ; Bound to `C-x n u'
  "Coalesce (unite) the izones of VARIABLE.
A non-destructive operation: The new value of VARIABLE is a new list.
Return the new value of VARIABLE.

United zones are in ascending order of their cars.

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
         (_IGNORE     (unless (boundp var) (set var ())))
         (val         (symbol-value var))
         (_IGNORE     (unless (zz-izones-p val) (error "Not an izones variable: `%s', value: `%S'" var val)))
         (zone-union  (zz-zone-union (zz-izone-limits val))))
    (set var  (zz-izones-from-zones zone-union))
    (when msgp
      (let ((len  (length (symbol-value var))))
        (message "Zones united for variable `%s': %d zone%s now" var len (if (> len 1) "s" ""))))
    (symbol-value var)))

;;;###autoload
(defalias 'zz-add-zone-and-coalesce #'zz-add-zone-and-unite)
;;;###autoload
(defun zz-add-zone-and-unite (start end &optional variable msg) ; Bound to `C-x n A'.
  "Add an izone from START to END to those of VARIABLE, and coalesce.
Use `zz-add-zone', then apply `zz-unite-zones'.
United zones are in ascending order of their cars.
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
* Non-nil MSG means echo messages for adding the zone and uniting
  zones.  In this case MSG is the message prefix for `zz-add-zone'."
  (interactive (let ((beg    (region-beginning))
                     (end    (region-end))
                     (var    (or (and current-prefix-arg  (zz-read-any-variable "Variable: " zz-izones-var))
                                 zz-izones-var))
                     (npref  (prefix-numeric-value current-prefix-arg)))
                 (when (and current-prefix-arg  (>= npref 0)) (make-local-variable var))
                 (when (and current-prefix-arg  (<= npref 0)) (setq zz-izones-var var))
                 (list beg end var "Zone recorded: ")))
  (unless variable (setq variable  zz-izones-var))
  (zz-add-zone start end variable nil nil msg)
  (zz-unite-zones variable msg)
  (symbol-value variable))

;;;###autoload
(defun zz-add-zones-from-highlighting (&optional start end face only-hlt-face overlay/text fonk-lock-p msgp)
  "Add highlighted areas as zones to izones variable.
By default, the text used is that highlighted with `hlt-last-face'.
With a non-negative prefix arg you are instead prompted for the face.

With a non-positive prefix arg use face property `font-lock-face'
instead of property `face'.

The izones variable to use is the value of `zz-izones-var'.  You can
set this to a different variable anytime using `\\[zz-set-izones-var]'.

All highlighting is checked, both overlays and face text properties.

The number of highlighted areas added as zones is echoed in a message.
This might be less than the number of zones added, because there can
be multiple highlights with the same face for the same area.

When called from Lisp:

* Non-nil START and END are the buffer limits to search.
* Non-nil FACE is the highlighting face to look for.
* Non-nil ONLY-HLT-FACE means check only `highlight.el' highlighting.
  (By default, any highlighting is checked.)
* If OVERLAY/TEXT is `text-prop' then only text-property highlighting
  is checked. If it is `overlay' then only overlay highlighting is
  checked.  (If nil then both are checked.)
* Non-nil FONK-LOCK-P means check property `font-lock-face'.  By
  default (nil), check property `face'."
  (interactive
   (let ((numarg  (and current-prefix-arg  (prefix-numeric-value current-prefix-arg))))
     (unless (require 'highlight nil t) (zz-user-error "You need library `highlight.el' to use this command"))
     `(,@(hlt-region-or-buffer-limits)
         ,(if (natnump numarg)
              (hlt-read-bg/face-name "Create zones highlighted with face: ")
            hlt-last-face)
         nil nil ,(and numarg  (<= numarg 0)) t)))
  (require 'highlight)
  (unless (and start  end) (let ((start-end  (hlt-region-or-buffer-limits)))
                             (setq start  (car start-end)
                                   end    (cadr start-end))))
  (unless face (setq face  hlt-last-face))
  (let ((hlt-use-overlays-flag     (case overlay/text
                                     (text-prop  nil) ; Only text property
                                     (overlay    'only) ; Only overlay
                                     (t          t))) ; Default: both
        (hlt-act-on-any-face-flag  (not only-hlt-face))
        (hlt-face-prop             (if fonk-lock-p 'font-lock-face 'face))
        (count                     0))
    (save-excursion
      (save-window-excursion
        (goto-char start)
        (let ((zone-beg  start)
              zone-end zone)
          (while (and zone-beg  (< zone-beg end))
            (setq zone      (hlt-next-highlight zone-beg end face nil nil 'no-error-msg)
                  zone-beg  (car zone)
                  zone-end  (cdr zone))
            ;; Create zone from `zone-beg' to `zone-end' if highlighted.  Add it to zones list.
            (when hlt-use-overlays-flag
              (let ((overlays  (overlays-at zone-beg)))
                (while overlays
                  (when (and (or hlt-act-on-any-face-flag
                                 (equal face (overlay-get (car overlays) 'hlt-highlight)))
                             (equal face (overlay-get (car overlays) hlt-face-prop)))
                    (zz-add-zone zone-beg zone-end)
                    (setq count  (1+ count)))
                  (when overlays (setq overlays  (cdr overlays))))))
            (when (and (not (eq hlt-use-overlays-flag 'only))
                       (or hlt-act-on-any-face-flag  (equal face (get-text-property (point) 'hlt-highlight)))
                       (let ((pt-faces  (get-text-property (point) hlt-face-prop)))
                         (if (consp pt-faces) (memq face pt-faces) (equal face pt-faces))))
              (zz-add-zone zone-beg zone-end)
              (setq count  (1+ count)))))))
    (when msgp
      (case count
        (0 (message "NO zones added or updated"))
        (1 (message "1 zone added or updated"))
        (t (message "%s highlighted areas added or updated as zones" count))))))

;;;###autoload
(defun zz-set-zones-from-highlighting (&optional start end face only-hlt-face overlay/text fonk-lock-p msgp)
  "Replace value of izones variable with zones from the highlighted areas.
Like `zz-add-zones-from-highlighting' (which see), but it replaces any
current zones instead of adding to them."
  (interactive
   (let ((numarg  (and current-prefix-arg  (prefix-numeric-value current-prefix-arg))))
     (unless (require 'highlight nil t) (zz-user-error "You need library `highlight.el' to use this command"))
     `(,@(hlt-region-or-buffer-limits)
         ,(if (natnump numarg)
              (hlt-read-bg/face-name "Create zones highlighted with face: ")
            hlt-last-face)
         nil nil ,(and numarg  (<= numarg 0)) t)))
  (set zz-izones-var ())
  (zz-add-zones-from-highlighting start end face only-hlt-face overlay/text fonk-lock-p msgp))

;;;###autoload
(defun zz-create-face-zones (face &optional start end variable msgp)
  "Set an izones variable to (united) zones of a face or background color.
You are prompted for a face name or a color name.  If you enter a
color, it is used for the face background.  The face foreground is
determined by the value of `hlt-auto-face-foreground'.
The variable defaults to `zz-izones'.  With a prefix arg you are
  prompted for a different izones variable."
  (interactive
   (progn
     (unless (require 'highlight nil t)
       (error "You need library `highlight.el' for this command"))
     (let ((fac  (hlt-read-bg/face-name "Choose background color or face: "
                                        (and (symbolp hlt-last-face)  (symbol-name hlt-last-face))))
           (var  (or (and current-prefix-arg  (zz-read-any-variable "Variable: " zz-izones-var))
                     zz-izones-var)))
       (if (hlt-nonempty-region-p)
           (if (< (point) (mark)) (list (point) (mark) var t) (list (mark) (point) var t))
         (list fac (point-min) (point-max) var t)))))
  (unless (require 'highlight nil t)
    (error "You need library `highlight.el' for this command"))
  (unless (require 'isearch-prop nil t)
    (error "You need library `isearch-prop.el' for this command"))
  (unless (require 'zones nil t)
    (error "You need library `zones' for this command"))
  (font-lock-default-fontify-buffer)    ; Fontify the whole buffer.
  (zz-set-zones-from-highlighting start end face nil 'text-prop)
  (zz-unite-zones variable t))

;;---------------------
(defun zz-add-key-bindings-to-narrow-map (bindings)
  "Add BINDINGS to `narrow-map'.
\(For Emacs prior to Emacs 24, add bindings to prefix key `C-x n'.)"
  (let ((map  (if (boundp 'narrow-map) narrow-map (lookup-key ctl-x-map "n"))))
    (when (keymapp map)
      (dolist (binding  bindings)
        (let ((kseq  (car binding))
              (cmd   (cdr binding)))
          (unless (lookup-key map kseq) (define-key map kseq cmd)))))))

(zz-add-key-bindings-to-narrow-map '(("a" . zz-add-zone)
                                     ("A" . zz-add-zone-and-unite)
                                     ("c" . zz-clone-zones)
                                     ("C" . zz-clone-and-unite-zones)
                                     ("\C-d" . zz-delete-zone)
                                     ("r" . zz-select-region-repeat)
                                     ("u" . zz-unite-zones)
                                     ("v" . zz-set-izones-var)
                                     ("x" . zz-narrow-repeat)))

(eval-after-load "highlight"
  '(zz-add-key-bindings-to-narrow-map '(("h" . hlt-highlight-regions)
                                        ("H" . hlt-highlight-regions-in-buffers)
                                        ("l" . zz-add-zones-from-highlighting)
                                        ("L" . zz-set-zones-from-highlighting))))

;; Call `zz-add-zone' if interactive or if `zz-add-zone-anyway-p'.

(defun zz-narrow-advice (interactive-p)
  (when (or interactive-p  zz-add-zone-anyway-p)
    (zz-add-zone (point-min) (point-max) nil nil nil "Narrowed, and recorded zone: ")))

(defadvice narrow-to-region (after zz-add-zone--region activate)
  "Push the region limits to the current `zz-izones-var'.
You can use `C-x n x' to widen to a previous buffer restriction.

This is a destructive operation. The list structure of the variable
value can be modified."
  (zz-narrow-advice (interactive-p)))

(defadvice narrow-to-defun (after zz-add-zone--defun activate)
  "Push the defun limits to the current `zz-izones-var'.
You can use `C-x n x' to widen to a previous buffer restriction.

This is a destructive operation. The list structure of the variable
value can be modified."
  (zz-narrow-advice (interactive-p)))

;; Call `zz-add-zone' if interactive or `zz-add-zone-anyway-p'.
;;
(defadvice narrow-to-page (after zz-add-zone--defun activate)
  "Push the page limits to the current `zz-izones-var'.
You can use `C-x n x' to widen to a previous buffer restriction.

This is a destructive operation. The list structure of the variable
value can be modified."
  (zz-narrow-advice (interactive-p)))

(when (> emacs-major-version 24)

  (defun zz-izones-from-noncontiguous-region ()
    "Return a list of izones from `region-extract-function' bounds."
    (let ((ii  0))
      (mapcar (lambda (posn) (cons (setq ii  (1+ ii)) (list (copy-marker (car posn)) (copy-marker (cdr posn)))))
              (funcall region-extract-function 'bounds))))

  (defun zz-zones-from-noncontiguous-region ()
    "Return a list of basic zones from `region-extract-function' bounds."
    (mapcar (lambda (posn) (list (copy-marker (car posn)) (copy-marker (cdr posn))))
            (funcall region-extract-function 'bounds)))

  (defun zz-query-replace-zones (from-string to-string &optional delimited start end backward zones)
    "`query-replace' in the zones currently defined in the current buffer.
The value of variable `zz-izones' defines the zones."
    (interactive
     (let* ((common  (query-replace-read-args (concat "Query replace"
                                                      (if current-prefix-arg
                                                          (if (eq current-prefix-arg '-) " backward" " word")
                                                        "")
                                                      (if (use-region-p) " in region" ""))
                                              nil))
            (beg     (point-max))
            (end     (point-min))
            (zs      (zz-noncontiguous-region-from-izones zz-izones-var)))
       (list (nth 0 common) (nth 1 common) (nth 2 common) beg end (nth 3 common) zs)))
    (unless zones (error "No zones to search"))
    (let ((region-extract-function  (lambda (_ignore) zones)))
      (query-replace from-string to-string delimited start end backward 'REGION-NONCONTIGUOUS-P)))

  (defun zz-query-replace-regexp-zones (regexp to-string &optional delimited start end backward zones)
    "`query-replace-regexp' in the zones currently defined in the current buffer.
The value of variable `zz-izones' defines the zones."
    (interactive
     (let* ((common  (query-replace-read-args (concat "Query replace"
                                                      (if current-prefix-arg
                                                          (if (eq current-prefix-arg '-) " backward" " word")
                                                        "")
                                                      (if (use-region-p) " in region" ""))
                                              nil))
            (beg     (point-max))
            (end     (point-min))
            (zs      (zz-noncontiguous-region-from-izones zz-izones-var)))
       (list (nth 0 common) (nth 1 common) (nth 2 common) beg end (nth 3 common) zs)))
    (unless zones (error "No zones to search"))
    (let ((region-extract-function  (lambda (_ignore) zones)))
      (query-replace-regexp regexp to-string delimited start end backward 'REGION-NONCONTIGUOUS-P)))


  (when (> emacs-major-version 26) ; These three depend on the fix to Emacs bug #27897.

    (defun zz-map-query-replace-regexp-zones (regexp to-strings &optional n start end zones)
      "`map-query-replace-regexp' in the zones currently defined in the current buffer.
The value of variable `zz-izones' defines the zones."
      (interactive
       (let* ((from  (read-regexp "Map query replace (regexp): " nil query-replace-from-history-variable))
              (to    (read-from-minibuffer (format "Query replace %s with (space-separated strings): "
                                                   (query-replace-descr from))
                                           nil nil nil query-replace-to-history-variable from t))
              (beg   (point-max))
              (end   (point-min))
              (zs      (zz-noncontiguous-region-from-izones zz-izones-var)))
         (list from to (and current-prefix-arg  (prefix-numeric-value current-prefix-arg)) beg end zs)))
      (unless zones (error "No zones to search"))
      (let ((region-extract-function  (lambda (_ignore) zones)))
        (map-query-replace-regexp regexp to-strings n start end 'REGION-NONCONTIGUOUS-P)))

    (defun zz-replace-string-zones (from-string to-string &optional delimited start end backward zones)
      "`replace-string' in the zones currently defined in the current buffer.
The value of variable `zz-izones' defines the zones."
      ;; (declare (interactive-only "use `search-forward' and `replace-match' instead."))
      (interactive
       (let ((common  (query-replace-read-args (concat "Replace"
                                                       (if current-prefix-arg
                                                           (if (eq current-prefix-arg '-) " backward" " word")
                                                         "")
                                                       " string"
                                                       (if (use-region-p) " in region" ""))
                                               nil))
             (beg     (point-max))
             (end     (point-min))
             (zs      (zz-noncontiguous-region-from-izones zz-izones-var)))
         (list (nth 0 common) (nth 1 common) (nth 2 common) beg end (nth 3 common) zs)))
      (unless zones (error "No zones to search"))
      (let ((region-extract-function  (lambda (_ignore) zones)))
        (replace-string from-string to-string delimited start end backward 'REGION-NONCONTIGUOUS-P)))

    (defun zz-replace-regexp-zones (regexp to-string &optional delimited start end backward zones)
      "`replace-regexp' in the zones currently defined in the current buffer.
The value of variable `zz-izones' defines the zones."
      ;; (declare (interactive-only "use `re-search-forward' and `replace-match' instead."))
      (interactive
       (let ((common  (query-replace-read-args (concat "Replace"
                                                       (if current-prefix-arg
                                                           (if (eq current-prefix-arg '-) " backward" " word")
                                                         "")
                                                       " regexp"
                                                       (if (use-region-p) " in region" ""))
                                               t))
             (beg     (point-max))
             (end     (point-min))
             (zs      (zz-noncontiguous-region-from-izones zz-izones-var)))
         (list (nth 0 common) (nth 1 common) (nth 2 common) beg end (nth 3 common) zs)))
      (unless zones (error "No zones to search"))
      (let ((region-extract-function  (lambda (_ignore) zones)))
        (replace-regexp regexp to-string delimited start end backward 'REGION-NONCONTIGUOUS-P)))

    )                                   ; Emacs 27+
  )                                     ; Emacs 25+

(defun zz-noncontiguous-region-from-izones (&optional variable)
  "Return a noncontiguous region from value of value of VARIABLE.
VARIABLE defaults to the value of `zz-izones-var'.  An Emacs
\"noncontiguous region\" (Emacs 25+) is what the value of
`region-extract-function' returns for a METHOD argument of `bounds'.
It is like a list of united basic zones, but the entry pairs are
dotted: `(beg . end)', not `(beg end)'."
  (let ((iz-var  (make-symbol "NRFI")))
    (zz-dot-pairs (zz-izone-limits (zz-clone-and-unite-zones (or variable  zz-izones-var) iz-var)))))

(defun zz-noncontiguous-region-from-zones (basic-zones)
  "Return a noncontiguous region from a list of BASIC-ZONES.
An Emacs \"noncontiguous region\" (Emacs 25+) is what the value of
`region-extract-function' returns.  It is like a list of united basic
zones, but the entry pairs are dotted: `(beg . end)', not `(beg end)'."
  (zz-dot-pairs (zz-zone-union basic-zones)))

(defun zz-dot-pairs (pairs)
  "Dot PAIRS, a list of lists, each of which has at least two elements."
  (mapcar (lambda (b-e) (cons (car b-e) (cadr b-e))) pairs))

(defun zz-overlay-to-zone (overlay &optional pos-type)
  "Return a basic zone derived from OVERLAY.
If OVERLAY is not an overlay or it has been deleted (has no buffer)
then return nil.

Optional arg POS-TYPE controls the kind of position used by the zone:
 `markers'          - use markers
 `readable-markers' - use readable-markers
 nil                - use positive integers"
  (let ((buf  (overlay-buffer overlay)))
    (and buf
         (let* ((beg    (overlay-start overlay))
                (end    (overlay-end overlay))
                (props  (overlay-properties overlay)))
           (case pos-type
             (markers           (setq beg  (copy-marker beg)
                                      end  (copy-marker end)))
             (readable-markers  (setq beg  (zz-readable-marker beg buf)
                                      end  (zz-readable-marker end buf))))
           `(,beg ,end ,@props)))))

(defun zz-overlays-to-zones (overlays &optional pos-type izones-p)
  "Return a list of zones derived from OVERLAYS list.
This uses `zz-overlay-to-zone', which see for optional arg POS-TYPE.

By default, the zones are basic zones.  Non-nil optional arg IZONES-P
means they are izones.

Note: If you plan to coelesce the resulting ZONES (using, e.g.,
`zz-unite-zones') then you will no doubt want to ensure that the
OVERLAYS are all of the same type."
  (let ((zones  (delq nil (mapcar `(lambda (ov) (zz-overlay-to-zone ov ',pos-type)) overlays))))
    (when izones-p (setq zones  (zz-izones-from-zones zones)))
    zones))

(defun zz-zone-to-overlay (zone &optional properties front-advance rear-advance)
  "Create and return an overlay derived from ZONE.
ZONE is a basic zone; it has the form (LIMIT1 LIMIT2 . EXTRA).

If EXTRA is not a list then it is ignored.
If EXTRA is a list then it is treated as a plist of overlay
properties.  This is so regardless of the type of any given
property (e.g., it need not be a symbol).  If the list has an odd
number of elements then the last one is treated as a property with
value `nil'.

Optional arg PROPERTIES is a plist to add to the overlay properties
coming from the `cddr' of ZONE (after adding the value `nil' if the
latter list has an odd length.

Optional args FRONT-ADVANCE and REAR-ADVANCE are passed to
 `make-overlay'."
  (let* ((buf     (get-buffer (zz-zone-buffer-name zone)))
         (zon     (zz-zone-ordered zone))
         (beg     (car zon))
         (end     (cadr zone))
         (zprops  (cddr zone))
         (zprops  (and (listp zprops)  zprops))
         (ov      (make-overlay (zz-marker-from-object beg) (zz-marker-from-object end) buf
                                front-advance rear-advance)))
    (while zprops
      (overlay-put ov (car zprops) (cadr zprops))
      (setq zprops  (cddr zprops)))
    (while properties
      (overlay-put ov (car properties) (cadr properties))
      (setq properties  (cddr properties)))
    ov))

(defun zz-zones-to-overlays (zones)
  "Return a list of overlays derived from ZONES list of basic zones."
  (mapcar #'zz-zone-to-overlay zones))

(defun zz-overlay-union (overlays &optional buffer dont-delete-p)
  "Return the union (coalescence) of the overlays in list OVERLAYS.
Overlapping and adjacent overlays are coalesced to a single overlay
whose set of properties are the union of the properties of OVERLAYS.

Returns a list of new overlays, which is sorted by the lower limit of
each overlay.

Only overlays in BUFFER (default: current buffer) are coalesced.

By default the original OVERLAYS in BUFFER are all deleted.  Non-nil
optional arg DONT-DELETE-P means do not delete them.  Any of the
original overlays that are not in BUFFER (and so were not coalesced)
are included in the returned list."
  (let* ((zones    (zz-zone-union (zz-overlays-to-zones overlays 'markers) buffer))
         (new-ovs  (zz-zones-to-overlays zones)))
    (dolist (ov  overlays)
      (if (eq (overlay-buffer ov) (or buffer  (current-buffer)))
          (unless dont-delete-p (delete-overlay ov)) ; Delete original overlays in BUFFER
        (push ov new-ovs)))             ; Add other-buffer overlays to return value.
    new-ovs))

;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'zones)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; zones.el ends here
