;;; isearch-prop.el --- Search text-property or overlay-property contexts.
;;
;; Filename: isearch-prop.el
;; Description: Search text-property or overlay-property contexts.
;; Author: Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 2011-2018, Drew Adams, all rights reserved.
;; Created: Sun Sep  8 11:51:41 2013 (-0700)
;; Version: 0
;; Package-Requires: ()
;; Last-Updated: Mon Jan  1 14:35:33 2018 (-0800)
;;           By: dradams
;;     Update #: 1427
;; URL: https://www.emacswiki.org/emacs/download/isearch-prop.el
;; Doc URL: https://www.emacswiki.org/emacs/IsearchPlus
;; Keywords: search, matching, invisible, thing, help
;; Compatibility: GNU Emacs: 23.2+, 24.x, 25.x, 26.x
;;
;; Features that might be required by this library:
;;
;;   `hexrgb', `thingatpt', `thingatpt+', `zones'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Search text-property or overlay-property contexts.
;;
;;  Such contexts are either zones of text that have certain text
;;  properties or overlays that have certain overlay properties.
;;
;;  This file is part of package Isearch+, which includes also file
;;  `isearch+.el'.  You can use either of the files without the other,
;;  if you like, but I recommend that you use them together.
;;
;;  Some of the features of library `isearch+.el' need library
;;  `zones.el'.  There is no error if you do not have that library,
;;  but those features will be unavailable without it.
;;
;;  Some of the features provided by this library are based on similar
;;  features introduced by Icicles
;;  (https://www.emacswiki.org/emacs/Icicles).
;;
;;  More description below - see Overview of Features.
;;
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
;;  (@> "Overview of Features")
;;  (@> "Macros")
;;  (@> "Options")
;;  (@> "Internal Variables")
;;  (@> "Keys")
;;  (@> "General Search-Property Commands")
;;  (@> "General Non-Interactive Functions")
;;  (@> "Search-Zones Commands and Functions")
;;  (@> "Imenu Commands and Functions")
;;  (@> "THING Commands and Functions")
;;  (@> "Character-Property Search")
;;
;;
;;  Macros defined here:
;;
;;    `isearchp-with-comments-hidden'.
;;
;;  Commands defined here:
;;
;;    `isearchp-add-prop-to-lazy-highlights',
;;    `isearchp-add-prop-to-other-prop-zones', `isearchp-cleanup',
;;    `isearchp-hide/show-comments', `isearchp-imenu',
;;    `isearchp-imenu-command', `isearchp-imenu-macro',
;;    `isearchp-imenu-non-interactive-function',
;;    `isearchp-lazy-highlights-forward',
;;    `isearchp-lazy-highlights-forward-regexp',
;;    `isearchp-make-anti-zones-invisible',
;;    `isearchp-make-anti-zones-visible',
;;    `isearchp-make-zones-invisible', `isearchp-make-zones-visible',
;;    `isearchp-mark-lazy-highlights',
;;    `isearchp-narrow-to-lazy-highlights',
;;    `isearchp-narrow-to-matching-zones',
;;    `isearchp-next-visible-thing',
;;    `isearchp-previous-visible-thing', `isearchp-property-backward',
;;    `isearchp-property-backward-regexp',
;;    `isearchp-property-forward', `isearchp-property-forward-regexp',
;;    `isearchp-put-prop-on-region', `isearchp-put-prop-on-zones',
;;    `isearchp-regexp-context-regexp-search',
;;    `isearchp-regexp-context-search',
;;    `isearchp-regexp-define-contexts',
;;    `isearchp-remove-all-properties', `isearchp-remove-dimming',
;;    `isearchp-remove-property', `isearchp-thing',
;;    `isearchp-thing-define-contexts', `isearchp-thing-regexp',
;;    `isearchp-toggle-anti-zones-invisible',
;;    `isearchp-toggle-complementing-domain',
;;    `isearchp-toggle-dimming-outside-search-area',
;;    `isearchp-toggle-ignoring-comments',
;;    `isearchp-toggle-hiding-comments',
;;    `isearchp-toggle-zone/anti-zone-visibility',
;;    `isearchp-toggle-zones-invisible', `isearchp-zones-backward',
;;    `isearchp-zones-backward-regexp', `isearchp-zones-forward',
;;    `isearchp-zones-forward-regexp'.
;;
;;  User options defined here:
;;
;;    `isearchp-dimming-color',
;;    `isearchp-dim-outside-search-area-flag',
;;    `isearchp-hide-whitespace-before-comment-flag',
;;    `isearchp-ignore-comments-flag',
;;    `isearchp-query-replace-zones-flag'.
;;
;;  Non-interactive functions defined here:
;;
;;    `isearchp-add-regexp-as-property',
;;    `isearchp-add/remove-dim-overlay',
;;    `isearchp-bounds-of-thing-at-point',
;;    `isearchp-complement-dimming', `isearchp-defined-thing-p',
;;    `isearchp-dim-color', `isearchp-dim-face-spec',
;;    `isearchp-exclude-zones-w-no-lazy-highlight',
;;    `isearchp-lazy-highlights-forward-1',
;;    `isearchp-lazy-highlights-present-p', `isearchp-message-prefix',
;;    `isearchp-next-visible-thing-1',
;;    `isearchp-next-visible-thing-2',
;;    `isearchp-next-visible-thing-and-bounds',
;;    `isearchp-properties-in-buffer', `isearchp-property-1',
;;    `isearchp-property-default-match-fn',
;;    `isearchp-property-filter-pred', `isearchp-property-matches-p',
;;    `isearchp-read-context-regexp', `isearchp-read-face-names',
;;    `isearchp-read-face-names--read', `isearchp-read-sexps',
;;    `isearchp-regexp-read-args', `isearchp-regexp-scan',
;;    `isearchp-remove-duplicates',
;;    `isearchp-restore-pred-and-remove-dimming', `isearchp-some',
;;    `isearchp-thing-read-args', `isearchp-text-prop-present-p',
;;    `isearchp-thing-scan', `isearchp-things-alist',
;;    `isearchp-zones-1', `isearchp-zones-filter-pred',
;;    `isearchp-zone-limits-izones', `isearchp-zones-read-args'.
;;
;;  Internal variables defined here:
;;
;;    `isearchp-anti-zones-invisible-p', `isearchp-dimmed-overlays',
;;    `isearchp-excluded-zones', `isearchp-property-prop',
;;    `isearchp-property-prop-prefix', `isearchp-property-type',
;;    `isearchp-property-values', `isearchp-complement-domain-p',
;;    `isearchp-context-level', `isearchp-filter-predicate-orig',
;;    `isearchp-last-thing-type', `isearchp-zone-limits-function',
;;    `isearchp-zones-invisible-p'.
;;
;;
;;  Keys bound in `isearch-mode-map' here:
;;
;;    `C-t'        `isearchp-property-forward'
;;    `C-M-t'      `isearchp-property-forward-regexp'
;;    `C-M-;'      `isearchp-toggle-ignoring-comments'
;;    `C-M-~'      `isearchp-toggle-complementing-domain'
;;
;;
;;  This file should be loaded *AFTER* loading the standard GNU file
;;  `isearch.el'.  So, in your `~/.emacs' file, do this:
;;
;;  (eval-after-load "isearch" '(require 'isearch-prop))
 
;;(@* "Overview of Features")
;;
;;; Overview of Features ---------------------------------------------
;;
;;  Library `isearch-prop.el' lets you search within contexts.  You
;;  can limit incremental search to a set of zones of buffer text -
;;  search contexts that in effect constitute a multi-region.  These
;;  zones can be defined in various ways, including some ways provided
;;  specially by this library.  You can search zones defined by either
;;  their limits (positions) or text or overlay properties on their
;;  text.  (For the former, you also need library `zones.el'.)
;;
;;  You can put text or overlay properties on zones of text that are
;;  defined by matching a regexp or by corresponding to a type of
;;  THING (e.g. comment, sexp, paragraph).
;;
;;  As one example of searching zones that have a given overlay
;;  property, you can search the matches of a previous Isearch, the
;;  lazy-highlight text.  If you also use library `isearch+.el' then
;;  you can use `M-s h l' during isearch to toggle the automatic
;;  removal of lazy-highlighting.  Toggle it off, to keep the
;;  highlighting from the last search.
;;
;;  While you are searching lazy-highlight zones from a previous
;;  search, if you hit `C-S-SPC' then you start searching the current
;;  search hits.  You can repeat this.  This is a kind of progressive
;;  searching: narrowing of search results.
;;
;;  Another kind of progressive searching of zones of text is
;;  available with `S-SPC'.  It removes zones from among the areas
;;  being searched if the zones do not currently contain a search hit
;;  (lazy highlight).  This means you can search using multiple search
;;  patterns, effectively AND'ing them.  For example, you can search
;;  zones for both `fox' and `hen' by searching for one and then the
;;  other, using `S-SPC' in between.  You need library `zones.el' for
;;  this, and this feature is currently limited to removing zones
;;  defined by their limits, not text or overlay properties.
;;
;;  Features:
;;
;;  * If you use library `zones.el' then you can search a set of
;;    buffer zones that are defined by their limits (markers or
;;    numbers) -- like multiple regions, using commands
;;    `isearchp-zones-forward' and `isearchp-zones-forward-regexp'.
;;    You can use different such zone sets.
;;
;;    Library `zones.el' gives you an easy, interactive way to define
;;    them.  A prefix arg to the commands that search a set of zones
;;    prompts you for a variable whose value is such a set.  By
;;    default the variable is `zz-izones'.
;;
;;    If option `isearchp-query-replace-zones-flag' is non-nil then
;;    replacement commands, such as `query-replace', limit replacement
;;    to the current set of zones (value of the current value of
;;    `zz-izones-var').
;;
;;  * You can search within text-property or overlay-property zones of
;;    the buffer or active region.  Example: search within zones
;;    having a `face' text property with a value of
;;    `font-lock-comment-face' or `font-lock-string-face'.
;;
;;  * The basic commands for searching propertied zones are
;;    `isearchp-property-forward' and
;;    `isearchp-property-forward-regexp', and their backward
;;    counterparts.  By default, you are prompted for the property
;;    type (`text' or `overlay'), the property, and the property
;;    values (e.g., a list of faces, for property `face').  These
;;    commands are bound to `C-t' and `C-M-t', respectively, during
;;    Isearch.
;;
;;  * Besides relying on other code to set `face' and other text
;;    properties for use with `C-t', you can use command
;;    `isearchp-put-prop-on-region' (outside of Isearch) to add a text
;;    property to a zone of text.  By default, it applies the last
;;    property and value whose zones you searched using `C-t', but a
;;    prefix arg lets you specify the property and value to apply.
;;    This gives you an interactive way to set up zones for
;;    text-property search (`C-t').  For property `face', empty input
;;    removes all faces from the region.
;;
;;  * If you use library `zones.el' then you can use command
;;    `isearchp-put-prop-on-region' (outside of Isearch) to add a text
;;    property to a set of zones.
;;
;;  * If you use library `zones.el' then you can make a set of zones,
;;    or their complement (the anti-zones), invisible (or visible, if
;;    they are invisible).  See commands
;;    `isearchp-make-zones-invisible',
;;    `isearchp-make-anti-zones-invisible',
;;    `isearchp-make-anti-zones-visible',
;;    `isearchp-make-zones-visible',
;;    `isearchp-toggle-zone/anti-zone-visibility',
;;    `isearchp-toggle-zones-invisible', and
;;    `isearchp-toggle-anti-zones-invisible'.
;;
;;  * You can use command `isearchp-mark-lazy-highlights' to put
;;    property `isearchp-lazy-highlight' on the text that has lazy
;;    highlighting (an overlay with face `lazy-highlight').  To use
;;    this, you will likely want to first set option
;;    `lazy-highlight-cleanup' to nil, so this highlighting is not
;;    removed when you exit Isearch.  If you use library `isearch+.el'
;;    then you can use `M-s h l' during Isearch to toggle option
;;    `lazy-highlight-cleanup'.  (You can also remove lazy-highlight
;;    highlighting manually anytime using `M-x
;;    lazy-highlight-cleanup'.)
;;
;;  * You can use command `isearchp-lazy-highlights-forward' `(or
;;    `isearchp-lazy-highlights-forward-regexp') to search the zones
;;    of text that have text property `isearchp-lazy-highlight', that
;;    is, the text that you have marked using
;;    `isearchp-mark-lazy-highlights'.  This lets you search within
;;    the hits from a previous search.
;;
;;  * You can use `C-S-SPC' (command
;;    `isearchp-narrow-to-lazy-highlights') to narrow a lazy-highlight
;;    search.  What this means is that while you are searching the
;;    marked lazy-highlight zones, if you hit `C-S-SPC' then the
;;    current lazy-highlight areas (from the current search of the
;;    marked zones) are marked and replace the previously marked
;;    zones.  The effect is that you are now searching only the areas
;;    that were lazy-highlighted before you hit `C-S-SPC'.
;;
;;  * You can search zones of text/overlays that have a given
;;    property, as described above, or you can search the
;;    *COMPLEMENT*: the zones that do *NOT* have a given property.
;;    You can toggle this search-domain complementing at any time
;;    during Isearch, using `C-M-~' (command
;;    `isearchp-toggle-complementing-domain').
;;
;;  * When you search propertied zones, the non-searchable zones are
;;    sometimes dimmed, to make the searchable areas stand out.
;;    Option `isearchp-dim-outside-search-area-flag' controls whether
;;    such dimming occurs.  You can toggle it anytime during Isearch,
;;    using `C-M-D' (aka `C-M-S-d').  Option `isearchp-dimming-color'
;;    defines the dimming behavior.  It specifies a given background
;;    color to use always, or it specifies that the current background
;;    color is to be dimmed a given amount.
;;
;;  * You can use `M-S-delete' to clean up any property-searching
;;    artifacts (properties, dimming), as well as remove any
;;    lazy-highlighting.
;;
;;  * You can search the zones of text that match a given regexp,
;;    using command `isearchp-regexp-context-search' or
;;    `isearchp-regexp-context-regexp-search'.  This is equivalent to
;;    using command `isearchp-regexp-define-contexts', which marks
;;    such zones with a text property, and then using command
;;    `isearchp-property-forward' or
;;    `isearchp-property-forward-regexp' (`C-t' or `C-M-t' during
;;    Isearch, respectively).
;;
;;  * You can search the text of THINGS of various kind (sexps, lists,
;;    defuns, lines, pages, sentences, filenames, strings, comments,
;;    xml/html elements, symbols,...), using command `isearchp-thing'
;;    or `isearchp-thing-regexp'.  This is equivalent to using command
;;    `isearchp-thing-define-contexts', which marks such zones with a
;;    text property, and then using `isearchp-property-forward' or
;;    `isearchp-property-forward-regexp' (`C-t' or `C-M-t' during
;;    Isearch, respectively).
;;
;;  * Not related to searching, but you can also move forward and
;;    backward among things of a given kind, using the repeatable
;;    commands `isearchp-next-visible-thing' and
;;    `isearchp-previous-visible-thing'.  For best results I strongly
;;    recommend that you also use library `thingatpt+.el'.  It
;;    enhances the vanilla treatment of THINGS and fixes various
;;    vanilla thing-at-point bugs.
;;
;;  * You can search the text of Emacs-Lisp definitions of different
;;    kinds, using commands `isearchp-imenu',
;;    `isearchp-imenu-command', `isearchp-imenu-macro', and
;;    `isearchp-imenu-non-interactive-function'.  Since Imenu is based
;;    on regexps that recognize definitions, these commands are based
;;    on the behavior of `isearchp-regexp-context-search'.
;;
;;  * You can remove properties from text or overlays using commands
;;    `isearchp-remove-property' and `isearchp-remove-all-properties'.
;;    By default, the latter removes only properties whose names begin
;;    with `isearchp-'.  These are the properties inserted
;;    automatically by the commands of this library, when you do not
;;    specify a property.
;;
;;  * You can hide or show code comments during Isearch, using `M-;'
;;    (command `isearchp-toggle-hiding-comments').  You can toggle
;;    ignoring comments during Isearch, using `C-M-;' (command
;;    `isearchp-toggle-ignoring-comments').
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2017/11/19 dadams
;;     isearchp-narrow-to-lazy-highlights:
;;       Added isearchp-restore-pred-and-remove-dimming to isearch-mode-end-hook.
;; 2016/11/17 dadams
;;     isearchp-add/remove-dim-overlay: Ensure that OBEG and OEND are numbers before using them.
;; 2016/08/27 dadams
;;     isearchp-thing(-regexp): Added optional arg MSGP (passed to isearchp-thing-define-contexts).
;;     isearchp-thing-read-args: Return also nil for TRANSFORM-FN and non-nil for MSGP.
;;     isearchp-thing-define-contexts: Use only (updated) *-thing-read-args in interactive spec.
;; 2016/08/19 dadams
;;     Added: isearchp-put-prop-on-zones, isearchp-make-zones-invisible,
;;            isearchp-make-anti-zones-invisible, isearchp-make-zones-visible,
;;            isearchp-make-anti-zones-visible, isearchp-toggle-zones-invisible,
;;            isearchp-toggle-anti-zones-invisible, isearchp-anti-zones-invisible-p,
;;            isearchp-zones-invisible-p, isearchp-toggle-zone/anti-zone-visibility.
;;     Soft-require zones.el.
;;     isearchp-put-prop-on-region:
;;       Set isearchp-property-prop, isearchp-property-type, isearchp-property-values, for next use of C-u.
;; 2015/11/08 dadams
;;     Added: isearchp-query-replace-zones-flag.
;;     Advise perform-replace so that it respects isearchp-query-replace-zones-flag.
;; 2015/09/01 dadams
;;     Renamed isearchp-property-finish to isearchp-restore-pred-and-remove-dimming.
;; 2015/08/31 dadams
;;     isearchp-thing-define-contexts: Typo when supplying args: need nil for TRANSFORM and t for MSGP.
;; 2015/08/30 dadams
;;     isearchp-add/remove-dim-overlay: Changed test for removal from <= to <.
;; 2015/08/28 dadams
;;     Added: isearchp-excluded-zones, isearchp-zone-limits-function, isearchp-narrow-to-matching-zones,
;;            isearchp-exclude-zones-w-no-lazy-highlight.
;;     Renamed isearchp-lazy-highlights-narrow to isearchp-narrow-to-lazy-highlights.
;;     Bind S-SPC to isearchp-narrow-to-matching-zones and C-S-SPC to isearchp-lazy-highlights-narrow.
;;     isearchp-remove-dimming: Reset isearchp-excluded-zones to nil.
;;     isearchp-zones-1: Apply zz-izone-limits to izones (isearchp-zones-filter-pred now takes basic zones).
;;     isearchp-zones-filter-pred: The arg is now a list of basic zones, not izones.
;; 2015/08/27 dadams
;;     isearchp-zones-filter-pred: Bind limits of IZONES using lexical-let, for use in fn passed to zz-some.
;;     isearchp-zones-1: Pass value of var, not var, to isearchp-zones-filter-pred.
;; 2015/08/24 dadams
;;     isearchp-complement-dimming, isearchp-add/remove-dim-overlay:
;;       When delete overlay, remove it from isearchp-dimmed-overlays.
;; 2015/08/23 dadams
;;     isearchp-add/remove-dim-overlay: Iterate over isearchp-dimmed-overlays instead of from BEG to END.
;; 2015/08/16 dadams
;;     Renamed wide-n.el stuff to zones.el stuff.  Require only (the new) zones.el.
;; 2015/08/14 dadams
;;     isearchp-zones-1, isearchp-zones-filter-pred: pass non-nil ONLY-ONE-BUFFER-P arg to wide-n-limits.
;; 2015/08/09 dadams
;;     Added Search-zones functions:
;;       isearchp-zones-forward, isearchp-zones-forward-regexp, isearchp-zones-backward, isearchp-zones-1,
;;       isearchp-zones-backward-regexp, isearchp-zones-read-args, isearchp-zones-filter-pred.
;;     Renamed: isearchp-toggle-dimming-non-prop-zones to isearchp-toggle-dimming-outside-search-area,
;;              isearchp-dim-non-prop-zones-flag       to isearchp-dim-outside-search-area-flag.
;;     Changed compatibility in file header from 23.x to 23.2+, for with-silent-modifications.
;; 2015/08/01 dadams
;;     Added: isearchp-hide-whitespace-before-comment-flag.
;;     isearchp-hide/show-comments:
;;       Updated per hide-comnt.el, including respect isearchp-hide-whitespace-before-comment-flag.
;; 2015/07/31 dadams
;;     Added: isearchp-lazy-highlights-forward-regexp, isearchp-lazy-highlights-forward-1,
;;            isearchp-lazy-highlights-narrow.
;;     isearchp-lazy-highlights-forward: Use isearchp-lazy-highlights-forward-1.
;;                                       (Fixes, restore read-only & mod etc.)
;;     isearchp-cleanup: Added optional arg MSGP.
;;     Bind isearchp-cleanup to M-S-delete.  Bind isearchp-lazy-highlights-narrow to S-SPC.
;;     isearchp-remove-property, isearchp-remove-all-properties, isearchp-cleanup,
;;       isearchp-put-prop-on-region, isearchp-lazy-highlights-forward-1,
;;       isearchp-add-prop-to-other-prop-zones, isearchp-regexp-scan, isearchp-hide/show-comments,
;;       isearchp-thing-scan: Use with-silent-modifications to allow text-prop mods without raising errors.
;; 2015/07/29 dadams
;;     Added: isearchp-lazy-highlights-present-p.
;;     isearchp-lazy-highlights-forward:
;;       Added args BEG, END, RESTARTP.
;;       Raise error if not isearchp-lazy-highlights-present-p.
;;       With prefix arg or if no marked lazy-highlights, call isearchp-mark-lazy-highlights to mark them.
;;     isearchp-property-matches-p: Corrected to loop over all overlays at point, not just use the first.
;;     isearchp-thing-read-args: Turn off *-ignore-comments-flag if THING = comment and not complementing.
;;     isearchp-thing: Updated doc string to mention this.
;;     isearchp-hide/show-comments: No-op if no comment-start.  Pass NOERROR arg to comment-normalize-vars.
;; 2015/07/28 dadams
;;     isearchp-thing-scan: Revert hopeful code that expected bug #9300 to be fixed in Emacs 24.
;;                          You really need library thingatpt+.el if you want reasonable behavior.
;;     isearchp-add-prop-to-other-prop-zones:
;;       Moved final call to isearchp-add/remove-dim-overlay to ADD outside of condition-case-no-debug.
;;     Moved defmacro forward in file.
;; 2015/07/20 dadams
;;     isearchp-add-prop-to-other-prop-zones: Added missing error clause to condition-case-no-debug.
;; 2015/07/19 dadams
;;     Added: isearchp-cleanup, isearchp-lazy-highlights-forward, isearchp-add-prop-to-lazy-highlights,
;;            isearchp-add-prop-to-other-prop-zones, isearchp-mark-lazy-highlights.
;; 2015/07/18 dadams
;;     isearchp-property-(forward|backward)(-regexp), isearchp-regexp-context-regexp-search:
;;       Initialize isearch-forward, isearch-regexp, so initial message is accurate.
;;     isearchp-property-1:
;;       Wrap call to isearch-done in ignore-errors (Emacs bug #21091 workaround).
;;       Simplify initial msg, and reinitialize some vars for it:
;;         isearch-word, isearch-success, isearch-wrapped, isearch-adjusted.
;;     isearchp-remove-property: Ensure TYP is non-nil before passing it to intern.
;; 2015/04/12 dadams
;;     Added: isearchp-remove-dimming, isearchp-regexp-context-regexp-search, isearchp-thing-regexp.
;;     isearchp-property-finish: Use isearchp-remove-dimming (its code was factored out from here).
;;     isearchp-regexp-read-args: Return also nil for ACTION arg.
;;     isearchp-regexp-context-search: Test isearchp-property-prop vs _IGNORED, not vs interned REGEXP.
;;     isearchp-imenu*: Added optional arg REGEXP-P.
;; 2013/12/26 dadams
;;     isearchp-hide/show-comments: Updated from hide/show-comments in hide-comnts.el.
;; 2013/10/09 dadams
;;     Added: isearchp-toggle-hiding-comments (same as hide/show-comments-toggle in hide-comnt.el).
;;            Bind it, not isearchp-hide/show-comments, to M-;.
;;     isearchp-hide/show-comments: Use comment-end-skip, not comment-end.
;; 2013/10/02 dadams
;;     isearchp-next-visible-thing-1: Put back <=, not <, for comparison.  See comment.
;;     Soft-require thingatpt+.el.
;;     isearchp-defined-thing-p, isearchp-things-alist: defalias to thgcmd-* version if defined.
;; 2013/10/01 dadams
;;     Added: isearchp-dimming-color, isearchp-dim-face-spec, isearchp-dim-color.
;;     Removed: face isearchp-dimmed.
;;     Require color.el, when available.
;;     isearchp-add/remove-dim-overlay: Use isearchp-dim-color, not face isearchp-dimmed.
;; 2013/09/30 dadams
;;     Added: isearchp-dimmed-overlays, isearchp-dim-non-prop-zones-flag, face isearchp-dimmed,
;;       isearchp-complement-dimming, isearchp-toggle-dimming-non-prop-zones,
;;       isearchp-add/remove-dim-overlay.
;;     Renamed: isearchp-property-end to isearchp-property-finish.
;;     Soft-require hexrgb.el.
;;     Bind isearchp-toggle-dimming-non-prop-zones to C-M-S-d.
;;     isearchp-toggle-complementing-domain, isearchp-regexp-define-contexts,
;;       isearchp-toggle-ignoring-comments, isearchp-thing-define-contexts:
;;         Added optional MSGP arg.
;;     isearchp-property-finish: Delete overlays in isearchp-dimmed-overlays, and empty it.
;;     isearchp-(regexp|thing)-scan, isearchp-put-prop-on-region: Call isearchp-add/remove-dim-overlay.
;; 2013/09/27 dadams
;;     isearchp-property-1: Do not deactivate-mark (done in isearch-mode of isearch+.el).
;;     isearchp-property-filter-pred: Ensure that BEG, END are within isearchp-reg-(beg|end).
;;     isearchp-(thing|regexp)-scan:
;;       Do not handle isearchp-complement-domain-p here.  Done in isearchp-property-filter-pred.
;;     isearchp-thing-scan: Move set-buffer-modified-p outside isearchp-with-comments-hidden.
;;     isearchp-regexp-scan:
;;       Apply ACTION only if there is a match.
;;       Remove PROPERTY from text between matches.
;;       Move to updated LAST-BEG at each loop iteration.
;;       Bind prop-value outside loop.
;;     isearchp-remove-(property|all-properties), isearchp-put-prop-on-region, isearchp-regexp-scan,
;;       isearchp-thing-scan:
;;         Bind buffer-read-only to nil.
;; 2013/09/22 dadams
;;     isearchp-properties-in-buffer: Fixed condition for adding prop to list for overlays.
;; 2013/09/21 dadams
;;     Added: isearchp-remove-all-properties, isearchp-remove-property, isearchp-property-prop-prefix.
;;     Everywhere: Reversed prefix arg: none now prompts for input, with prefix arg reuse last.
;;     isearchp-properties-in-buffer: Added PREDICATE arg.
;;     isearchp-read-sexps: Ignore errors when reading.
;;     isearchp-(regexp|thing)-scan: Restore bufmodp properly.  Do not add extra isearchp property.
;;     isearchp-thing-scan: Fixed scope for last-beg.  Do not put property on empty scan hit.
;;     isearchp-thing-read-args: Use isearchp-property-prop-prefix.
;;     *-add-regexp-as-property, *-regexp-context-search, *-(regexp|thing)-read-args:
;;       Use isearchp-property-prop-prefix as init value.
;;     isearchp-imenu-1: Removed first arg.  Pass (intern regexp) as prop to *-regexp-context-search.
;;     Use string-match-p, not string-match.
;; 2013/09/18 dadams
;;     Renamed *-char-prop-* to *-property-*, *-char-properties to *-properties.
;;     isearchp-property-matches-p: Corrected overlays-only check.
;; 2013/09/17 dadams
;;     isearchp-regexp-scan: If complementing, then PREDICATE must *not* be satisfied.
;;                           Moved saving of buffer-modified-p earlier.
;;     isearchp-thing-scan: Save & restore buffer-modified-p.
;; 2013/09/10 dadams
;;     isearchp-regexp-scan, isearchp-thing-scan: Added text property isearchp.
;;     Bound isearchp-hide/show-comments to M-; in isearch-mode-map.
;;     Forgot to require cl.el at compile time, for case.
;; 2013/09/09 dadams
;;     Added: isearchp-toggle-complementing-domain, isearchp-context-level,
;;            isearchp-complement-domain-p, isearchp-read-context-regexp, isearchp-regexp-scan,
;;            isearchp-regexp-define-contexts, isearchp-regexp-context-search,
;;            isearchp-add-regexp-as-property, isearchp-regexp-read-args, isearchp-text-prop-present-p,
;;            isearchp-ignore-comments-flag, isearchp-hide/show-comments, isearchp-with-comments-hidden,
;;            isearchp-toggle-ignoring-comments, isearchp-last-thing-type, isearchp-thing,
;;            isearchp-thing-read-args, isearchp-thing-define-contexts, isearchp-thing-scan,
;;            isearchp-things-alist, isearchp-defined-thing-p, isearchp-bounds-of-thing-at-point,
;;            isearchp-next-visible-thing-and-bounds, isearchp-previous-visible-thing,
;;            isearchp-next-visible-thing(-1|-2).
;;     isearchp-char-prop-1: Handle complementing. Deactivate region when done.
;;     isearchp-char-prop-filter-pred: Handle complementing.
;;     isearchp-char-prop-forward: Update doc to better describe behavior with no prefix arg.
;;     Bound isearchp-toggle-ignoring-comments to C-M-;, isearchp-toggle-complementing-domain' to C-M-~.
;; 2013/09/08 dadams
;;     Created from code factored out of isearch+.el.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:


(eval-when-compile (require 'cl)) ;; case

(if (fboundp 'color-name-to-rgb)
    (require 'color)                    ; Emacs 24+
  (require 'hexrgb nil t)) ;; (no error if not found): hexrgb-increment-value

(require 'thingatpt+ nil t) ;; (no error if not found): tap-bounds-of-thing-at-point
(require 'zones nil t) ;; (no error if not found):
 ;; zz-izone-limits, zz-izones-var, zz-read-any-variable, zz-some, zz-zones-complement, zz-zone-union

;; Quiet the byte-compiler.
(defvar isearchp-reg-beg) ;; In `isearch+.el'.
(defvar isearchp-reg-end) ;; In `isearch+.el'.
(defvar comment-end-skip) ;; In `newcomment.el' (Emacs 24+).
 
;;(@* "Macros")

;;; Macros ----------------------------------------------

;; Same as `with-comments-hidden' in `hide-comnt.el', except doc here mentions `C-M-;'.
;;
(defmacro isearchp-with-comments-hidden (start end &rest body)
  "Evaluate the forms in BODY while comments are hidden from START to END.
But if `isearchp-ignore-comments-flag' is nil, just evaluate BODY,
without hiding comments.  Show comments again when BODY is finished.
You can toggle `isearchp-ignore-comments-flag' using `C-M-;' in the
minibuffer, but depending on when you do so you might need to invoke
the current command again.

See `isearchp-hide/show-comments', which is used to hide and show the
comments."
  (let ((result  (make-symbol "result"))
        (ostart  (make-symbol "ostart"))
        (oend    (make-symbol "oend")))
    `(let ((,ostart  ,start)
           (,oend    ,end)
           ,result)
       (unwind-protect
            (setq ,result  (progn (when isearchp-ignore-comments-flag
                                    (isearchp-hide/show-comments 'hide ,ostart ,oend))
                                  ,@body))
         (when isearchp-ignore-comments-flag (isearchp-hide/show-comments 'show ,ostart ,oend))
         ,result))))
 
;;(@* "Options")

;;; Options ----------------------------------------------------------

(defcustom isearchp-dimming-color (if (or (fboundp 'color-name-to-rgb)  (featurep 'hexrgb))
                                      -0.10
                                    "#9A6BC0CDCD4C")
  "*Background color to use for dimmed zones of text.
The value can be a color name or a hex RGB code that starts with `#'.

Or it can be a number between -1.0 and 1.0, which is used to calculate
the background color by tweaking the current background color..  A
negative option value dims; a positive value brightens.  The larger
the absolute value, the greater the difference from the current color.

* If you have library `color.el' (in Emacs 24+), this tweaking uses a
  combination of `color-lighten-name' and `color-saturate-name'.

* If not, but you have library `hexrgb.el', it uses
  `hexrgb-increment-value'.

* If you have neither `hexrgb.el' nor `color.el', a numeric value has
  no effect."
  :group 'isearch-plus :type '(choice
                               (color  :tag "Shading color"  :value "#9A6BC0CDCD4C")
                               (number :tag "Dimming factor" :value -0.10)))

(define-obsolete-variable-alias 'isearchp-dim-non-prop-zones-flag ; $$$$$$ Remove alias later.
 'isearchp-dim-outside-search-area-flag "2015-08-10")
(defcustom isearchp-dim-outside-search-area-flag t
  "*Non-nil means dim text that does not have the property being searched.
More precisely, if `isearchp-complement-domain-p' is non-nil then the
text being searched does not have the property, and this then dims the
text that does have the property."
  :type 'boolean :group 'isearch-plus
  :initialize 'custom-initialize-changed
  :set (lambda (symb new-val)
         (let ((old-val  (symbol-value symb)))
           (custom-set-default symb new-val)
           (unless (eq old-val (symbol-value symb)) (isearchp-toggle-dimming-outside-search-area)))))

;; Same as `hide-whitespace-before-comment-flag' in `hide-comnt.el'.
;; Same as `icicle-hide-whitespace-before-comment-flag' in `icicles-opt.el'.
;; Except that those libraries need to also handle Emacs < 23.
;;
(defcustom isearchp-hide-whitespace-before-comment-flag t
  "*Non-nil means hide whitespace preceding a comment.
Empty lines (newline chars) are not hidden, however.
Used by `isearchp-hide/show-comments'."
  :type 'boolean :group 'isearch-plus)

;; Same as `ignore-comments-flag' in `hide-comnt.el'.
;;
(defcustom isearchp-ignore-comments-flag t
  "*Non-nil means `isearchp-with-comments-hidden' hides comments.
This has the effect that comments are ignored for searching."
  :type 'boolean :group 'isearch-plus)

(defcustom isearchp-query-replace-zones-flag nil
  "Non-nil means limit query-replacing to the zones of `zz-izones-var'.
This option has no effect if you do not use library `zones.el'."
  :type 'boolean :group 'isearch-plus)
 
;;(@* "Internal Variables")

;;; Internal Variables -----------------------------------------------

(defvar isearchp-anti-zones-invisible-p nil
  "Non-nil means the complement of `zz-izones-var' is currently invisible.")
(make-variable-buffer-local 'isearchp-anti-zones-invisible-p)

(defvar isearchp-complement-domain-p nil
  "Non-nil means complement the initial search candidates wrt the buffer.
This has an effect only on (some) Isearch+ commands.  The scan
function or regexp for the search command defines a set of matches in
the buffer.

If the variable value is non-nil then the actual candidates used are
the sections of region or buffer text that are separated by the
initial candidates, that is, the non-candidates as defined by the scan
or regexp.")

(defvar isearchp-context-level 0
  "Match level for Isearch context regexp.
0 means use whatever matches the whole context regexp as the search
context.  1 means use whatever matches the first subgroup of the
regexp as the search context, and so on.")

(defvar isearchp-dimmed-overlays () "Dimmed-text overlays for text not being searched.")

(defvar isearchp-excluded-zones ()
  "Buffer zones currently excluded from Isearch.")
(make-variable-buffer-local 'isearchp-excluded-zones)

(defvar isearchp-filter-predicate-orig nil
  "Original value of `isearch-filter-predicate'.")

;;; $$$$$$ (defvar isearchp-last-prop+value nil "Last Isearch+ property added.")

;;; Same as `thgcmd-last-thing-type' in `thing-cmds.el'.
(defvar isearchp-last-thing-type (if (boundp 'thgcmd-last-thing-type) thgcmd-last-thing-type 'sexp)
  "Type of thing last used by `isearchp-next-visible-thing' (or previous).")

(defvar isearchp-property-prop nil
  "Last property used for `isearchp-property-*' commands.")

(defvar isearchp-property-prop-prefix "isearchp-"
  "Prefix for Isearch property names.")

(defvar isearchp-property-type nil
  "Last property type used for `isearchp-property-*' commands.
Possible values are `text', `overlay', and nil, meaning both.")

(defvar isearchp-property-values nil
  "Last property values used for `isearchp-property-*' commands.")

(defvar isearchp-zones-invisible-p nil
  "Non-nil means zones in `zz-izones-var' are currently invisible.")
(make-variable-buffer-local 'isearchp-zones-invisible-p)

(defvar isearchp-zone-limits-function 'isearchp-zone-limits-izones
  "Function used to return limits of current search zones.")
 
;;(@* "Keys")

;;; Keys -------------------------------------------------------------

(define-key isearch-mode-map (kbd "C-t")          'isearchp-property-forward)
(define-key isearch-mode-map (kbd "M-;")          'isearchp-toggle-hiding-comments)
(define-key isearch-mode-map (kbd "C-M-t")        'isearchp-property-forward-regexp)
(define-key isearch-mode-map (kbd "C-M-;")        'isearchp-toggle-ignoring-comments)
(define-key isearch-mode-map (kbd "C-M-~")        'isearchp-toggle-complementing-domain)
(define-key isearch-mode-map (kbd "C-M-S-d")      'isearchp-toggle-dimming-outside-search-area)
(define-key isearch-mode-map (kbd "M-S-<delete>") 'isearchp-cleanup)
(when (fboundp 'isearchp-narrow-to-matching-zones)
  (define-key isearch-mode-map (kbd "S-SPC")      'isearchp-narrow-to-matching-zones))
(define-key isearch-mode-map (kbd "C-S-SPC")      'isearchp-narrow-to-lazy-highlights)
 
;;(@* "General Search-Property Commands")

;;; General Search-Property Commands ---------------------------------

(defun isearchp-remove-property (beg end property &optional type predicate msgp)
  "Remove PROPERTY from the active region or buffer.
If PROPERTY was the last property used for a property search, then
forget that it was used.

You are prompted for PROPERTY.  By default, PROPERTY is removed from
both text and overlays.  With a prefix arg you are also prompted for
the type to remove: text, overlay, or both.

Non-interactively:
 TYPE = nil means both text and overlays.
 Non-nil PREDICATE is a predicate that each property must satisfy."
  (interactive
   (let ((beg1  (if (and transient-mark-mode  mark-active) (region-beginning) (point-min)))
         (end1  (if (and transient-mark-mode  mark-active) (region-end) (point-max)))
         (typ   (and current-prefix-arg
                     (completing-read  "Type: " '(("text") ("overlay") ("text and overlay"))
                                       nil t nil nil "text and overlay"))))
     (setq typ  (and typ  (not (equal typ "text and overlay"))  (intern typ)))
     (list beg1
           end1
           (let ((prop  (completing-read
                         "Property: " (mapcar #'(lambda (prop) (list (symbol-name prop)))
                                              (isearchp-properties-in-buffer
                                               (current-buffer) beg1 end1 typ))
                         nil t nil nil ; Must pick a property that is present.
                         (and isearchp-property-prop  (symbol-name isearchp-property-prop)))))
             (when (equal "" prop) (error "No property chosen"))
             (intern prop))
           (if (not typ) '(text overlay) (list typ))
           nil
           'MSGP)))
  (unless type (setq type  '(text overlay)))
  (let ((removedp  nil))
    (with-silent-modifications
      (when (memq 'text type) (setq removedp  (remove-text-properties beg end (list property))))
      (when (memq 'overlay type) (while (< beg end)
                                   (dolist (ov  (overlays-at beg))
                                     (when (overlay-get ov property)
                                       (delete-overlay ov)
                                       (setq removedp  t)))
                                   (setq beg  (1+ beg)))))
    (when msgp (message (if removedp "Property removed" "Property NOT removed"))))
  (when (eq isearchp-property-prop property) (setq isearchp-property-prop    nil
                                                   isearchp-property-values  nil)))

(defun isearchp-remove-all-properties (beg end &optional type predicate msgp)
  "Remove all properties from the active region or buffer.
With a non-negative prefix arg you are prompted for the type of
properties to remove: text, overlay, or both.

By default, remove only Isearch+ properties, which are those whose
names begin with `isearchp-'.  But with a non-positive prefix arg all
properties present in the active region or buffer are candidates for
removal.  In this case, you are prompted for a predicate that each
property to be removed must satisfy.  Empty input at the prompt means
no filtering: *all* properties are removed unconditionally.

Non-interactively:
 TYPE = nil means both text and overlays.
 Non-nil PREDICATE is a predicate that each property must satisfy,
  instead of having to be an Isearch property."
  (interactive
   (let ((beg1  (if (and transient-mark-mode  mark-active) (region-beginning) (point-min)))
         (end1  (if (and transient-mark-mode  mark-active) (region-end) (point-max)))
         (typ   (and current-prefix-arg  (wholenump (prefix-numeric-value current-prefix-arg))
                     (completing-read  "Type: " '(("text") ("overlay") ("text and overlay"))
                                       nil t nil nil "text and overlay"))))
     (setq typ  (and typ  (not (equal typ "text and overlay"))  (intern typ)))
     (list beg1 end1 (if (not typ) '(text overlay) (list typ))
           (if (and current-prefix-arg  (<= (prefix-numeric-value current-prefix-arg) 0))
               (read-from-minibuffer "Remove properties satisfying predicate: "
                                     nil read-expression-map t (and (boundp 'function-name-history)
                                                                    'function-name-history)
                                     "nil")
             (lambda (prop) (string-match-p "\\`isearchp-" (symbol-name prop))))
           'MSGP)))
  (unless type (setq type  '(text overlay)))
  (let ((removedp  nil)
        beg2)
    (with-silent-modifications
      (dolist (prop  (isearchp-properties-in-buffer (current-buffer) beg end (if (cadr type) nil type)
                                                    predicate))
        (when (memq 'text type)  (let ((removd  (remove-text-properties beg end (list prop 'IGNORE))))
                                   (setq removedp  (or removedp  removd))))
        (setq beg2  beg)
        (when (memq 'overlay type) (while (< beg2 end)
                                     (dolist (ov  (overlays-at beg2))
                                       (when (overlay-get ov prop)
                                         (delete-overlay ov)
                                         (setq removedp  t)))
                                     (setq beg2  (1+ beg2))))
        (when (eq isearchp-property-prop prop) (setq isearchp-property-prop    nil
                                                     isearchp-property-values  nil))))
    (when msgp (message (if removedp "Properties removed" "Properties NOT removed")))))

(defun isearchp-remove-dimming ()
  "Remove any text dimming that was applied by `isearchp-*' functions."
  (interactive)
  (while isearchp-dimmed-overlays
    (delete-overlay (car isearchp-dimmed-overlays))
    (setq isearchp-dimmed-overlays  (cdr isearchp-dimmed-overlays)))
  (setq isearchp-excluded-zones  ()))

(defun isearchp-cleanup (&optional msgp)
  "Remove lazy-highlighting and artifacts from property searching.
This includes dimming and all `isearchp-' properties.
Bound to `\\<isearch-mode-map>\\[isearchp-cleanup]' during Isearch."
  (interactive "p")
  (with-silent-modifications
    (isearchp-remove-all-properties (point-min) (point-max))
    (isearchp-remove-dimming)
    (lazy-highlight-cleanup 'FORCE))
  (when msgp (message "Removed lazy-highlighting and property-search artifacts")))

(defun isearchp-property-forward (arg) ; Bound to `C-t' in `isearch-mode-map'.
  "Isearch forward in text with a text property or overlay property.
That is, move to the next such property and search within it for text
matching your input.

If `isearchp-complement-domain-p' is non-nil then move to the next
zone that does *not* have the given property.  (Use `C-M-~' during
Isearch to toggle this variable.)  For example, this lets you search
for text that is NOT displayed using a certain face or combination of
faces.

With no prefix argument, or if you have not previously used an Isearch
command that searches properties or applies them, you are prompted for
the following:

 * the property type (`text', `overlay', or `text and overlay')
 * the property (e.g., `face', `mumamo-major-mode')
 * the property values (e.g., a list of faces, for property `face')

If you have previously used such an Isearch property command, then a
prefix arg means reuse the property type, property, and value from the
last Isearch property command.  This includes `isearchp-property-*'
commands and commands such as `isearchp-imenu*',
`isearchp-thing(-regexp)', `isearchp-regexp-context-search', and
`isearchp-put-prop-on-region'.

Note that this means that you can simply use `C-u C-t' during ordinary
Isearch in order to repeat the last property search.

The particular prefix arg controls the behavior as follows:

 * Plain `C-u': Reuse the last property type (overlay, text, or both).
 * Positive prefix arg (e.g., `C-9'): Search only text properties.
 * Negative prefix arg (e.g., `C--'): Search only overlay properties.
 * Zero prefix arg (`C-0'): Search both text and overlay properties.

By default, an actual value of the property matches the value
you specify if it is `equal'.  Properties `mumamo-major-mode' and
`face' (or `font-lock-face') are exceptions.

For `mumamo-major-mode', you specify the major mode whose zones of
text you want to search.  The actual property value is a list whose
car is the major mode symbol.

For properties `face' and `font-lock-face', you can pick multiple
faces, using completion (hit `RET' with empty input to finish
choosing).  Text is searched that has a face property that includes
any of the faces you choose.  If you choose no face (empty input at
the outset), then text with any face at all is searched.

NOTE: If you search zones of property `face' and the property values
      include `font-lock' faces, then you might want to first make
      sure the entire buffer has been fontified.  You can do that
      using command `isearchp-fontify-buffer-now'.

NOTE: This command is available during normal Isearch, on key `C-t'.
      However, in order to be able to use a prefix arg within Isearch,
      you must set `isearch-allow-scroll' or `isearch-allow-prefix'
      (if available) to non-nil.  Otherwise, a prefix arg exits
      Isearch."
  (interactive "P")
  (setq isearch-forward  t)
  (isearchp-property-1 'isearch-forward arg))

(defun isearchp-property-backward (arg)
  "Isearch backward in text with a text or overlay property.
See `isearchp-property-forward'."
  (interactive "P")
  (setq isearch-forward  nil)
  (isearchp-property-1 'isearch-backward arg))

(defun isearchp-property-forward-regexp (arg) ; Bound to `C-M-t' in `isearch-mode-map'.
  "Regexp Isearch forward in text with a text or overlay property.
NOTE: This command is available during normal Isearch, on key `C-M-t'.
      However, in order to be able to use a prefix arg within Isearch,
      you must set `isearch-allow-scroll' or `isearch-allow-prefix'
      (if available) to non-nil.  Otherwise, a prefix arg exits
      Isearch.
See `isearchp-property-forward'."
  (interactive "P")
  (setq isearch-regexp   t
        isearch-forward  t)
  (isearchp-property-1 'isearch-forward-regexp arg))

(defun isearchp-property-backward-regexp (arg)
  "Regexp Isearch backward in text with a text or overlay property.
See `isearchp-property-backward'."
  (interactive "P")
  (setq isearch-regexp   t
        isearch-forward  nil)
  (isearchp-property-1 'isearch-backward-regexp arg))

(defun isearchp-toggle-complementing-domain (&optional msgp) ; Bound to `C-M-~' during Isearch.
  "Toggle searching the complements of the normal search contexts.
This toggles internal variable `isearchp-complement-domain-p'.
Bound to `\\<isearch-mode-map>\\[isearchp-toggle-complementing-domain]' during Isearch."
  (interactive "p")
  (setq isearchp-complement-domain-p  (not isearchp-complement-domain-p))
  (when isearchp-dim-outside-search-area-flag (isearchp-complement-dimming))
  (when msgp (message "%s the search domain now" (if isearchp-complement-domain-p
                                                     "*COMPLEMENTING*"
                                                   "*NOT* complementing"))))

(defun isearchp-complement-dimming ()
  "Complement which areas are dimmed."
  (let* ((beg      (or (and (boundp 'isearchp-reg-beg)  isearchp-reg-beg)  (point-min)))
         (end      (or (and (boundp 'isearchp-reg-end)  isearchp-reg-end)  (point-max)))
         (pos      beg)
         (ov-lims  ())
         (new-ovs  ())
         new-ov ovb ove oprops oprops1)
    (setq isearchp-dimmed-overlays  (sort isearchp-dimmed-overlays
                                          (lambda (ov1 ov2)
                                            (and (overlay-start ov1)  (overlay-start ov2)
                                                 (< (overlay-start ov1) (overlay-start ov2))))))
    (dolist (ov  isearchp-dimmed-overlays)
      (setq ovb     (overlay-start ov)
            ove     (overlay-end ov)
            oprops  (overlay-properties ov))
      (delete-overlay ov)
      (setq isearchp-dimmed-overlays  (delete ov isearchp-dimmed-overlays))
      (when ovb (push ovb ov-lims))
      (when ove (push ove ov-lims)))
    (setq ov-lims  (nreverse ov-lims))
    (when (and (car ov-lims)  (< beg (car ov-lims)))
      (push (setq new-ov  (make-overlay beg (car ov-lims))) new-ovs)
      (setq oprops1  oprops)
      (while oprops1 (overlay-put new-ov (pop oprops1) (pop oprops1))))
    (when (and (car ov-lims)  (< (setq ove  (car (last ov-lims))) end))
      (push (setq new-ov  (make-overlay ove end)) new-ovs)
      (setq oprops1  oprops)
      (while oprops1 (overlay-put new-ov (pop oprops1) (pop oprops1))))
    (setq ov-lims  (butlast ov-lims))   ; Drop the last end pos.
    (while (setq ov-lims  (cdr ov-lims)) ; Drop the first beg pos, to start with.
      (unless (= (car ov-lims) (cadr ov-lims))
        (push (setq new-ov  (make-overlay (car ov-lims) (cadr ov-lims))) new-ovs)
        (setq oprops1  oprops)
        (while oprops1 (overlay-put new-ov (pop oprops1) (pop oprops1))))
      (setq ov-lims  (cdr ov-lims)))
    (setq isearchp-dimmed-overlays  new-ovs)
    (isearch-lazy-highlight-update)))

(define-obsolete-function-alias 'isearchp-toggle-dimming-non-prop-zones ; $$$$$$ Remove alias later.
    'isearchp-dim-outside-search-area-flag "2015-08-10")
(defun isearchp-toggle-dimming-outside-search-area (&optional msgp) ; Bound to `C-M-D' during Isearch.
  "Toggle dimming text that is outside the search area.
More precisely, toggle option `isearchp-dim-outside-search-area-flag',
then update dimming.  This updating applies to the searchable area:
the region that was active before Isearch started, or the whole buffer
if the region was not active.

Dimming is per option `isearchp-dimming-color'.
Bound to `\\<isearch-mode-map>\\[isearchp-toggle-dimming-outside-search-area]' during Isearch."
  (interactive "p")
  (setq isearchp-dim-outside-search-area-flag  (not isearchp-dim-outside-search-area-flag))
  (let ((face-spec  (isearchp-dim-face-spec)))
    (dolist (ov  isearchp-dimmed-overlays) (overlay-put ov 'face face-spec)))
  (when msgp (message "%s the zones not being searched now" (if isearchp-dim-outside-search-area-flag
                                                                "*DIMMING*"
                                                              "*NOT* dimming"))))

(defun isearchp-dim-face-spec ()
  "Return `(:background COLOR)', where COLOR is the background color dimmed."
  (let ((dim-color  (if (stringp isearchp-dimming-color)
                        isearchp-dimming-color
                      (isearchp-dim-color (face-background 'default) isearchp-dimming-color))))
    (and isearchp-dim-outside-search-area-flag  (list :background dim-color))))

(declare-function color-saturate-name "color.el" '(name percent))

(defun isearchp-dim-color (color number)
  "Return COLOR, dimmed by NUMBER.
COLOR is a color name or an RGB hexadecimal color triplet.
NUMBER is limited to be between -1.0 and 1.0.  A negative NUMBER
 decreases the brightness, dimming.  A positive NUMBER increases the
 brightness, brightening.

* If you have library `color.el' (in Emacs 24+), this uses a
  combination of `color-lighten-name' and `color-saturate-name'.

* If not, but you have library `hexrgb.el', this uses
  `hexrgb-increment-value'.

* If you have neither `hexrgb.el' nor `color.el', this is a no-op:
  COLOR is returned unchanged."
  (setq number  (min  1.0 number)
        number  (max -1.0 number))
  (if (fboundp 'color-lighten-name)
      (let* ((new  (color-saturate-name color (* 220.0 number))) ; Emacs 24+, `color.el'.
             (new  (color-lighten-name new (* 88.7 number))))
        new)
    (if (fboundp 'hexrgb-increment-value)
        (hexrgb-increment-value color number)
      color)))                       ; < Emacs 24.  Punt - cannot dim.

(defun isearchp-add-regexp-as-property (property regexp &optional beg end predicate action msgp)
  "Add PROPERTY with value (REGEXP . PREDICATE) to REGEXP matches.
If region is active, limit action to region.  Else, use whole buffer.
If REGEXP has subgroups, then use what the Nth subgroup matches as the
 search context (hit), where N = `isearchp-context-level'.
 If N=0, then use the overall match of REGEXP as the search context.
PROPERTY is a text property to add to the search-hit text.
 the value of the property is REGEXP (a string).
 Interactively, PREDICATE is nil, so the PROPERTY value is (REGEXP).

Non-interactively:

BEG, END are the region limits.  If nil, the buffer limits are used.
PREDICATE is nil or a boolean function that takes these arguments:
  - the search-context string
  - a marker at the end of the search-context
If PREDICATE is non-nil then act on only the hits for which it holds.
If ACTION is non-nil then it is a function that accepts no arguments.
 It is called after matching buffer text with REGEXP.  After ACTION,
 the search hit end position is extended or restricted to point.
Non-interactively, non-nil BEG and END are used as the region limits."
  (interactive (list (intern (read-string "Text property: " isearchp-property-prop-prefix
                                          'isearchp-property-history isearchp-property-prop-prefix))
                     (isearchp-read-context-regexp)
                     (if (and transient-mark-mode  mark-active) (region-beginning) (point-min))
                     (if (and transient-mark-mode  mark-active) (region-end) (point-max))
                     nil
                     nil
                     'MSGP))
  (let ((prop-value  (isearchp-regexp-scan beg end property regexp predicate action)))
    (when msgp
      (if prop-value
          (message "Prop value added: `%s'" prop-value)
        (message "No property added - no match for %s" regexp)))))

(defun isearchp-regexp-context-search (reuse beg end _ignored regexp &optional predicate action)
  "Search within search contexts that are defined by a regexp.
If `isearchp-complement-domain-p' is non-nil then search *outside* the
contexts defined by the regexp.  (Use `C-M-~' during Isearch to toggle
this variable.

Search is limited to the region if it is active.

If you do not use a prefix arg (or non-interactively if argument REUSE
is nil), or you have not previously used this command or similar
commands, then you are prompted for the following:

 * the context-defining regexp
 * a predicate (Boolean function) that takes these arguments:
  - the search-hit string (what matches the regexp or chosen subgroup)
  - a marker at the end of the search-context

Only the search hits for which the predicate is satisfied are
retained.

If the regexp has subgroups then you are prompted for the subgroup to
use to define the contexts.  Subgroup 0 means use the entire regexp
match as a context.  Subgroup 1 means use the first regexp group match
as a context.  And so on.

The search contexts are marked in the buffer using a text property.
You can also search them using `isearch-property-*' commands.  The
text property used is the symbol whose name is the regexp.  The
property value is a cons whose car is the regexp (a string) and whose
cdr is PREDICATE.

If you use a prefix arg (or non-interactively if argument REUSE is
non-nil), and you have previously used this command or similar
commands, then this command acts like `isearchp-property-forward' with
a plain prefix arg.  That is, it searches for the text property or
overlay property that you last used in an Isearch command that
searches or applies such properties.  See `isearchp-property-forward'
for more information.

Non-interactively, _IGNORED is ignored, and the other arguments are as
in `isearchp-add-regexp-as-property'."
  (interactive (cons current-prefix-arg (isearchp-regexp-read-args)))
  (setq _ignored  (intern (concat isearchp-property-prop-prefix regexp)))
  (when (or (not reuse)
            (not (consp (car isearchp-property-values)))
            (not (equal (caar isearchp-property-values) regexp))
            (not (eq isearchp-property-prop _ignored))
            (not (isearchp-text-prop-present-p beg end (intern regexp) (cons regexp predicate))))
    (isearchp-regexp-define-contexts beg end _ignored regexp predicate action))
  (setq isearch-forward  t)
  (isearchp-property-1 'isearch-forward '(4)))

(defun isearchp-regexp-context-regexp-search (reuse beg end _ignored regexp &optional predicate action)
  "Regexp search within search contexts that are defined by a regexp.
Same as `isearchp-regexp-context-search', but with regexp searching."
  (interactive (cons current-prefix-arg (isearchp-regexp-read-args)))
  (setq _ignored  (intern (concat isearchp-property-prop-prefix regexp)))
  (when (or (not reuse)
            (not (consp (car isearchp-property-values)))
            (not (equal (caar isearchp-property-values) regexp))
            (not (eq isearchp-property-prop _ignored))
            (not (isearchp-text-prop-present-p beg end (intern regexp) (cons regexp predicate))))
    (isearchp-regexp-define-contexts beg end _ignored regexp predicate action))
  (setq isearch-regexp   t
        isearch-forward  t)
  (isearchp-property-1 'isearch-forward-regexp '(4)))


(defun isearchp-regexp-define-contexts (beg end property regexp &optional predicate action msgp)
  "Define search contexts for a future text- or overlay-property search.
This command does not actually search the contexts.  For that, use
`isearchp-regexp-context-search' or `isearchp-property-forward'.
See `isearchp-regexp-context-search' for a description of the
arguments and prefix-argument behavior in terms of prompting for them.

If `isearchp-dim-outside-search-area-flag' is non-nil then dim the
non-contexts.  (You can use command `isearchp-remove-dimming' to
remove the dimming.)"
  (interactive (append (isearchp-regexp-read-args) (list t)))
  (when msgp (message "Scanning for regexp matches..."))
  (let ((matches-p  (isearchp-regexp-scan beg end property regexp predicate action)))
    (setq isearchp-property-prop    property
          isearchp-property-type    'text
          isearchp-property-values  (list (cons regexp predicate)))
    (when msgp (message (if matches-p "Scanning for regexp matches...done" "NO MATCH for regexp")))))
   
;;(@* "General Non-Interactive Functions")

;;; General Non-Interactive Search-Property Functions ----------------

(defun isearchp-regexp-read-args ()
  "Read args for `isearchp-regexp*'.
The list of args returned is:
 BEG and END, the active region limits or the buffer limits
 PROPERTY, the regexp read, as a symbol (i.e., interned)
 REGEXP, the regexp read, as a string
 PREDICATE, a predicate to filter search contexts
See `isearchp-regexp-context-search' for a description of the prompting."
  (let* ((beg   (if (and transient-mark-mode  mark-active) (region-beginning) (point-min)))
         (end   (if (and transient-mark-mode  mark-active) (region-end) (point-max)))
         (valuesp  (and (consp (car isearchp-property-values))
                        (stringp (caar isearchp-property-values))))
         (regxp    (if (or (not current-prefix-arg)  (not valuesp))
                       (isearchp-read-context-regexp)
                     (caar isearchp-property-values)))
         (prop     (if (or (not current-prefix-arg)  (not valuesp))
                       (intern (concat isearchp-property-prop-prefix regexp))
                     isearchp-property-prop))
         (pred     (and (or (not current-prefix-arg)  (not valuesp))
                        (read-from-minibuffer "Predicate to filter search contexts: "
                                              nil read-expression-map t
                                              (and (boundp 'function-name-history)
                                                   'function-name-history)
                                              "nil"))))
    (list beg end prop regxp pred nil))) ; ACTION is always nil.

(defun isearchp-property-1 (search-fn arg)
  "Helper for text and overlay property Isearch commands.
SEARCH-FN is the search function.
ARG is normally from the prefix arg - see `isearchp-property-forward'."
  ;; Emacs bug #21091: must wrap with `ignore-errors' now - if fixed before Emacs 25 then I can remove it.
  (ignore-errors (isearch-done))

  ;; At least for the initial message, assume we are starting over, with a new kind of searching.
  ;; Maybe we should reset more vars here, unless we are starting from within `isearch-mode'?
  ;; Do nothing about that, for now.  Just reset those that might make the message wrong.
  (setq isearch-word                     nil
        isearch-success                  t
        isearch-wrapped                  nil
        isearch-adjusted                 nil
        ;; isearch-barrier                  (point)
	;; isearch-yank-flag                nil
        ;; isearch-error                    nil
        ;; isearch-just-started             t
        )
  (let ((message-log-max  nil))
    (message "CHAR PROP %s" (isearchp-message-prefix nil nil isearch-nonincremental)) (sit-for 1))

  (let* ((enable-recursive-minibuffers    t)
         ;; Prevent invoking `isearch-edit-string', from `isearch-exit'.
         (search-nonincremental-instead   nil)
         (restartp  (or (not arg)  (not isearchp-property-prop)))
         (type      (if restartp ; For TYPE, tested *-prop above, not *-type, because nil means both.
                        (let ((typname
                               (completing-read
                                "Type: " '(("text") ("overlay") ("text and overlay"))
                                nil t nil nil "text and overlay")))
                          (and (not (string= "text and overlay" typname))  (intern typname)))
                      (if (consp arg)   ; `C-u' means use last.
                          isearchp-property-type
                        (setq arg  (prefix-numeric-value arg))
                        (cond ((zerop arg)  nil) ; Both text and overlay.
                              ((> arg 0)    'text)
                              (t            'overlay)))))
         (props     (and restartp  (mapcar #'(lambda (prop) (list (symbol-name prop)))
                                           (isearchp-properties-in-buffer
                                            (current-buffer) (point-min) (point-max) type))))
         (prop      (if restartp
                        (intern (completing-read
                                 (format "%s property to %ssearch: "
                                         (if type (capitalize (symbol-name type))
                                           "Text and overlay")
                                         (if isearchp-complement-domain-p "NOT " ""))
                                 props nil nil nil 'isearchp-property-history "face"))
                      isearchp-property-prop))
         (values    (if restartp
                        (if (memq prop '(face font-lock-face))
                            (isearchp-read-face-names)
                          (isearchp-read-sexps))
                      isearchp-property-values)))
    (setq isearchp-filter-predicate-orig  isearch-filter-predicate
          isearch-filter-predicate        (isearchp-property-filter-pred type prop values)
          isearchp-property-type          type
          isearchp-property-prop          prop
          isearchp-property-values        values))
  (add-hook 'isearch-mode-end-hook 'isearchp-restore-pred-and-remove-dimming)
  (funcall search-fn))

;; Similar to `icicle-properties-in-buffer', defined in `icicles-cmd2.el', but this has PREDICATE.
(defun isearchp-properties-in-buffer (&optional buffer beg end type predicate)
  "List of all text or overlay properties in BUFFER between BEG and END.
Only the properties are included, not their values.
TYPE can be `overlay', `text', or nil, meaning overlay properties,
text properties, or both, respectively.
Non-nil PREDICATE is a predicate that each property must satisfy."
  (unless buffer (setq buffer  (current-buffer)))
  (let ((props  ())
        ovrlays curr-props)
    (when (bufferp buffer)     ; Do nothing if BUFFER is not a buffer.
      (with-current-buffer buffer
        (unless (and beg  end)
          (setq beg  (point-min)
                end  (point-max)))
        (when (or (not type)  (eq type 'overlay)) ; Get overlay properties.
          (setq ovrlays  (overlays-in beg end))
          (dolist (ovrly  ovrlays)
            (setq curr-props  (overlay-properties ovrly))
            (while curr-props
              (when (and (not (memq (car curr-props) props))
                         (or (not predicate)  (funcall predicate (car curr-props))))
                (push (car curr-props) props))
              (setq curr-props  (cddr curr-props)))))
        (when (or (not type)  (eq type 'text)) ; Get text properties.
          (while (< beg end)
            (setq beg         (or (next-property-change beg nil end)  end)
                  curr-props  (text-properties-at beg))
            (while curr-props
              (when (and (not (memq (car curr-props) props))
                         (or (not predicate)  (funcall predicate (car curr-props))))
                (push (car curr-props) props))
              (setq curr-props  (cddr curr-props)))))))
    props))

(defun isearchp-property-filter-pred (type property values)
  "Return a predicate that uses `isearchp-property-matches-p'.
TYPE, PROPERTY, and VALUES are used by that function.
The predicate is suitable as a value of `isearch-filter-predicate'."
  (let ((tag  (make-symbol "isearchp-property-filter-pred")))
    `(lambda (beg end)
       (and (or (not (boundp 'isearchp-reg-beg))  (not isearchp-reg-beg)  (>= beg isearchp-reg-beg))
            (or (not (boundp 'isearchp-reg-end))  (not isearchp-reg-end)  (< end isearchp-reg-end))
            (or (and (fboundp 'isearch-filter-visible) (isearch-filter-visible beg end))
                (and (boundp 'isearch-invisible) ; Emacs 24.4+
                     (not (or (eq search-invisible t)  (not (isearch-range-invisible beg end))))))
            (catch ',tag
              (while (< beg end)
                (let ((matches-p  (isearchp-property-matches-p ',type ',property ',values
                                                               (isearchp-property-default-match-fn
                                                                ',property)
                                                               beg)))
                  (unless (if matches-p (not isearchp-complement-domain-p) isearchp-complement-domain-p)
                    (throw ',tag nil)))
                (setq beg  (1+ beg)))
              t)))))

(defun isearchp-text-prop-present-p (beg end property value)
  "Return non-nil if some text between BEG and END has PROPERTY of VALUE."
  (unless (and beg  end) (setq beg  (point-min)
                               end  (point-max)))
  (save-excursion
    (let ((pos  beg)
          propval)
      (goto-char pos)
      (catch 'isearchp-text-prop-present-p
        (while (<= pos end)
          (setq pos      (or (next-property-change pos nil end)  end)
                propval  (get-text-property pos property))
          (if (and propval  (equal propval value))
              (throw 'isearchp-text-prop-present-p t)
            (setq pos  (1+ pos))))
        nil))))

;; Same as `icicle-search-char-prop-matches-p', defined in `icicles-cmd2.el'.
(defun isearchp-property-matches-p (type property values match-fn position)
  "Return non-nil if POSITION has PROPERTY with a value matching VALUES.
TYPE is `overlay', `text', or nil, and specifies the type of property.
TYPE nil means look for both overlay and text properties.  Return
 non-nil if either matches.

Matching means finding text with a PROPERTY value that overlaps with
VALUES: If the value of PROPERTY is an atom, then it must be a member
of VALUES.  If it is a list, then at least one list element must be a
member of VALUES.

MATCH-FN is a binary predicate that is applied to each item of VALUES
and a zone of text with property PROP.  If it returns non-nil then the
zone is a search hit."
  (let* ((ov-matches-p   nil)
         (txt-matches-p  nil)
         (ovchk-p        (and (or (not type)  (eq type 'overlay))))
         (ovs            (and ovchk-p  (overlays-at position))))
    (when ovchk-p
      (setq ov-matches-p
            (catch 'isearchp-property-matches-p
              (dolist (ov  ovs)
                (when (isearchp-some values (overlay-get ov property) match-fn)
                  (throw 'isearchp-property-matches-p t)))
              nil)))
    (when (and (or (not type)  (eq type 'text)))
      (setq txt-matches-p  (isearchp-some values (get-text-property position property) match-fn)))
    (or ov-matches-p  txt-matches-p)))

;; Same as `icicle-some', defined in `icicles-fn.el'.
(defun isearchp-some (lst arg2 predicate)
  "Apply binary PREDICATE successively to an item of list LST and ARG2.
Return the first non-nil value returned by PREDICATE, or nil if none.
PREDICATE must be a function with two required arguments."
  (let ((result  nil))
    (catch 'isearchp-some
      (dolist (arg1  lst)
        (when (setq result  (funcall predicate arg1 arg2))  (throw 'isearchp-some result))))
    result))

;; Same as `icicle-search-property-default-match-fn', defined in `icicles-cmd2.el'.
(defun isearchp-property-default-match-fn (property)
  "Return the default match function for text or overlay PROPERTY.
Properties `face' and `mumamo-major-mode' are handled specially.
For other properties the values are matched using `equal'."
  (case property
    ((face font-lock-face) (lambda (val rprop)
                             (if (consp rprop)
                                 (condition-case nil ; Allow for dotted cons.
                                     (member val rprop)
                                   (error nil))
                               (eq val rprop))))
    ((mumamo-major-mode)   (lambda (val rprop) (equal val (car rprop))))
    (t                     #'equal)))

(defun isearchp-restore-pred-and-remove-dimming ()
  "End Isearch for a text or overlay property."
  (setq isearch-filter-predicate  isearchp-filter-predicate-orig)
  (isearchp-remove-dimming)
  (remove-hook 'isearch-mode-end-hook 'isearchp-restore-pred-and-remove-dimming))

(defun isearchp-put-prop-on-region (property value beg end)
  "Add text PROPERTY with VALUE to the region from BEG to END.
Interactively, BEG and END are the region limits.

With no prefix argument, or if you have not previously used an Isearch
command that searches properties or applies them, you are prompted for
the following:

 * the property (e.g., `face', `mumamo-major-mode')
 * the property values (e.g., a list of faces, for property `face')

If the property is `face' or `font-lock-face' then you can specify
more than one face - their union is used as the property value.  If
you specify none (empty input immediately) then *all* faces are
*removed* from the region.

If the property is not `face' or `font-lock-face' then you enter a
sexp, which is read as the Lisp value to use.  For example, if the
property is `mumamo-major-mode' you might enter `(emacs-lisp-mode)'
as the value.

If you have already used any of the Isearch property commands, then a
prefix arg means reuse the property and (the first of) its values that
you last specified.  Such commands include the `isearchp-property-*'
commands and commands such as `isearchp-imenu*',
`isearchp-thing(-regexp)', `isearchp-regexp-context-search', and
`isearchp-put-prop-on-region'."
  (interactive
   (if (and current-prefix-arg  isearchp-property-prop  (car isearchp-property-values))
       (list isearchp-property-prop isearchp-property-values (region-beginning) (region-end))
     (let* ((props  (and (or (not current-prefix-arg)  (not isearchp-property-prop))
                         (mapcar #'(lambda (prop) (list (symbol-name prop)))
                                 (isearchp-properties-in-buffer
                                  (current-buffer) (point-min) (point-max) 'text))))
            (prop   (intern (completing-read "Text property: " props nil nil nil
                                             'isearchp-property-history "face")))
            (vals   (if (memq prop '(face font-lock-face))
                        (isearchp-read-face-names 'EMPTY-MEANS-NONE-P)
                      (isearchp-read-sexps 'ONLY-ONE-P))))
       (setq isearchp-property-prop    prop
             isearchp-property-type    'text
             isearchp-property-values  (list prop vals))
       (list prop vals (region-beginning) (region-end)))))
  (with-silent-modifications
    (add-text-properties beg end (list property value))
    (isearchp-add/remove-dim-overlay beg end nil)
;;; $$$$$$    (when (string-match-p "\\`isearchp-" (symbol-name property))
;;;             (setq isearchp-last-prop+value (cons property value)))
    ))


;; FIXME: If repeat `isearchp-lazy-highlights-forward' then no more dimming.
(defun isearchp-lazy-highlights-forward (beg end &optional restartp)
  "Search lazy-highlight zones of text in the active region or the buffer.
If lazy-highlight zones are already marked, and if you did not use a
prefix arg, those previously marked zones are searched.

Otherwise (either a prefix arg or the lazy-highlight zones have not
been marked), the current lazy-highlight zones are marked and then
searched.

You can mark the lazy-highlight zones anytime using command
`isearchp-mark-lazy-highlights'.  See that command for info about
options `lazy-highlight-cleanup' and `lazy-highlight-max-at-a-time'.

Remember that you can use `\\<isearch-mode-map>\\[isearchp-cleanup]' to remove lazy-highlighting and
artifacts from property searching.  This includes dimming and all
`isearchp-' properties."
  (interactive 
   (list (if (and transient-mark-mode  mark-active) (region-beginning) (point-min))
         (if (and transient-mark-mode  mark-active) (region-end) (point-max))
         current-prefix-arg))
  (isearchp-lazy-highlights-forward-1 'isearch-forward beg end restartp))

(defun isearchp-lazy-highlights-forward-regexp (beg end &optional restartp)
  "Same as `isearchp-lazy-highlights-forward', but regexp search."
  (interactive 
   (list (if (and transient-mark-mode  mark-active) (region-beginning) (point-min))
         (if (and transient-mark-mode  mark-active) (region-end) (point-max))
         current-prefix-arg))
  (isearchp-lazy-highlights-forward-1 'isearch-forward-regexp beg end restartp))

(defun isearchp-lazy-highlights-forward-1 (fun beg end restartp)
  "Helper for `isearchp-lazy-highlights-forward(-regexp)'."
  (let ((markedp  (next-single-property-change beg 'isearchp-lazy-highlight)))
    (with-silent-modifications
      (unless (or markedp  (isearchp-lazy-highlights-present-p beg end))
        (error "No lazy-highlight zones"))
      (setq markedp  (if (not restartp)
                         (let ((chg  (next-single-property-change beg 'isearchp-lazy-highlight)))
                           (or (and chg  (/= end chg))
                               (prog1 (isearchp-mark-lazy-highlights beg end 'MSG) (sit-for 0.5))))
                       (put-text-property beg end 'isearchp-lazy-highlight nil) ; Start over.
                       (prog1 (isearchp-mark-lazy-highlights beg end 'MSG) (sit-for 0.5)))))
    (unless markedp (error "No lazy-highlight zones marked")) ; Should not happen.
    (setq isearchp-property-prop    'isearchp-lazy-highlight
          isearchp-property-type    'text
          isearchp-property-values  '(t)))
  (isearchp-property-1 fun '(4)))

(defun isearchp-narrow-to-lazy-highlights (beg end &optional msgp) ; Bound to `C-S-SPC'.
  "Narrow the zones to search, to the current lazy-highlighted text.
Search these zones, but update lazy-highlighting from the new search.
Removes any lazy-highlight marks, then marks lazy-highlights anew,
from the new search."
  (interactive (if (or (not mark-active)  (null (mark))  (= (point) (mark)))
                   (list (point-min) (point-max) t)
                 (if (< (point) (mark)) (list (point) (mark) t) (list (mark) (point) t))))
  (let ((fun  (if isearch-regexp
                  'isearchp-lazy-highlights-forward-regexp
                'isearchp-lazy-highlights-forward)))
    (with-silent-modifications
      (put-text-property beg end 'isearchp-lazy-highlight nil)
      (isearchp-mark-lazy-highlights beg end)))
  (when msgp (message "Updated lazy-highlighting"))
  (add-hook 'isearch-mode-end-hook 'isearchp-restore-pred-and-remove-dimming)
  (setq isearch-string   ""
        isearch-message  ""))

(defun isearchp-lazy-highlights-present-p (start end)
  "Return non-nil if any text from START to END has face `lazy-highlight'."
  (unless start (setq start  (point-min)))
  (unless end   (setq end    (point-max)))
  (catch 'isearchp-lazy-highlights-present-p
    (save-excursion
      (goto-char start)
      (while (< (point) end)
        (let ((face  (get-char-property-and-overlay (point) 'face))
              next)
          (setq face  (and (cdr face) (car face)))
          (when (or (eq face 'lazy-highlight)  (and (listp face)  (memq 'lazy-highlight face)))
            (throw 'isearchp-lazy-highlights-present-p t))
          (goto-char (if (setq next  (next-overlay-change (point)))
                         next
                       end)))))
    nil))

(defun isearchp-mark-lazy-highlights (&optional start end msgp)
  "Add text property `isearchp-lazy-highlight' to lazy-highlighted text.
Do this over the buffer text or over the region text, if active.

Note:

 1. To make Isearch keep lazy-highlighting when done searching, set
    option `lazy-highlight-cleanup' to nil.

 2. Only search matches that have actually been lazy-highlighted are
    marked.  If you want to be sure to match all search matches then
    set option `lazy-highlight-max-at-a-time' to nil before
    searching.  (But see Emacs bug #21092.)"
  (interactive (if (or (not mark-active)  (null (mark))  (= (point) (mark)))
                   (list (point-min) (point-max) t)
                 (if (< (point) (mark)) (list (point) (mark) t) (list (mark) (point) t))))
  (let ((addedp  (isearchp-add-prop-to-lazy-highlights 'isearchp-lazy-highlight t start end)))
    (when msgp
      (message (if addedp
                   "Lazy highlights now marked with property `isearchp-lazy-highlight'"
                 "No changes made - lazy-highlighting not marked")))
    addedp))

(defun isearchp-add-prop-to-lazy-highlights (prop-to-add value-to-add &optional start end msgp)
  "Add a text property to lazy-highlighted text, from START to END.
START and END are the buffer limits, or the region limits if active.
Interactively, you are prompted for the property to add and its value.

Note: To make Isearch keep lazy-highlighting when done searching, set
option `lazy-highlight-cleanup' to nil."
  (interactive
   (let* ((p-add  (intern (read-string "Property to add: " nil 'isearchp-property-history)))
          (v-add  (if (memq p-add '(face font-lock-face))
                      (isearchp-read-face-names nil 'ONLY-ONE)
                    (isearchp-read-sexps 'ONLY-ONE))))
     (append (list p-add v-add)
             (if (or (not mark-active)  (null (mark))  (= (point) (mark)))
                 (list (point-min) (point-max))
               (if (< (point) (mark)) (list (point) (mark)) (list (mark) (point))))
             (list t))))
  (isearchp-add-prop-to-other-prop-zones
   prop-to-add value-to-add 'face 'lazy-highlight 'overlay start end msgp))

(defun isearchp-add-prop-to-other-prop-zones (prop-to-add value-to-add
                                              prop-to-find value-to-find type-to-find
                                              &optional start end msgp)
  "Add text property to text that has another property, from START to END.
PROP-TO-ADD and VALUE-TO-ADD are the property to add and its value.
PROP-TO-FIND, VALUE-TO-FIND, and TYPE-TO-FIND are the existing
 property, its value, and its type (symbol `text' or `overlay').
START and END are the buffer limits, or the region limits if active.
Interactively, you are prompted for the required arguments.

Returns non-nil if the property was added, nil if not."
  (interactive
   (let* ((p-add   (intern (read-string "Property to add: " nil 'isearchp-property-history)))
          (v-add   (if (memq p-add '(face font-lock-face))
                       (isearchp-read-face-names nil 'ONLY-ONE)
                     (isearchp-read-sexps 'ONLY-ONE)))
          (p-find  (intern (read-string "Property to find: " nil 'isearchp-property-history)))
          (v-find  (if (memq p-find '(face font-lock-face))
                       (intern (isearchp-read-face-names nil 'ONLY-ONE))
                     (isearchp-read-sexps 'ONLY-ONE)))
          (typ     (let ((typname (completing-read
                                   "Type: " '(("text") ("overlay") ("text and overlay"))
                                   nil t nil nil "text and overlay")))
                     (and (not (string= "text and overlay" typname))  (intern typname)))))
     (append (list p-add v-add p-find v-find typ)
             (if (or (not mark-active)  (null (mark))  (= (point) (mark)))
                 (list (point-min) (point-max))
               (if (< (point) (mark)) (list (point) (mark)) (list (mark) (point))))
             (list t))))
  (let ((last-beg          nil)
        (prop-value        nil)
        (added-prop-p      nil)
        (zbeg              nil)
        (zend              nil)
        (zend-last         nil)
        (match             nil))
    (with-silent-modifications
      (isearchp-with-comments-hidden
       start end
       (restore-buffer-modified-p nil)
       (condition-case-no-debug add-prop-to-zones-with-other-prop
           (save-excursion
             (goto-char (setq last-beg  start))
             (while (and last-beg  (< last-beg end))
               (setq zbeg   nil
                     zend   nil
                     match  nil)
               (while (and (not match)  (< (point) end))
                 (goto-char (next-single-char-property-change (point) prop-to-find nil end))
                 (when (isearchp-property-matches-p type-to-find prop-to-find (list value-to-find)
                                                    (isearchp-property-default-match-fn prop-to-find)
                                                    (point))
                   (setq zbeg   (point)
                         match  t)
                   (isearchp-add/remove-dim-overlay last-beg zbeg 'ADD)))
               (goto-char (next-single-char-property-change (point) prop-to-find nil end))
               (unless (isearchp-property-matches-p type-to-find prop-to-find (list value-to-find)
                                                    (isearchp-property-default-match-fn prop-to-find)
                                                    (point))
                 (setq zend  (point)))
               (when (and zbeg  zend  (/= zbeg zend))
                 ;; `isearchp-put-prop-on-region' also removes dimming from ZBEG to ZEND.
                 (isearchp-put-prop-on-region prop-to-add value-to-add zbeg zend)
                 (setq added-prop-p  value-to-add
                       zend-last     zend))
               (goto-char (setq last-beg  (or zend  zbeg  last-beg)))))
         (error (error "%s" (error-message-string add-prop-to-zones-with-other-prop))))
       (if added-prop-p
           (isearchp-add/remove-dim-overlay zend-last end 'ADD)
         (isearchp-add/remove-dim-overlay start end nil))))
    (when msgp (message (if added-prop-p
                            (format "Added property `%s' with value `%s'" prop-to-add value-to-add)
                          "No property added")))
    added-prop-p))

(defun isearchp-message-prefix (&optional arg1 arg2 arg3)
  "Version of `isearch-message-prefix' that works for all Emacs releases."
  (if (or (< emacs-major-version 24)
          (and (= emacs-major-version 24)  (< emacs-minor-version 3)
               (not (string-match-p "^[0-9]+\\.[0-9]+\\.[0-9]+" emacs-version))))
      (isearch-message-prefix arg1 arg2 arg3) ; Emacs 20 through 24.2.
    (isearch-message-prefix arg1 arg2))) ; Emacs 24.1.N and 24.3+

(defun isearchp-read-face-names  (&optional empty-means-none-p only-one-p)
  "Read face names with completion, and return a list of their symbols.
If user hits `RET' with empty input immediately, then include all
faces.  Otherwise, read faces one by one, until user hits `RET' twice
consecutively.

Non-nil optional arg EMPTY-MEANS-NONE-P means return nil (no face
names) for empty user input.

Non-nil optional arg ONLY-ONE-P means read only one face name and
return its symbol.

If you use also library Icicles then face-name candidates show their
face in buffer `*Completions*' (WYSIWYG) - see option
`icicle-WYSIWYG-Completions-flag'."
  (let ((icicle-multi-completing-p                   t)
        (icicle-list-nth-parts-join-string           ": ")
        (icicle-list-join-string                     ": ")
        (icicle-list-use-nth-parts                   '(1))
        (icicle-proxy-candidates
         (and (boundp 'icicle-add-proxy-candidates-flag)  icicle-add-proxy-candidates-flag
              (append (and (fboundp 'eyedrop-face-at-point)  (list "*point face name*"))
                      (let ((ipc  ()))
                        (mapatoms
                         (lambda (cand)
                           (when (and (user-variable-p cand)  (eq (get cand 'custom-type) 'face))
                             (push `,(concat "'" (symbol-name cand) "'") ipc))))
                        ipc))))
        (face-cands                                  (mapcar
                                                      (if (and (boundp 'icicle-mode)  icicle-mode)
                                                          #'icicle-make-face-candidate
                                                        (lambda (face) (list (symbol-name face))))
                                                      (face-list)))
        (faces                                       ())
        (prompt1                                     "Face (RET for each, empty input to finish): ")
        (prompt2                                     "Face: ")
        (icicle-unpropertize-completion-result-flag  t)
        face)
    (when (and (boundp 'icicle-mode)  icicle-mode)
      (put-text-property 0 1 'icicle-fancy-candidates t prompt1)
      (put-text-property 0 1 'icicle-fancy-candidates t prompt2))
    (setq face  (isearchp-read-face-names--read prompt1 face-cands))
    (if (and empty-means-none-p  (string= "" face))
        ()
      (if only-one-p
          face
        (if (string= "" face)
            (setq faces  (face-list))
          (setq face-cands  (delete (assoc face face-cands) face-cands))
          (while (not (string= "" face))
            (add-to-list 'faces (intern face))
            (setq face        (isearchp-read-face-names--read prompt2 face-cands)
                  face-cands  (delete (assoc face face-cands) face-cands)))
          (nreverse faces))))))

(defun isearchp-read-face-names--read (prompt candidates)
  "Read a face name using PROMPT and face-name completion CANDIDATES."
  (if (and (boundp 'icicle-mode)  icicle-mode)
      (icicle-transform-multi-completion
       (completing-read
        prompt candidates nil (not (stringp icicle-WYSIWYG-Completions-flag)) nil
        (if (boundp 'face-name-history) 'face-name-history 'icicle-face-name-history)))
    (completing-read prompt candidates nil t nil 'face-name-history)))

(defun isearchp-read-sexps  (&optional only-one-p)
  "Read sexps with completion, and return them as a list.
Read sexps one by one, until user hits `RET' twice consecutively.
Non-nil ONLY-ONE-P means read only one sexp and return it."
  (let ((sexp-cands                         (mapcar #'list (isearchp-remove-duplicates
                                                            read-expression-history)))
        (sexps                              ())
        (prompt1                            "Sexp (RET for each, empty input to finish): ")
        (prompt2                            "Sexp: ")
        sexp)
    (setq sexp        (completing-read (if only-one-p prompt2 prompt1) sexp-cands
                                       nil nil nil 'read-expression-history)
          sexp-cands  (delete (assoc sexp sexp-cands) sexp-cands))
    (if only-one-p
        (car (read-from-string sexp))
      (while (not (string= "" sexp))
        (add-to-list 'sexps sexp)
        (setq sexp        (completing-read prompt2 sexp-cands nil nil nil 'read-expression-history)
              sexp-cands  (delete (assoc sexp sexp-cands) sexp-cands)))
      (prog1 (setq sexps  (nreverse (delete "" sexps)) ; Return the list of sexps.
                   sexps  (mapcar (lambda (sx)
                                    (car (condition-case nil
                                             (read-from-string sx)
                                           (error nil))))
                                  sexps))
        (when (interactive-p) (message "Sexps: %S" sexps))))))

(defun isearchp-read-context-regexp ()
  "Read context regexp and determine `isearchp-context-level'."
  (let* ((prompt  "Search %s contexts (regexp)")
         (prompt  (format prompt (if isearchp-complement-domain-p "*OUTSIDE*" "within")))
         (regexp  (read-regexp prompt)))
    (while (string= "" regexp)
      (message "Regexp cannot be empty.  Try again...") (sit-for 2)
      (setq regexp  (read-regexp prompt)))
    (setq isearchp-context-level  (if (string-match-p "\\\\(" regexp)
                                      (truncate (read-number
                                                 "Subgroup to use as search context [0, 1, 2,...]: "
                                                 0))
                                    0))
    regexp))

(defun isearchp-regexp-scan (beg end property regexp &optional predicate action)
  "Scan for REGEXP, adding text property PROPERTY to matches.
If the region is active then scan it.  Otherwise scan the buffer.
Return the value of PROPERTY, if added somewhere.  Else return nil.
See `isearchp-add-regexp-as-property' for the parameter descriptions."
  ;; Use text property (REGEXP . PREDICATE).  It is not enough to record only REGEXP.
  ;; E.g., `*-imenu-non-interactive-function' and `*-imenu-command' use the same REGEXP.
  ;; Only the PREDICATE is different.
  (setq regexp  (or regexp  (isearchp-read-context-regexp)))
  (when (stringp property) (setq property  (intern property)))
  (unless (and beg  end) (setq beg  (point-min)
                               end  (point-max)))
  (unless (< beg end) (setq beg  (prog1 end (setq end  beg)))) ; Ensure BEG is before END.
  (let ((prop-value    (cons regexp predicate))
        (last-beg      nil)
        (added-prop-p  nil))
    (with-silent-modifications
      (condition-case-no-debug isearchp-regexp-scan
          (save-excursion
            (restore-buffer-modified-p nil)
            (goto-char (setq last-beg  beg))
            (while (and beg  (< beg end)  (not (eobp))
                        (progn (while (and (setq beg  (re-search-forward regexp end t))
                                           (eq last-beg beg)
                                           (not (eobp)))
                                 ;; Matched again, same place.  Advance 1 char.
                                 (forward-char) (setq beg  (1+ beg)))
                               beg))    ; Stop if no more matches.
              (unless (or (not beg)  (match-beginning isearchp-context-level)) ; No `user-error': Emacs 23
                (error "Search context has no subgroup of level %d - try a lower number"
                       isearchp-context-level))
              (let* ((hit-beg     (match-beginning isearchp-context-level))
                     (hit-end     (match-end isearchp-context-level))
                     (IGNORED     (when (and hit-beg  action)
                                    (save-excursion (funcall action)
                                                    (setq hit-end  (min end (point))
                                                          beg      hit-end))))
                     (hit-string  (buffer-substring-no-properties hit-beg hit-end))
                     (c-beg       last-beg)
                     (c-end       (if beg
                                      (match-beginning isearchp-context-level)
                                    (min end (point-max)))) ; Truncate.
                     (pred-ok-p   t)
                     end-marker)
                (remove-text-properties c-beg c-end (list property 'IGNORED))
                (isearchp-add/remove-dim-overlay c-beg c-end 'ADD)
                (when (and predicate
                           (not (string= "" hit-string))
                           (setq end-marker  (copy-marker hit-end)))
                  (setq pred-ok-p  (save-match-data (funcall predicate hit-string end-marker))))
                (cond ((and pred-ok-p  (not (string= "" hit-string)))
                       (put-text-property hit-beg hit-end property prop-value)
                       (setq added-prop-p  prop-value)
                       (isearchp-add/remove-dim-overlay hit-beg hit-end nil))
                      (t
                       (remove-text-properties hit-beg hit-end (list property 'IGNORED))
                       (isearchp-add/remove-dim-overlay hit-beg hit-end 'ADD))))
              (goto-char (setq last-beg  beg))))
        (error (error "%s" (error-message-string isearchp-regexp-scan)))))
    ;; $$$$$$ (when added-prop-p (setq isearchp-last-prop+value (cons property prop-value)))
    added-prop-p)) ; Return property value if added, or nil otherwise.

;; This does not, itself, use `with-silent-modifications', so code that calls this needs to use it.
(defun isearchp-add/remove-dim-overlay (beg end addp)
  "Add dim overlay from BEG to END, or remove overlays between BEG and END.
Non-nil ADDP means add an overlay; nil means remove any present.
But reverse the effect of ADDP if `isearchp-complement-domain-p'
is non-nil.

For removal, an overlay is considered present between BEG and END if
it begins before END and it ends after BEG."
  (when isearchp-complement-domain-p (setq addp  (not addp)))
  (cond (addp
         (let ((ov  (make-overlay beg end)))
           (push ov isearchp-dimmed-overlays)
           (overlay-put ov 'priority 200) ; < isearch-overlay's 1001.
           (overlay-put ov 'face (isearchp-dim-face-spec))))
        (t
         (let (obeg oend)
           (dolist (dim-ov  isearchp-dimmed-overlays)
             (setq obeg  (overlay-start dim-ov)
                   oend  (overlay-end   dim-ov))
             (when (and obeg  oend  (< beg oend)  (< obeg end))
               (delete-overlay dim-ov)
               (setq isearchp-dimmed-overlays  (delete dim-ov isearchp-dimmed-overlays))))))))

;; Same as `icicle-remove-duplicates'.
(defun isearchp-remove-duplicates (sequence &optional test)
  "Copy of SEQUENCE with duplicate elements removed.
Optional arg TEST is the test function.  If nil, test with `equal'.
See `make-hash-table' for possible values of TEST."
  (setq test  (or test  #'equal))
  (let ((htable  (make-hash-table :test test)))
    (loop for elt in sequence
       unless (gethash elt htable)
       do     (puthash elt elt htable)
       finally return (loop for i being the hash-values in htable collect i))))
 
;;(@* "Search-Zones Commands and Functions")

;;; Search-Zones Commands and Functions ------------------------------

(when (require 'zones nil t)
  (defun isearchp-zones-forward (&optional variable)
    "Search forward within the zones of izones variable VARIABLE.
VARIABLE defaults to the value of `zz-izones-var'.

With a prefix arg you are prompted for a different variable to use, in
place of the current value of `zz-izones-var'.  If the prefix arg is
non-negative (>= 0) then make the variable buffer-local.  If the
prefix arg is non-positive (<= 0) then set `zz-izones-var' to that
variable symbol.  (Zero: do both.)

Non-interactively, VARIABLE is the izones variable to use.

If `isearchp-complement-domain-p' is non-nil then move to the next
non-zone; that is, the areas to search are those outside the given
zones.  (Use `C-M-~' during Isearch to toggle this variable.)"
    (interactive (isearchp-zones-read-args))
    (setq isearch-forward  t)
    (isearchp-zones-1 'isearch-forward variable))

  (defun isearchp-zones-backward (&optional variable)
    "Search backward within the zones of restrictions variable VARIABLE.
See `isearchp-zones-forward'."
    (interactive (isearchp-zones-read-args))
    (setq isearch-forward  nil)
    (isearchp-zones-1 'isearch-backward variable))

  (defun isearchp-zones-forward-regexp (&optional variable)
    "Regexp search forward within the zones of restrictions variable VARIABLE.
See `isearchp-zones-forward'."
    (interactive (isearchp-zones-read-args))
    (setq isearch-regexp   t
          isearch-forward  t)
    (isearchp-zones-1 'isearch-forward-regexp variable))

  (defun isearchp-zones-backward-regexp (&optional variable)
    "Regexp search backward within the zones of restrictions variable VARIABLE.
See `isearchp-zones-forward'."
    (interactive (isearchp-zones-read-args))
    (setq isearch-regexp   t
          isearch-forward  nil)
    (isearchp-zones-1 'isearch-backward-regexp variable))

  (defun isearchp-zones-read-args ()
    "Read args for `isearchp-zones*'."
    (let ((var    (and current-prefix-arg  (zz-read-any-variable "Variable: ")))
          (npref  (prefix-numeric-value current-prefix-arg)))
      (when (and current-prefix-arg  (>= npref 0)) (make-local-variable var))
      (when (and current-prefix-arg  (<= npref 0)) (setq zz-izones-var  var))
      (list var)))

  (defun isearchp-zones-1 (search-fn &optional variable)
    "Helper for Isearch zone commands.
Search with function SEARCH-FN within the zones of izones VARIABLE.
VARIABLE defaults to the value of `zz-izones-var'."
    (unless variable (setq variable  zz-izones-var))
    (when isearchp-dim-outside-search-area-flag
      (with-silent-modifications
        (let* ((zones       (zz-zone-union (zz-izone-limits (symbol-value variable) nil 'ONLY-THIS-BUFFER)))
               (comp-zones  (zz-zones-complement zones)))
          (dolist (zone  (if isearchp-complement-domain-p comp-zones zones))
            (let ((isearchp-complement-domain-p  nil))
              (isearchp-add/remove-dim-overlay (car zone) (cadr zone) nil)))
          (dolist (zone  (if isearchp-complement-domain-p zones comp-zones))
            (let ((isearchp-complement-domain-p  nil))
              (isearchp-add/remove-dim-overlay (car zone) (cadr zone) 'ADD))))))
    ;; Emacs bug #21091: must wrap with `ignore-errors' now - if fixed before Emacs 25 then I can remove it.
    (ignore-errors (isearch-done))
    ;; At least for the initial message, assume we are starting over, with a new kind of searching.
    ;; Maybe we should reset more vars here, unless we are starting from within `isearch-mode'?
    ;; Do nothing about that, for now.  Just reset those that might make the message wrong.
    (setq isearch-word                     nil
          isearch-success                  t
          isearch-wrapped                  nil
          isearch-adjusted                 nil
          ;; isearch-barrier                  (point)
          ;; isearch-yank-flag                nil
          ;; isearch-error                    nil
          ;; isearch-just-started             t
          )
    (let ((message-log-max  nil))
      (message "%sZONE SEARCH %s" (if isearchp-complement-domain-p "*OUTSIDE* " "")
               (isearchp-message-prefix nil nil isearch-nonincremental))
      (sit-for 1))
    (let* ((enable-recursive-minibuffers   t)
           ;; Prevent invoking `isearch-edit-string', from `isearch-exit'.
           (search-nonincremental-instead  nil))
      (setq isearchp-filter-predicate-orig  isearch-filter-predicate
            isearch-filter-predicate        (isearchp-zones-filter-pred (zz-izone-limits
                                                                         (symbol-value variable)))))
    (add-hook 'isearch-mode-end-hook 'isearchp-restore-pred-and-remove-dimming)
    (funcall search-fn))

  (defun isearchp-zones-filter-pred (&optional zones)
    "Return a predicate that tests if its args are in ZONES.
Optional arg ZONES is a list of basic zones.
The predicate is suitable as a value of `isearch-filter-predicate'."
    (lexical-let ((limits  zones))
      (lambda (beg end)
        (and (or (not (boundp 'isearchp-reg-beg))  (not isearchp-reg-beg)  (>= beg isearchp-reg-beg))
             (or (not (boundp 'isearchp-reg-end))  (not isearchp-reg-end)  (< end isearchp-reg-end))
             (or (and (fboundp 'isearch-filter-visible) (isearch-filter-visible beg end))
                 (and (boundp 'isearch-invisible) ; Emacs 24.4+
                      (not (or (eq search-invisible t)  (not (isearch-range-invisible beg end))))))
             (let ((in-zone-p  (zz-some (lambda (zone) (and (<= (nth 0 zone) beg)  (>= (nth 1 zone) end)))
                                        (zz-zone-union limits))))
               (if isearchp-complement-domain-p (not in-zone-p) in-zone-p))))))

  (defun isearchp-narrow-to-matching-zones (&optional all-zones msgp) ; Bound to `S-SPC'.
    "Narrow the zones to search, to those containing lazy-highlighting.
The other search zones are excluded for the continued searching.
Optional arg ALL-ZONES is the list of zones to check for exclusion.
Non-interactively, arg MSGP means show a status message."
    (interactive (list (funcall isearchp-zone-limits-function) t))
    (let ((zones  (or all-zones  (zz-izone-limits (symbol-value zz-izones-var)))))
      (isearchp-exclude-zones-w-no-lazy-highlight zones))
    (let ((opred  isearch-filter-predicate))
      (setq isearch-filter-predicate
            `(lambda (beg end)
               (and (not (funcall (isearchp-zones-filter-pred isearchp-excluded-zones) beg end))
                    (funcall ,opred beg end))))
      (when msgp (message "Narrowed to zones that have a search hit"))))

  (defun isearchp-zone-limits-izones ()
    "Return the limits of the current value of `zz-izones-var'.
That is, return the basic zones."
    (zz-izone-limits (symbol-value zz-izones-var)))

  (defun isearchp-exclude-zones-w-no-lazy-highlight (zones)
    "Add zones that contain no lazy highlight to `isearchp-excluded-zones'.
ZONES is the list of zones to check for exclusion."
    (let (beg end)
      (dolist (zone  zones)
        (setq beg  (nth 0 zone)
              end  (nth 1 zone))
        (when (> beg end) (setq beg  (prog1 end (setq end  beg))))
        (unless (isearchp-lazy-highlights-present-p beg end) (add-to-list 'isearchp-excluded-zones zone))))
    (dolist (zone  isearchp-excluded-zones)
      (isearchp-add/remove-dim-overlay (nth 0 zone) (nth 1 zone) 'ADDP))
    (isearch-lazy-highlight-update)
    isearchp-excluded-zones)

  (defadvice perform-replace (around respect-isearchp-query-replace-zones-flag activate)
    "Respect option `isearchp-query-replace-zones-flag'."
    (let* ((orig-pred                 isearch-filter-predicate)
           (zone-pred                 (and isearchp-query-replace-zones-flag
                                           (isearchp-zones-filter-pred
                                            (zz-izone-limits (symbol-value zz-izones-var)))))
           (isearch-filter-predicate  (cond ((and orig-pred  zone-pred)
                                             (lambda (beg end)
                                               (and (funcall orig-pred beg end)
                                                    (funcall zone-pred beg end))))
                                            (zone-pred)
                                            (orig-pred))))
      ad-do-it))

  (defun isearchp-put-prop-on-zones (property value zones)
    "Add text PROPERTY with VALUE to the text in each of the ZONES.
Does `isearchp-put-prop-on-region' (which see) on each zone in ZONES.
Interactively, ZONES is the value of variable `zz-izones-var'."
    (interactive
     (let ((zs  (isearchp-zone-limits-izones)))
       (unless zs (error "No zones"))
       (if (and current-prefix-arg  isearchp-property-prop  (car isearchp-property-values))
           (list isearchp-property-prop isearchp-property-values zs)
         (let* ((props  (and (or (not current-prefix-arg)  (not isearchp-property-prop))
                             (mapcar #'(lambda (prop) (list (symbol-name prop)))
                                     (isearchp-properties-in-buffer
                                      (current-buffer) (point-min) (point-max) 'text))))
                (prop   (intern (completing-read "Text property: " props nil nil nil
                                                 'isearchp-property-history "face")))
                (vals   (if (memq prop '(face font-lock-face))
                            (isearchp-read-face-names 'EMPTY-MEANS-NONE-P)
                          (isearchp-read-sexps 'ONLY-ONE-P))))
           (setq isearchp-property-prop    prop
                 isearchp-property-type    'text
                 isearchp-property-values  (list prop vals))
           (list prop vals zs)))))
    (with-silent-modifications
      (dolist (beg+end  zones)
        (add-text-properties (nth 0 beg+end) (nth 1 beg+end) (list property value))
        (isearchp-add/remove-dim-overlay (nth 0 beg+end) (nth 1 beg+end) nil)
;;; $$$$$$    (when (string-match-p "\\`isearchp-" (symbol-name property))
;;;             (setq isearchp-last-prop+value (cons property value)))
        )))

  (defun isearchp-make-zones-invisible (zones &optional onlyp no-error-p msgp)
    "Make ZONES invisible.
With a prefix argument, also make anti-zones visible.
Non-interactively:
 Non-nil ONLYP means also make anti-zones visible.
 Non-nil NO-ERROR-P means do not raise an error if ZONES is empty.
 Non-nil MSGP means echo the new state."
    (interactive (list (isearchp-zone-limits-izones) current-prefix-arg nil t))
    (unless (or no-error-p  zones) (error "No zones"))
    (isearchp-put-prop-on-zones 'invisible t zones)
    (setq isearchp-zones-invisible-p  t)
    (when onlyp (isearchp-make-anti-zones-visible zones nil no-error-p msgp))
    (when msgp (message (if onlyp
                            "Only ANTI-zones are now visible"
                          "Zones are now INvisible"))))

  (defun isearchp-make-zones-visible (zones &optional onlyp no-error-p msgp)
    "Make ZONES visible.
With a prefix argument, also make anti-zones invisible.
Non-interactively:
 Non-nil ONLYP means also make anti-zones invisible.
 Non-nil NO-ERROR-P means do not raise an error if ZONES is empty.
 Non-nil MSGP means echo the new state."
    (interactive (list (isearchp-zone-limits-izones) current-prefix-arg nil t))
    (unless (or no-error-p  zones) (error "No zones"))
    (isearchp-put-prop-on-zones 'invisible nil zones)
    (setq isearchp-zones-invisible-p  nil)
    (when onlyp (isearchp-make-anti-zones-invisible zones nil no-error-p msgp))
    (when msgp (message "%sones are now VISIBLE" (if onlyp "Only z" "Z"))))

  (defun isearchp-toggle-zones-invisible (zones &optional onlyp no-error-p msgp)
    "Toggle visibility of ZONES.
With a prefix argument, also toggle anti-zones the other way.
Non-interactively:
 Non-nil ONLYP means also toggle anti-zones the other way.
 Non-nil NO-ERROR-P means do not raise an error if ZONES is empty.
 Non-nil MSGP means echo the new state."
    (interactive (list (isearchp-zone-limits-izones) current-prefix-arg nil t))
    (unless (or no-error-p  zones) (error "No zones"))
    (if isearchp-zones-invisible-p
        (isearchp-make-zones-visible zones onlyp no-error-p msgp)
      (isearchp-make-zones-invisible zones onlyp no-error-p msgp)))

  (defun isearchp-make-anti-zones-invisible (zones &optional onlyp no-error-p msgp)
    "Make the complement of (the union of) ZONES invisible.
With a prefix argument, also make zones visible.
Non-interactively:
 Non-nil ONLYP means also make zones visible.
 Non-nil NO-ERROR-P means do not raise an error if ZONES is empty.
 Non-nil MSGP means echo the new state."
    (interactive (list (isearchp-zone-limits-izones) current-prefix-arg nil t))
    (unless (or no-error-p  zones) (error "No zones"))
    (let ((anti-zones  (zz-zones-complement (zz-zone-union zones))))
      (isearchp-put-prop-on-zones 'invisible t anti-zones)
      (setq isearchp-anti-zones-invisible-p  t)
      (when onlyp (isearchp-make-zones-visible zones nil no-error-p msgp))
      (when msgp (message "Anti-zones are now INvisible%s" (if onlyp ", zones are visible" "")))))

  (defun isearchp-make-anti-zones-visible (zones &optional onlyp no-error-p msgp)
    "Make the complement of (the union of) ZONES visible.
With a prefix argument, also make zones invisible.
Non-interactively:
 Non-nil ONLYP means also make zones invisible.
 Non-nil NO-ERROR-P means do not raise an error if ZONES is empty.
 Non-nil MSGP means echo the new state."
    (interactive (list (isearchp-zone-limits-izones) current-prefix-arg nil t))
    (unless (or no-error-p  zones) (error "No zones"))
    (let ((anti-zones  (zz-zones-complement (zz-zone-union zones))))
      (isearchp-put-prop-on-zones 'invisible nil anti-zones)
      (setq isearchp-anti-zones-invisible-p  nil)
      (when onlyp (isearchp-make-zones-invisible zones nil no-error-p msgp))
      (when msgp (message "Anti-zones are now VISIBLE%s" (if onlyp ", zones are invisible" "")))))

  (defun isearchp-toggle-anti-zones-invisible (zones &optional onlyp no-error-p msgp)
    "Toggle visibility of the complement of (the union of) ZONES.
With a prefix argument, also toggle zones the other way.
Non-interactively:
 Non-nil ONLYP means also toggle zones the other way.
 Non-nil NO-ERROR-P means do not raise an error if ZONES is empty.
 Non-nil MSGP means echo the new state."
    (interactive (list (isearchp-zone-limits-izones) current-prefix-arg nil t))
    (unless (or no-error-p  zones) (error "No zones"))
    (if isearchp-anti-zones-invisible-p
        (isearchp-make-anti-zones-visible zones onlyp no-error-p msgp)
      (isearchp-make-anti-zones-invisible zones onlyp no-error-p msgp)))

  (defun isearchp-toggle-zone/anti-zone-visibility (zones &optional no-error-p msgp)
    "Show only zones, if they are invisible.  Else show only anti-zones."
    (interactive (list (isearchp-zone-limits-izones) nil t))
    (unless (or no-error-p  zones) (error "No zones"))
    (cond (isearchp-zones-invisible-p
           (isearchp-make-zones-visible zones 'ONLY no-error-p)
           (when msgp (message "Showing only ZONES now")))
          (t
           (isearchp-make-anti-zones-visible zones 'ONLY no-error-p)
           (when msgp (message "Showing only ANTI-zones now")))))

  )
 
;;(@* "Imenu Commands and Functions")

;;; Imenu Commands and Functions -------------------------------------

(defun isearchp-imenu (&optional regexp-p)
  "Search Imenu entries.
Search is limited to the region if it is active.
A search context is the text between the beginning of the Imenu regexp
match and `forward-sexp' from there.

With a prefix arg use regexp search.  Otherwise, use literal search."
  (interactive "P")
  (isearchp-imenu-1 nil nil regexp-p))

(defun isearchp-imenu-command (&optional regexp-p)
  "Search Emacs command definitions.
This uses `commandp', so it finds only currently defined commands.
That is, if the buffer has not been evaluated, then its function
definitions are NOT considered commands by `isearchp-imenu-command'.
See `isearchp-imenu' for more information."
  (interactive "P")
  (unless (eq major-mode 'emacs-lisp-mode)
    (error "This command is only for Emacs-Lisp mode")) ; No `user-error' in Emacs 23.
  (isearchp-imenu-1 (lambda (_hit _mrkr)
                      (commandp (intern-soft
                                 (buffer-substring-no-properties (match-beginning 2) (match-end 2)))))
                    (lambda (menus)
                      (or (car (assoc "Functions" menus))
                          (car (assoc "Other" menus))
                          (error "No command definitions in buffer"))) ; No `user-error' in Emacs 23.
                    regexp-p))

(defun isearchp-imenu-non-interactive-function (&optional regexp-p)
  "Search Emacs non-command function definitions.
This uses `commandp', so it finds only currently defined functions
that are not interactive.
See `isearchp-imenu' for more information."
  (interactive "P")
  (unless (eq major-mode 'emacs-lisp-mode)
    (error "This command is only for Emacs-Lisp mode")) ; No `user-error' in Emacs 23.
  (isearchp-imenu-1 (lambda (_hit _mrkr)
                      (let ((fn  (intern-soft
                                  (buffer-substring-no-properties (match-beginning 2) (match-end 2)))))
                        (and (fboundp fn)  (not (commandp fn)))))
                    (lambda (menus)
                      (or (car (assoc "Functions" menus))
                          (car (assoc "Other" menus))
                          (error "No non-command function definitions in buffer"))) ; No `user-error'
                    regexp-p))

(defun isearchp-imenu-macro (&optional regexp-p)
  "Search Lisp macro definitions.
See `isearchp-imenu' for more information."
  (interactive "P")
  (unless (memq major-mode '(emacs-lisp-mode lisp-mode))
    (error "This command is only for Emacs-Lisp mode or Lisp mode")) ; No `user-error' in Emacs 23.
  (isearchp-imenu-1 (lambda (_hit _mrkr)
                      (let ((fn  (intern-soft
                                  (buffer-substring-no-properties (match-beginning 2) (match-end 2)))))
                        (if (fboundp 'macrop) ; Emacs 24.4+
                            (macrop fn)
                          (and (fboundp fn)
                               (let ((def  (symbol-function fn)))
                                 (and (consp def)  (eq (car def) 'macro)))))))
                    (lambda (menus)
                      (or (car (assoc "Macro" menus))
                          (car (assoc "Other" menus))
                          (error "No macro definitions in buffer"))) ; No `user-error' in Emacs 23.
                    regexp-p))

(defun isearchp-imenu-1 (&optional predicate submenu-fn regexp-p)
  "Helper for `isearchp-imenu*' commands.
Non-nil PREDICATE means act on only the hits for which it holds.  It
is a Boolean function that takes these args:
  - the search-context string
  - a marker at the end of the search-context
SUBMENU-FN is a function to apply to the list of Imenu submenus to
 choose one.  If nil then the user chooses one using completion.
Non-nil REGEXP-P means use regexp search (otherwise, literal search)."
  (unless imenu-generic-expression
    (error "No Imenu pattern for this buffer")) ; No `user-error' in Emacs 23.
  (let ((case-fold-search  (if (or (local-variable-p 'imenu-case-fold-search)
                                   (not (local-variable-p 'font-lock-defaults)))
                               imenu-case-fold-search
                             (nth 2 font-lock-defaults)))
        (old-table         (syntax-table))
        (table             (copy-syntax-table (syntax-table)))
        (slist             imenu-syntax-alist)
        (beg               (if (and transient-mark-mode  mark-active) (region-beginning) (point-min)))
        (end               (if (and transient-mark-mode  mark-active) (region-end) (point-max)))
        (search-fn         (if regexp-p
                               #'isearchp-regexp-context-regexp-search
                             #'isearchp-regexp-context-search)))
    (dolist (syn  slist) ; Modify the syntax table used while matching regexps.
      (if (numberp (car syn))
          (modify-syntax-entry (car syn) (cdr syn) table) ; Single character.
        (dolist (char  (car syn))  (modify-syntax-entry char (cdr syn) table)))) ; String.
    (unwind-protect
         (save-match-data
           (set-syntax-table table)
           (let* ((others   0)
                  (menus    (mapcar (lambda (menu)
                                      (when (equal (car menu) "Other")
                                        (setq others  (1+ others))
                                        (when (> others 1)
                                          (setcar menu (format "Other<%d>" others))))
                                      menu)
                                    (isearchp-remove-if-not
                                     #'isearchp-imenu-in-buffer-p ; Use only menus that match buffer.
                                     (mapcar (lambda (menu) ; Name unlabeled menu(s) `Other[<N>]'.
                                               (if (stringp (car menu))
                                                   menu
                                                 (cons "Other" (cdr menu))))
                                             imenu-generic-expression))))
                  (submenu  (if submenu-fn
                                (funcall submenu-fn menus)
                              (if (cadr menus)
                                  (let ((completion-ignore-case  t))
                                    (completing-read "Choose: " menus nil t))
                                (caar menus)))) ; Only one submenu, so use it.
                  (regexp   (cadr (assoc submenu menus))))
             (unless (stringp regexp) (error "No match")) ; No `user-error' in Emacs 23.
             (funcall search-fn nil beg end 'IGNORED regexp predicate
                      ;; We rely on the match data having been preserved.
                      ;; $$$$$$ An alternative fn for Lisp only: (lambda () (up-list -1) (forward-sexp))))))
                      (lambda ()
                        (goto-char (match-beginning 0))
                        (condition-case isearchp-imenu-1
                            (forward-sexp)
                          (error (goto-char (match-end 0))))))))
      (set-syntax-table old-table))))

(defun isearchp-imenu-in-buffer-p (menu)
  "Return non-nil if the regexp in MENU has a match in the buffer."
  (save-excursion (goto-char (point-min)) (re-search-forward (cadr menu) nil t)))

(defun isearchp-remove-if-not (pred xs)
  "A copy of list XS with only elements that satisfy predicate PRED."
  (let ((result  ()))
    (dolist (x xs) (when (funcall pred x) (push x result)))
    (nreverse result)))
 
;;(@* "THING Commands and Functions")

;;; THING Commands and Functions" ------------------------------------

;; Same as `hide/show-comments-toggle' in `hide-comnt.el'.
;;
(defun isearchp-toggle-hiding-comments (&optional start end) ; Bound to `M-;' during Isearch
  "Toggle hiding/showing of comments in the active region or whole buffer.
If the region is active then toggle in the region.  Otherwise, in the
whole buffer.

During Isearch this is bound to `M-;'.

Interactively, START and END default to the region limits, if active.
Otherwise, including non-interactively, they default to `point-min'
and `point-max'.

Uses `save-excursion', restoring point.

Be aware that using this command to show invisible text shows *ALL*
such text, regardless of how it was hidden.  IOW, it does not just
show invisible text that you previously hid using this command."
  (interactive (if (or (not mark-active)  (null (mark))  (= (point) (mark)))
                   (list (point-min) (point-max))
                 (if (< (point) (mark)) (list (point) (mark)) (list (mark) (point)))))
  (when (require 'newcomment nil t) ; `comment-search-forward', Emacs 21+.
    (comment-normalize-vars)     ; Per Stefan, should call this first.
    (if (save-excursion (goto-char start) (and (comment-search-forward end 'NOERROR)
                                               (get-text-property (point) 'invisible)))
        (isearchp-hide/show-comments 'show start end)
      (isearchp-hide/show-comments 'hide start end))))

;; Same as `hide/show-comments' in `hide-comnt.el'.
;; Same as `icicle-hide/show-comments' in `icicles-cmd2.el'.
;; Except that those libraries need to also handle Emacs < 23.
;;
(defun isearchp-hide/show-comments (&optional hide/show start end)
  "Hide or show comments from START to END.
Interactively, hide comments, or show them if you use a prefix arg.
\(This is thus *NOT* a toggle command.)

If option `hide-whitespace-before-comment-flag' is non-nil, then hide
also any whitespace preceding a comment.

Interactively, START and END default to the region limits, if active.
Otherwise, including non-interactively, they default to `point-min'
and `point-max'.

During Isearch this is invoked when you use `M-;'.  However, in order
to use a prefix arg within Isearch you must set `isearch-allow-scroll'
or `isearch-allow-prefix' (if available) to non-nil.  Otherwise, a
prefix arg exits Isearch.

Uses `save-excursion', restoring point.

Be aware that using this command to show invisible text shows *ALL*
such text, regardless of how it was hidden.  IOW, it does not just
show invisible text that you previously hid using this command.

From Lisp, a HIDE/SHOW value of `hide' hides comments.  Other values
show them."
  (interactive (cons (if current-prefix-arg 'show 'hide)
                     (if (or (not mark-active) (null (mark)) (= (point) (mark)))
                         (list (point-min) (point-max))
                       (if (< (point) (mark)) (list (point) (mark)) (list (mark) (point))))))
  (when (and comment-start       ; No-op if no comment syntax defined.
             (require 'newcomment nil t)) ; `comment-search-forward'
    (comment-normalize-vars)     ; Per Stefan, should call this first.
    (unless start (setq start  (point-min)))
    (unless end   (setq end    (point-max)))
    (unless (<= start end) (setq start  (prog1 end (setq end  start))))
    (with-silent-modifications
      (restore-buffer-modified-p nil)
      (let (cbeg cend)
        (save-excursion
          (goto-char start)
          (while (and (< start end)  (save-excursion (setq cbeg  (comment-search-forward end 'NOERROR))))
            (goto-char cbeg)
            (save-excursion (setq cend  (if (comment-forward 1)
                                            (if (= (char-before) ?\n) (1- (point)) (point))
                                          end)))
            (when isearchp-hide-whitespace-before-comment-flag ; Hide preceding whitespace.
              (when (looking-back "\n?\\s-*" nil 'GREEDY) (setq cbeg  (match-beginning 0)))
              ;; First line: Hide newline after comment.
              (when (and (= cbeg (point-min))  (= (char-after cend) ?\n))
                (setq cend  (min (1+ cend) end))))
            (when (and cbeg cend)
              (put-text-property cbeg cend 'invisible (eq 'hide hide/show)))
            (goto-char (setq start  (or cend  end)))))))))

(defun isearchp-toggle-ignoring-comments (&optional msgp) ; Bound to `C-M-;' during Isearch.
  "Toggle the value of option `isearchp-ignore-comments-flag'.
If option `ignore-comments-flag' is defined (in library
`hide-comnt.el') then it too is toggled.
Bound to `C-M-;' during Isearch."
  (interactive "p")
  (setq isearchp-ignore-comments-flag  (not isearchp-ignore-comments-flag))
  (when (boundp 'ignore-comments-flag) (setq ignore-comments-flag  (not ignore-comments-flag)))
  (when msgp (message "Ignoring comments is now %s" (if isearchp-ignore-comments-flag "ON" "OFF"))))

;--------------

(defun isearchp-thing (new thing beg end property &optional predicate transform-fn msgp)
  "Search within THING search contexts.
That is, each zone of text searched is a THING.
Enter the type of THING to search: `sexp', `sentence', `list',
`string', `comment', etc.

You can alternatively choose to search, not the THINGs as search
contexts, but the non-THINGs (non-contexts), that is, the buffer text
that is outside THINGs.  To do this, use
`C-M-~' (`isearchp-toggle-completing-domain') during Isearch.

Possible THINGs are those for which
`isearchp-bounds-of-thing-at-point' returns non-nil (and for which the
bounds are not equal: an empty thing).  This does not include every
THING that is defined as a thing-at-point type.

You are prompted for the type of THING to search.

If user option `isearchp-ignore-comments-flag' is nil then include
THINGs located within comments.  Non-nil (the default value) means to
ignore things inside comments for searching.

This means also that you cannot search comments as THING search
contexts if `isearchp-ignore-comments-flag' is non-nil.  (You can,
however, search non-comments even if it is non-nil.)  If THING is
`comment' (and you are not searching the complement zones) then this
command automatically turns option `isearchp-ignore-comments-flag'
OFF.  You can use `C-M-;' toggle this option anytime during Isearch,
but to see the effect you will need to invoke Isearch again.

If you do not use a prefix argument then you are prompted also for a
PREDICATE (Boolean function) that acceptable things must satisfy.  It
is passed the thing plus its bounds, in the same form as the cons that
is returned by `isearchp-next-visible-thing-and-bounds'.

If you use a prefix arg, and if the last property search also searched
THING contexts, then search the contexts again, using the same
predicate (if any).

In either case, the search contexts are created as zones of text with
text property `isearchp-thing-THING' of value (THING . PREDICATE).

NOTE:

1. For best results, use also library `thingatpt+.el'.

2. In some cases it can take a while to compute the THING search
   contexts.  Use the command on an active region when you do not need
   to search THINGS throughout the entire buffer.

3. In `nxml-mode', remember that option `nxml-sexp-element-flag'
   controls what a `sexp' means.  To use whole XML elements as search
   contexts, set the option to t, not nil.  (This is already done for
   the predefined Isearch+ commands.)

4. The buffer or region scan moves forward a THING at a time.  In
   particular, if either PREDICATE or TRANSFORM-FN disqualifies the
   thing being scanned currently, then scanning skips forward to the
   next thing.  The scan does not dig inside the current thing to look
   for a qualified THING.

Non-interactively, these are the arguments:

NEW:          Whether to define new search contexts or reuse contexts.
THING:        The type of things to search.
BEG and END:  The search limits.
PROPERTY:     Symbol `isearchp-thing-THING', where THING is the type.
PREDICATE:    A predicate to filter search contexts.
TRANSFORM-FN: A function to apply to each thing plus its bounds.  It
              returns the actual target to search in place of THING.
              Its argument is the same as the argument of PREDICATE.

              TRANSFORM-FN returns a replacement search context for
              the thing plus its bounds, in the same form: a cons
              (STRING START . END), where STRING is the search-hit
              string and START and END are its bounds).
MSGP:         Non-nil means show status/patience message."
  (interactive (cons (not current-prefix-arg) (isearchp-thing-read-args)))
  (when (or new
            (not (consp (car isearchp-property-values)))
            (not (equal (caar isearchp-property-values) thing))
            (not (eq isearchp-property-prop property))
            (not (isearchp-text-prop-present-p beg end property (cons thing predicate))))
    (isearchp-thing-define-contexts thing beg end property predicate nil msgp))
  (isearchp-property-forward '(4)))

(defun isearchp-thing-regexp (new thing beg end property &optional predicate transform-fn msgp)
  "Regexp search within THING search contexts.
Same as `isearchp-thing', but with regexp searching."
  (interactive (cons (not current-prefix-arg) (isearchp-thing-read-args)))
  (when (or new
            (not (consp (car isearchp-property-values)))
            (not (equal (caar isearchp-property-values) thing))
            (not (eq isearchp-property-prop property))
            (not (isearchp-text-prop-present-p beg end property (cons thing predicate))))
    (isearchp-thing-define-contexts thing beg end property predicate nil msgp))
  (isearchp-property-forward-regexp '(4)))

(defun isearchp-thing-define-contexts (thing beg end property &optional predicate transform-fn msgp)
  "Define search contexts for future thing searches.
This command does not actually search the contexts.  For that, use
`isearchp-thing(-regexp)' or `isearchp-property-forward'."
  (interactive (isearchp-thing-read-args))
  (when msgp (message "Scanning for thing: `%s'..." thing))
  (isearchp-thing-scan beg end thing property predicate transform-fn)
  (setq isearchp-property-prop    property
        isearchp-property-type    'text
        isearchp-property-values  (list (cons thing predicate)))
  (when msgp (message "Scanning for thing: `%s'...done" thing)))

(defun isearchp-thing-read-args ()
  "Read and return arguments for `isearchp-thing*'.
The list of args returned is:
 THING, the type of things to search
 BEG and END, the active region limits or the buffer limits
 PROPERTY, the symbol `isearchp-thing-THING'
 PREDICATE, a predicate to filter search contexts
 TRANSFORM-FN (nil is returned always)
 MSGP (non-nil is returned always)
See `isearchp-thing' for a description of the prompting."
  (let* ((thng     (intern
                    (completing-read
                     (format "%shing (type): " (if isearchp-complement-domain-p "*NOT* t" "T"))
                     (isearchp-things-alist) nil nil nil nil (symbol-name isearchp-last-thing-type))))
         (IGNORE   (when (and (eq thng 'comment)  isearchp-ignore-comments-flag
                              (not isearchp-complement-domain-p))
                     (setq isearchp-ignore-comments-flag  nil)
                     (message "Turned OFF ignoring comments.  Use `C-M-;' to turn it on again")
                     (sit-for 3)))
         (beg      (if (and transient-mark-mode  mark-active) (region-beginning) (point-min)))
         (end      (if (and transient-mark-mode  mark-active) (region-end) (point-max)))
         (valuesp  (and (consp (car isearchp-property-values))
                        (equal thng (caar isearchp-property-values))))
         (prop     (if (or (not current-prefix-arg)  (not valuesp))
                       (intern (format "%sthing-%s" isearchp-property-prop-prefix thng))
                     isearchp-property-prop))
         (pred     (if (or (not current-prefix-arg)  (not valuesp))
                       (read-from-minibuffer
                        "Predicate to filter search contexts: " nil read-expression-map t
                        (and (boundp 'function-name-history)  'function-name-history) "nil")
                     (cdar isearchp-property-values)))) ; Reuse last predicate.
    (list thng beg end prop pred nil 'MSG)))

(defun isearchp-thing-scan (beg end thing property &optional predicate transform-fn)
  "Scan buffer from BEG to END for things of type THING.
Create THING search contexts: Add text property PROPERTY to THING
matches.

Return non-nil if PROPERTY was added somewhere, nil otherwise.

If PREDICATE is non-nil then it is a predicate that acceptable things
must satisfy.  It is passed the thing plus its bounds, in the form of
the cons returned by `isearchp-next-visible-thing-and-bounds'.

If TRANSFORM-FN is non-nil then it is a function to apply to each
thing plus its bounds.  Its argument is the same as PREDICATE's.  It
returns the actual search-target to propertize, in place of THING.
That is, it returns the replacement for the thing plus its bounds, in
the same form: a cons (STRING START . END), where STRING is the search
hit string and START and END are its bounds).  It can also return nil,
in which case it acts as another predicate: the thing is not
propertized.

NOTE: The scan moves forward a THING at a time.  In particular, if
either PREDICATE or TRANSFORM-FN disqualifies the thing being scanned
currently, then scanning skips forward to the next thing.  The scan
does not dig inside the current thing to look for a qualified THING.

This function respects both `isearchp-search-complement-domain-p' and
`isearchp-ignore-comments-flag'."
  (unless (and beg  end) (setq beg  (point-min)
                               end  (point-max)))
  (unless (< beg end) (setq beg  (prog1 end (setq end  beg)))) ; Ensure BEG is before END.
  (when (stringp property) (setq property  (intern property)))
  (let ((last-beg      nil)
        (prop-value    nil)
        (added-prop-p  nil))
    (with-silent-modifications
      (isearchp-with-comments-hidden
       beg end
       (restore-buffer-modified-p nil)
       (condition-case-no-debug isearchp-thing-scan
           (save-excursion
             (goto-char (setq last-beg  beg)) ; `isearchp-next-visible-thing-and-bounds' uses point.
             (while (and last-beg  (< last-beg end))
               (while (and (< beg end)  (invisible-p beg)) ; Skip invisible, overlay or text.
                 (when (get-char-property beg 'invisible)
                   (setq beg  (next-single-char-property-change beg 'invisible nil end))))
               (let ((thg+bnds  (isearchp-next-visible-thing-and-bounds thing beg end)))
                 (if (not thg+bnds)
                     (setq beg  end)
                   (let* ((thg-beg       (cadr thg+bnds))
                          (thg-end       (cddr thg+bnds))
                          (tr-thg-beg    thg-beg)
                          (tr-thg-end    thg-end)
                          (hit-beg       tr-thg-beg)
                          (hit-end       tr-thg-end)
                          (hit-string    (buffer-substring-no-properties hit-beg hit-end))
                          (end-marker    (copy-marker hit-end))
                          (filteredp     (or (not predicate)
                                             (not thg+bnds)
                                             (funcall predicate thg+bnds)))
                          (new-thg+bnds  (and filteredp
                                              thg+bnds
                                              (if transform-fn (funcall transform-fn thg+bnds) thg+bnds))))
                     (cond ((and (not (string= "" hit-string)) ; No-op if empty hit.
                                 new-thg+bnds)
                            (when transform-fn
                              (setq hit-string  (car  new-thg+bnds)
                                    tr-thg-beg  (cadr new-thg+bnds)
                                    tr-thg-end  (cddr new-thg+bnds)
                                    end-marker  (copy-marker tr-thg-end)))
                            (when isearchp-ignore-comments-flag
                              (put-text-property 0 (length hit-string) 'invisible nil hit-string))
                            (unless (equal hit-beg hit-end)
                              (let ((buffer-mod  (buffer-modified-p)))
                                (setq prop-value  (cons thing predicate))
                                (put-text-property hit-beg hit-end property prop-value)
                                (setq added-prop-p  prop-value)
                                (isearchp-add/remove-dim-overlay hit-beg hit-end nil))))
                           (t
                            (remove-text-properties hit-beg hit-end (list property 'IGNORED))
                            (isearchp-add/remove-dim-overlay hit-beg hit-end 'ADD)))
                     (if thg-end
                         ;; $$$$$$
                         ;; The correct code here is (setq beg thg-end).  However, unless you use my
                         ;; library `thingatpt+.el' or unless Emacs bug #9300 gets fixed (and there
                         ;; is so far no sign that will happen), the correct code will loop forever.
                         ;; In this case, we move forward one char to prevent looping, but that means
                         ;; that the position just after a THING is considered to be covered by the
                         ;; THING (which is incorrect).
                         (setq beg  (if (featurep 'thingatpt+) thg-end (1+ thg-end)))
                       ;; If visible then no more things - skip to END.
                       (unless (invisible-p beg) (setq beg  end)))))
                 (setq last-beg  beg))))
         (error (error "%s" (error-message-string isearchp-thing-scan))))))
    ;; $$$$$$ (when added-prop-p (setq isearchp-last-prop+value (cons property prop-value)))
    added-prop-p))  ; Return indication of whether property was added.

;;--------------------------------------

;;; Same as `thgcmd-things-alist' in `thing-cmds.el'.
(if (fboundp 'thgcmd-things-alist)
    (defalias 'isearchp-things-alist 'thgcmd-things-alist)
  (defun isearchp-things-alist ()
    "Alist of most thing types currently defined.
Each is a cons (STRING), where STRING names a type of text entity for
which there is a either a corresponding `forward-'thing operation, or
corresponding `beginning-of-'thing and `end-of-'thing operations.  The
list includes the names of the symbols that satisfy
`isearchp-defined-thing-p', but with these excluded: `thing', `buffer',
`point'."
    (let ((types  ()))
      (mapatoms
       (lambda (tt)
         (when (isearchp-defined-thing-p tt) (push (symbol-name tt) types))))
      (dolist (typ  '("thing" "buffer" "point")) ; Remove types that do not make sense.
        (setq types (delete typ types)))
      (setq types  (sort types #'string-lessp))
      (mapcar #'list types))))

;;; Same as `thgcmd-defined-thing-p' in `thing-cmds.el'.
(if (fboundp 'thgcmd-defined-thing-p)
    (defalias 'isearchp-defined-thing-p 'thgcmd-defined-thing-p)
  (defun isearchp-defined-thing-p (thing)
    "Return non-nil if THING (type) is defined as a thing-at-point type."
    (let ((forward-op    (or (get thing 'forward-op)  (intern-soft (format "forward-%s" thing))))
          (beginning-op  (get thing 'beginning-op))
          (end-op        (get thing 'end-op))
          (bounds-fn     (get thing 'bounds-of-thing-at-point))
          (thing-fn      (get thing 'thing-at-point)))
      (or (functionp forward-op)
          (and (functionp beginning-op)  (functionp end-op))
          (functionp bounds-fn)
          (functionp thing-fn)))))

(defun isearchp-bounds-of-thing-at-point (thing &optional syntax-table)
  "`thingatpt+.el' version of `bounds-of-thing-at-point', if possible.
`tap-bounds-of-thing-at-point' if defined, else
`bounds-of-thing-at-point'.
if non-nil, set SYNTAX-TABLE for the duration."
  (if (fboundp 'tap-bounds-of-thing-at-point)
      (tap-bounds-of-thing-at-point thing syntax-table)
    (if (and (fboundp 'with-syntax-table)  (syntax-table-p syntax-table)) ; Emacs 21+.
        (with-syntax-table syntax-table (bounds-of-thing-at-point thing))
      (bounds-of-thing-at-point thing)))) ; Punt - ignore any SYNTAX-TABLE arg.

(defun isearchp-next-visible-thing-and-bounds (thing start end)
  "Return the next visible THING and its bounds.
Start at BEG and end at END, when searching for THING.
Return (THING THING-START . THING-END), with THING-START and THING-END
 the bounds of THING.  Return nil if no such THING is found.

The \"visible\" in the name refers to ignoring things that are within
invisible text, such as hidden comments.

You can toggle hiding of comments using `C-M-;' during Isearch, but
depending on when you do so you might need to invoke the current
command again.."
  (save-excursion (isearchp-next-visible-thing thing start end)))

;; Simple version of `previous-visible-thing' from `thing-cmds.el'.
;;
(defun isearchp-previous-visible-thing (thing start &optional end)
  "Same as `isearchp-next-visible-thing', except it moves backward."
  (interactive
   (list (or (and (memq last-command '(isearchp-next-visible-thing isearchp-previous-visible-thing))
                  isearchp-last-thing-type)
             (prog1 (intern (completing-read "Thing (type): " (isearchp-things-alist) nil nil nil nil
                                             (symbol-name isearchp-last-thing-type)))))
         (point)
         (if (and mark-active  (not (eq (region-beginning) (region-end))))
             (min (region-beginning) (region-end))
           (point-min))))
  (if (interactive-p)
      (isearchp-with-comments-hidden start end (isearchp-next-visible-thing thing start end 'BACKWARD))
    (isearchp-next-visible-thing thing start end 'BACKWARD)))

;; Simple version of `next-visible-thing' from `thing-cmds.el'.
;;
(defun isearchp-next-visible-thing (thing &optional start end backward)
  "Go to the next visible THING.
Start at START.  If END is non-nil then look no farther than END.
Interactively:
 - START is point.
 - If the region is not active, END is the buffer end.  If the region
   is active, END is the region end: the greater of point and mark.

Ignores (skips) comments if `isearchp-ignore-comments-flag' is
non-nil.  You can toggle this ignoring of comments using `C-M-;'
during Isearch, but depending on when you do so you might need to
invoke the current command again.

If you use this command or `isearchp-previous-visible-thing'
successively, even mixing the two, you are prompted for the type of
THING only the first time.  You can thus bind these two commands to
simple, repeatable keys (e.g. `f8', `f9'), to navigate among things
quickly.

Non-interactively, THING is a symbol, and optional arg BACKWARD means
go to the previous thing.

Return (THING THING-START . THING-END), with THING-START and THING-END
the bounds of THING.  Return nil if no such THING is found."
  (interactive
   (list (or (and (memq last-command '(isearchp-next-visible-thing isearchp-previous-visible-thing))
                  isearchp-last-thing-type)
             (prog1 (intern (completing-read "Thing (type): " (isearchp-things-alist) nil nil nil nil
                                             (symbol-name isearchp-last-thing-type)))))
         (point)
         (if (and mark-active  (not (eq (region-beginning) (region-end))))
             (max (region-beginning) (region-end))
           (point-max))))
  (setq isearchp-last-thing-type  thing)
  (unless start (setq start  (point)))
  (unless end   (setq end    (if backward (point-min) (point-max))))
  (cond ((< start end) (when backward (setq start  (prog1 end (setq end  start)))))
        ((> start end) (unless backward (setq start  (prog1 end (setq end  start))))))
  (if (interactive-p)
      (isearchp-with-comments-hidden start end (isearchp-next-visible-thing-1 thing start end backward))
    (isearchp-next-visible-thing-1 thing start end backward)))

;;; Same as `thgcmd-next-visible-thing-1' in `thing-cmds.el'.
(if (fboundp 'thgcmd-next-visible-thing-1)
    (defalias 'isearchp-next-visible-thing-1 'thgcmd-next-visible-thing-1)
  (defun isearchp-next-visible-thing-1 (thing start end backward)
    "Helper for `isearchp-next-visible-thing'.  Get thing past point."
    (let ((thg+bds  (isearchp-next-visible-thing-2 thing start end backward)))
      (if (not thg+bds)
          nil
        ;; $$$$$$ Which is better, > or >=, < or <=, for the comparisons?
        ;;        Seems that < is better than <=, at least for `icicle-search-thing':
        ;;        for XML elements and lists, <= misses the first one.
        ;; $$$$$$ No, I do not think that is the case (anymore).
        ;;        <= is OK and is needed for interactive use of `isearchp-next-visible-thing'.
        (while (and thg+bds  (if backward (> (cddr thg+bds) (point)) (<= (cadr thg+bds) (point))))
          (if backward
              (setq start  (max end (1- (cadr thg+bds))))
            (setq start  (min end (1+ (cddr thg+bds)))))
          (setq thg+bds  (isearchp-next-visible-thing-2 thing start end backward)))
        (when thg+bds (goto-char (cadr thg+bds)))
        thg+bds))))

;;; Same as `thgcmd-next-visible-thing-2' in `thing-cmds.el'.
(if (fboundp 'thgcmd-next-visible-thing-2)
    (defalias 'isearchp-next-visible-thing-2 'thgcmd-next-visible-thing-2)
  (defun isearchp-next-visible-thing-2 (thing start end &optional backward)
    "Helper for `isearchp-next-visible-thing-1'.  Thing might not be past START."
    (and (not (= start end))
         (save-excursion
           (let ((bounds  nil))
             ;; If BACKWARD, swap START and END.
             (cond ((< start end) (when   backward (setq start  (prog1 end (setq end  start)))))
                   ((> start end) (unless backward (setq start  (prog1 end (setq end  start))))))
             (catch 'isearchp-next-visible-thing-2
               (while (if backward (> start end) (< start end))
                 (goto-char start)
                 ;; Skip invisible text.
                 (when (and (if backward (> start end) (< start end))  (invisible-p start))
                   (setq start  (if (get-text-property start 'invisible) ; Text prop.
                                    (if backward
                                        (previous-single-property-change start 'invisible nil end)
                                      (next-single-property-change start 'invisible nil end))
                                  (if backward ; Overlay prop.
                                      (previous-overlay-change start)
                                    (next-overlay-change start))))
                   (goto-char start))
                 (when (and (setq bounds  (isearchp-bounds-of-thing-at-point thing))
                            (not (equal (car bounds) (cdr bounds)))) ; Not an empty thing, "".
                   (throw 'isearchp-next-visible-thing-2
                     (cons (buffer-substring (car bounds) (cdr bounds)) bounds)))
                 (setq start  (if backward (1- start) (1+ start))))
               nil))))))

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'isearch-prop)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; isearch-prop.el ends here
