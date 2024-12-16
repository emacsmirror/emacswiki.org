;;; thingatpt+.el --- Extensions to `thingatpt.el'.
;;
;; Filename: thingatpt+.el
;; Description: Extensions to `thingatpt.el'.
;; Author: Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 1996-2024, Drew Adams, all rights reserved.
;; Created: Tue Feb 13 16:47:45 1996
;; Version: 0
;; Last-Updated: Mon Dec 16 15:20:21 2024 (-0800)
;;           By: dradams
;;     Update #: 2427
;; URL: https://www.emacswiki.org/emacs/download/thingatpt%2b.el
;; Doc URL: https://www.emacswiki.org/emacs/ThingAtPointPlus
;; Keywords: extensions, matching, mouse
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x, 24.x, 25.x, 26.x, 27.x, 28.x, 29.x
;;
;; Features that might be required by this library:
;;
;;   `thingatpt'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Extensions to `thingatpt.el'.
;;
;;
;;  Commands defined here:
;;
;;    `find-fn-or-var-nearest-point', `forward-char-same-line',
;;    `forward-whitespace-&-newlines', `tap-put-thing-at-point-props',
;;    `tap-redefine-std-fns'.
;;
;;  User options defined here:
;;
;;    `tap-near-point-x-distance', `tap-near-point-y-distance'.
;;
;;  Non-interactive functions defined here:
;;
;;    `tap-bounds-of-color-at-point', `tap-bounds-of-form-at-point',
;;    `tap-bounds-of-form-nearest-point',
;;    `tap-bounds-of-list-at-point',
;;    `tap-bounds-of-list-contents-at-point',
;;    `tap-bounds-of-list-nearest-point',
;;    `tap-bounds-of-number-at-point',
;;    `tap-bounds-of-number-at-point-decimal',
;;    `tap-bounds-of-number-at-point-decimal-whole',
;;    `tap-bounds-of-number-at-point-hex',
;;    `tap-bounds-of-sexp-at-point',
;;    `tap-bounds-of-sexp-nearest-point',
;;    `tap-bounds-of-string-at-point',
;;    `tap-bounds-of-string-contents-at-point',
;;    `tap-bounds-of-symbol-at-point',
;;    `tap-bounds-of-symbol-nearest-point',
;;    `tap-bounds-of-thing-nearest-point',
;;    `tap-bounds-of-vector-at-point', `tap-color-at-point',
;;    `tap-color-nearest-point',
;;    `tap-color-nearest-point-with-bounds',
;;    `tap-define-aliases-wo-prefix', `tap-form-at-point-with-bounds',
;;    `tap-form-nearest-point', `tap-form-nearest-point-with-bounds',
;;    `tap-list-at/nearest-point-with-bounds',
;;    `tap-list-at-point-with-bounds', `tap-list-contents-at-point',
;;    `tap-list-contents-nearest-point', `tap-list-nearest-point',
;;    `tap-list-nearest-point-with-bounds',
;;    `tap-list-nearest-point-as-string', `tap-looking-at-p',
;;    `tap-looking-back-p', `tap-non-nil-symbol-name-at-point',
;;    `tap-non-nil-symbol-name-nearest-point',
;;    `tap-non-nil-symbol-nearest-point',
;;    `tap-number-at-point-decimal',
;;    `tap-number-at-point-decimal-whole', `tap-number-at-point-hex',
;;    `tap-number-nearest-point', `tap-read-from-whole-string',
;;    `tap-region-or-word-at-point',
;;    `tap-region-or-word-nearest-point',
;;    `tap-region-or-non-nil-symbol-name-nearest-point',
;;    `tap-sentence-nearest-point', `tap-sexp-at-point-with-bounds',
;;    `tap-sexp-nearest-point', `tap-sexp-nearest-point-with-bounds',
;;    `tap-string-at-point', `tap-string-contents-at-point',
;;    `tap-string-contents-nearest-point', `tap-string-match-p',
;;    `tap-string-nearest-point', `tap-symbol-at-point-with-bounds',
;;    `tap-symbol-name-at-point', `tap-symbol-name-nearest-point',
;;    `tap-symbol-nearest-point',
;;    `tap-symbol-nearest-point-with-bounds', `tap-thing-at-point',
;;    `tap-thing-at-point-as-string',
;;    `tap-thing-at-point-with-bounds',
;;    `tap-thing/form-nearest-point-with-bounds',
;;    `tap-thing-nearest-point',
;;    `tap-thing-nearest-point-with-bounds',
;;    `tap-unquoted-list-at-point', `tap-unquoted-list-nearest-point',
;;    `tap-unquoted-list-nearest-point-as-string',
;;    `tap-vector-at-point', `tap-vector-nearest-point',
;;    `tap-word-nearest-point',
;;
;;    plus the same functions without the prefix `tap-', if you invoke
;;    `tap-redefine-std-fns'.
;;
;;
;;  ***** NOTE: The following function defined in `thingatpt.el'
;;              has been REDEFINED HERE:
;;
;;    `number-at-point' (Emacs 25+).
;;
;;  A REMINDER (the doc strings are not so great):
;;
;;    These functions, defined in `thingatpt.el', all move point:
;;      `beginning-of-thing', `end-of-sexp', `end-of-thing',
;;      `forward-symbol', `forward-thing'.
;;
;;  For older Emacs releases that do not have the following functions,
;;  they are defined here as no-ops:
;;
;;  `constrain-to-field', `field-beginning', `field-end'.
;;
;;
;;  How To Use This Library
;;  =======================
;;
;;  End Users
;;  ---------
;;
;;  Load this library after loading the standard GNU file
;;  `thingatpt.el'.  You can put this in your init file (`~/.emacs'):
;;
;;    (eval-after-load 'thingatpt '(require 'thingatpt+))
;;
;;  (If you use Emacs 20, where the first arg to `eval-after-load'
;;  must be a file name, then use the appropriate file name for
;;  thingatpt.el.  See `C-h f eval-after-load' for arg FILE.)
;;
;;  That defines new functions and improved versions of some of the
;;  standard thing-at-point functions.  All such functions have the
;;  prefix `tap-', so they are not used by default in any way.
;;
;;  Requiring library `thingatpt+.el' does not, however, make Emacs
;;  use the improved functions.  Merely loading it does not change the
;;  behavior of thing-at-point features.
;;
;;  If you want functions defined here to be used for calls to
;;  standard Emacs functions that make use of the `thing-at-point' and
;;  `bounds-of-thing-at-point' symbol properties for standard thing
;;  types (e.g. `list'), then put this in your init file, instead:
;;
;;    (eval-after-load 'thingatpt
;;      '(when (require 'thingatpt+)
;;         (tap-put-thing-at-point-props))
;;
;;  Note that some of my other libraries, including Icicles,
;;  Bookmark+, `grep+.el', `replace+.el', and `strings.el', do exactly
;;  that.  Note too that `tap-put-thing-at-point-props' improves the
;;  behavior of (thing-at-point 'list) - see below.
;;
;;  A further step, which I recommend, is to use the `tap-' versions
;;  of standard functions, defined here, everywhere in place of those
;;  standard functions.  In other words, redefine the standard
;;  functions as the `tap-' versions defined here.  For example,
;;  redefine `bounds-of-thing-at-point' to do what
;;  `tap-bounds-of-thing-at-point' does.
;;
;;  (If you do that then you need not invoke
;;  `tap-put-thing-at-point-props' to pick up the versions defined
;;  here of standard functions.  The property values set by vanilla
;;  library `thingatpt.el' will be OK because the functions themselves
;;  will have been redefined in that case.)
;;
;;  To get the most out of this library, I recommend that you put
;;  (only) the following in your init file:
;;
;;    (eval-after-load 'thingatpt
;;      '(when (require 'thingatpt+)
;;         (tap-redefine-std-fns))
;;
;;  That makes all Emacs code that uses the following standard
;;  functions use the their versions that are defined here, not the
;;  vanilla versions defined in `thingatpt.el'.
;;
;;  `bounds-of-thing-at-point' - Better behavior.
;;                               Accept optional arg SYNTAX-TABLE.
;;  `form-at-point'            - Accept optional arg SYNTAX-TABLE.
;;  `list-at-point'            - Better behavior.
;;  `symbol-at-point'          - Use `emacs-lisp-mode-syntax-table'.
;;  `thing-at-point'           - Ensure it returns a string or nil.
;;                               Accept optional arg SYNTAX-TABLE.
;;  `thing-at-point-bounds-of-list-at-point'
;;                             - Better behavior.  Accept optional
;;                               args UP and UNQUOTEDP.
;;
;;
;;  Lisp Programmers
;;  ----------------
;;
;;  If you write code that uses some of the functions defined here,
;;  this section is for you.
;;
;;  You can use the functions defined in `thingatpt+.el' that have
;;  prefix `tap-' to obtain, for your code, the improvements they
;;  provide.  Doing only that has no effect on any code that calls
;;  vanilla thing-at-point functions (which have no prefix `tap-').
;;
;;  For convenience you can invoke `tap-define-aliases-wo-prefix' to
;;  provide alias functions that have the same names but without the
;;  prefix `tap-'.  This affects only functions defined here that have
;;  no vanilla counterpart, so the aliases do not collide with any
;;  standard Emacs functions.  This is just a naming convenience.
;;
;;  For example, you might do this:
;;
;;    (when (require 'thingatpt+ nil t)  ; (no error if not found)
;;      (tap-define-aliases-wo-prefix))
;;
;;  You can optionally enable the improvements defined here to have
;;  wider application, so that code that does not directly invoke the
;;  functions defined here nevertheless uses them indirectly.
;;
;;  You can, for example, put `tap-' functions on THING-type symbols
;;  as property `thing-at-point' or property
;;  `bounds-of-thing-at-point'.  That has the effect of using those
;;  `tap-' functions for those THING types only.
;;
;;  For example, to get the improvements for lists offered by
;;  `tap-list-at-point', you can do this:
;;
;;    (put 'list 'bounds-of-thing-at-point
;;         'tap-bounds-of-list-at-point)
;;    (put 'list 'thing-at-point 'tap-list-at-point)
;;
;;  That causes the vanilla thing-at-point functions to invoke those
;;  `tap-' functions when handling lists.  It has an effect only on
;;  lists, not on other THINGs.  This behavior happens because the
;;  generic vanilla functions `thing-at-point' and
;;  `bounds-of-thing-at-point' use those standard symbol properties.
;;
;;  For even wider application, that is, if you want all of the
;;  improvements defined here to be available generally, then you will
;;  also need to do ONE of the following (#1 or #2):
;;
;;  1. Call `tap-redefine-std-fns', to redefine standard functions.
;;
;;  2. Do BOTH of these things:
;;
;;    a. Call `tap-put-thing-at-point-props', to substitute `tap-'
;;       functions for standard functions as the values of symbol
;;       properties `thing-at-point' and `bounds-of-thing-at-point'.
;;
;;    b. Call the individual `tap-*' functions explicitly for each of
;;       the standard functions that would be redefined by
;;       `tap-redefine-std-fns'.  Or call standard functions that make
;;       use of property `thing-at-point' or
;;       `bounds-of-thing-at-point'.
;;
;;    This (#2) changes (improves) the behavior of things like
;;    (thing-at-point 'list), even though it does not redefine any
;;    standard functions.  Again, this is because functions
;;    `thing-at-point' and `bounds-of-thing-at-point' use symbol
;;    properties `thing-at-point' and `bounds-of-thing-at-point', and
;;    `tap-put-thing-at-point-props' changes those property values.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2024/12/16 dadams
;;     tap-bounds-of-string-at-point: Use Emacs 30+ definition.
;; 2023/07/23 dadams
;;     Added redefinition of Emacs 25+ misguided version of number-at-point.
;; 2019/10/25 dadams
;;     tap-bounds-of-string-at-point:
;;       Use char-syntax. Don't respect only " as the string delimiter.
;;     tap-string-at-point, tap(-bounds-of)-string-contents-at-point:
;;       Adjust doc string for that change.
;; 2019/09/19 dadams
;;     Added: tap(-bounds-of)-number-at-point-decimal-whole
;;     tap(-bounds-of)-number-at-point-(decimal|hex):
;;       Allow initial minus sign, decimal point, and fractional part.
;; 2018/06/30 dadams
;;     Added: tap-vector-at-point, tap-bounds-of-vector-at-point, tap-vector-nearest-point.
;; 2016/11/21 dadams
;;     Rename tap-thing-at-point to tap-thing-at-point-as-string and add new def of former.
;;     The new def follows vanilla Emacs in letting property thing-at-point return non-string.
;;     tap-form-at-point: Use tap-thing-at-point-as-string.
;;     tap-region-or-word-at-point: Use args STRICT REALLY-WORD with current-word.
;; 2016/11/04 dadams
;;     tap-thing-at-point: Added optional arg NO-PROPERTIES, per Emacs 24.4+.
;;     tap-form-at-point, tap-non-nil-symbol-name-at-point:
;;       Use nil for optional arg NO-PROPERTIES in call to tap-thing-at-point.
;; 2016/09/06 dadams
;;     Added: tap-read-from-whole-string.
;;     tap-form-at-point(-with-bounds): Use tap-read-from-whole-string.
;; 2016/08/30 dadams
;;     tap-string-match-p: Do NOT alias string-match-p, because that is a defsubst.
;; 2016/06/20 dadams
;;     tap-thing-at-point:
;;       Typo: convert result of funcall, not original THING, to string.  Thx to Tino Calancha.
;; 2015/08/23 dadams
;;     tap-list-at/nearest-point-with-bounds:
;;       Use (nth 3 (syntax-ppss)) for Emacs 25+ - see Emacs bug #20732.
;; 2014/08/22 dadams
;;     tap-looking-at-p: Do not defalias to Emacs looking-at-p because that is a defsubst.
;; 2014/06/07 dadams
;;     Added: tap-bounds-of-list-contents-at-point, tap-list-contents-at-point,
;;            tap-list-contents-nearest-point.
;;     Put tap-bounds-of-(string|list)-contents-at-point as bounds-of-thing-at-point property.
;; 2013/09/20 dadams
;;     Added: tap-bounds-of-string-contents-at-point, tap-string-contents-at-point,
;;            tap-string-contents-nearest-point.
;;     tap-define-aliases-wo-prefix: Updated for new functions.
;;     tap-bounds-of-string-at-point (incompatible change: include " delimiters):
;;       Include " chars in string returned.  Return the string also when point on ending ".
;; 2013/09/13 dadams
;;     tap-thing/form-nearest-point-with-bounds:
;;       Do not skip looping over chars in same line when eobp.
;; 2012/11/10 dadams
;;     Added: tap(-bounds-of)-color-at-point, tap-color-nearest-point(-with-bounds).
;;     tap-word-nearest-point: Corrected doc string: returns nil if none found.
;; 2012/08/24 dadams
;;     Added: tap-string-match-p, tap-looking-at-p, tap-looking-back-p.
;;     tap-list-at/nearest-point-with-bounds: Handle point inside a string.
;;     tap-number-at-point-(hex|decimal): Use tap-string-match-p.
;; 2012/08/22 dadams
;;     Added: tap-bounds-of-number-at-point(-decimal|-hex).
;;     tap-thing-at-point, tap-bounds-of-thing-at-point-1:  Check first the tap-* property.
;;     For things (unquoted-)list, (non-nil-)symbol-name, region-or-word,
;;      (decimal-|hex-)number, string:
;;         put tap-* properties also.
;;     tap-put-thing-at-point-props: Use tap-bounds-of-number-at-point, not lambda.
;; 2012/08/21 dadams
;;     Added: tap-put-thing-at-point-props.
;;     Moved puts for list and number to tap-put-thing-at-point-props.
;;     tap-define-aliases-wo-prefix: Return non-nil so can use in Boolean test.
;; 2012/08/19 dadams
;;     Added: tap-symbol-name-at-point.
;;     tap(-bounds-of)-symbol-at-point(-with-bounds):
;;       Removed useless arg NON-NIL.  Adjust calls to them accordingly.
;;     tap-thing-at-point: Ensure it returns a string (or nil).
;;     tap-non-nil-symbol-name-at-point:
;;       Redefine, using tap-thing-at-point with emacs-lisp-mode-syntax-table.
;;     tap(-non-nil)-symbol-name-nearest-point: Return nil, not "", if none found.
;;     tap-sexp-nearest-point: Corrected: pass nil PREDICATE arg.
;;     Doc string improvements.
;; 2012/08/18 dadams
;;     tap-define-aliases-wo-prefix: Return non-nil so can use in Boolean guards.
;;     word-nearest-point -> tap-word-nearest-point (typo).
;; 2012/08/17 dadams
;;     Added: tap-define-aliases-wo-prefix, tap-redefine-std-fns.
;;     Added group thing-at-point-plus.  Use for defcustoms.
;;     Renamed fns & vars, adding prefix tap-.
;;     Do not redefine std stuff, except in tap-define-aliases-wo-prefix, tap-redefine-std-fns.
;; 2012/02/18 dadams
;;     thing/form-nearest-point-with-bounds:
;;       Fixed infloop: set [be]obp when finished sole line in both directions.
;; 2011/09/06 dadams
;;     thing/form-nearest-point-with-bounds: If only one line then do not try to access others.
;;     bounds-of-thing-at-point-1, thing-at-point, thing/form-nearest-point-with-bounds:
;;       Respect field boundaries.
;;     Define constrain-to-field, field-(beginning|end) as no-ops for older Emacs releases.
;; 2011/08/30 dadams
;;     Added: region-or-non-nil-symbol-name-nearest-point.
;;     region-or-*: Use region only if transient-mark-mode, non-empty (and active).
;; 2011/08/17 dadams
;;     list-at/nearest-point-with-bounds:
;;       Don't count `foo or 'foo as a list, i.e., (` foo) or (quote foo).
;; 2011/08/14 dadams
;;     bounds-of-thing-at-point-1:
;;       Tests for end need to use <, not <=.  If past the THING then should return nil.
;; 2011/07/08 dadams
;;     Removed: list-at/nearest-point.
;;     Added: (list|sexp)-(at|nearest)-point-with-bounds,
;;            bounds-of-(list|sexp)-(at|nearest)-point, list-at/nearest-point-with-bounds.
;;     (unquoted-)list-(at|nearest)-point(-as-string):
;;       Redefined using list-(at|nearest)-point-with-bounds.
;;     (put 'list 'bounds-of-thing-at-point 'bounds-of-list-at-point) - not nil.
;; 2011/05/24 dadams
;;     Added: (bounds-of-)string-at-point, string-nearest-point.
;; 2011/05/21 dadams
;;     bounds-of-thing-at-point-1: Synchronized with vanilla Emacs fix for bug #8667.
;; 2011/05/13 dadams
;;     Added redefinition of bounds-of-thing-at-point - fixed bug #8667.
;;       Removed old-bounds-of-thing-at-point.  Added: bounds-of-thing-at-point-1.
;;     Added: forward-whitespace-&-newlines.
;;     Added (put 'thing-at-point *) for unquoted-list, non-nil-symbol-name.
;;     Removed old eval-when-compile for Emacs before Emacs 20.
;; 2011/05/07 dadams
;;     Added: number-at-point-(decimal|hex) and aliases.
;;     Put (bounds-of-)thing-at-point properties: (hex-|decimal-)number-at-point.
;; 2011/05/05 dadams
;;     (put 'list 'bounds-of-thing-at-point nil)  See Emacs bug #8628.
;;     (put 'list 'thing-at-point 'list-at-point) - not really needed, though.
;;     bounds-of-thing-at-point: Mention in doc string that pre-Emacs 23 is buggy.
;; 2011/01/20 dadams
;;     *list-*-point: Improved doc strings.
;; 2011/01/04 dadams
;;     Removed autoload cookies from non def* sexps and non-interactive fns.
;;     Added autoload cookies for defcustom.
;; 2010/12/17 dadams
;;     Added: (unquoted-)list-(at|nearest)-point, list-at/nearest-point,
;;            unquoted-list-nearest-point-as-string.
;;     list-nearest-point: Redefined using list-at/nearest-point.
;; 2010/12/10 dadams
;;     form-at-point-with-bounds:
;;       Moved condition-case to around whole.  Let sexp be any format of nil.
;; 2010/01/24 dadams
;;     Added: region-or-word-nearest-point.
;; 2008/10/22 dadams
;;     Added: region-or-word-at-point.  Thx to Richard Riley.
;; 2007/07/15 dadams
;;     Added: thing/form-nearest-point-with-bounds,
;;            non-nil-symbol(-name)-(at|nearest)-point, near-point-(x|y)-distance.
;;     (thing|form)-nearest-point-with-bounds:
;;       Use thing/form-nearest-point-with-bounds, which: (1) accepts PRED arg,
;;         (2) respects near-point-(x|y)-distance, (3) fixed some logic.
;;     form-at-point-with-bounds:
;;       Distinguish between nil (no find) and "nil" object found.
;;     (bounds-of-)symbol-(at|nearest)-point(-with-bounds), :
;;       Added optional non-nil arg.
;;     Added beginning-op, end-op, and forward-op for defun type.
;; 2006/12/08 dadams
;;     Added: find-fn-or-var-nearest-point.
;; 2006/05/16 dadams
;;     Only require cl (at compile time) for Emacs < 20.
;;     Replace incf by setq...1+.
;; 2005/12/17 dadams
;;     symbol-name-nearest-point, form-at-point-with-bounds:
;;       Treat nil as legitimate symbol.
;; 1996/06/11 dadams
;;     bounds-of-symbol-at-point, bounds-of-symbol-nearest-point,
;;       symbol-at-point, symbol-at-point-with-bounds,
;;       symbol-name-nearest-point, symbol-nearest-point,
;;       symbol-nearest-point-with-bounds: No longer use a syntax-table
;;       arg.  Always dealing with elisp symbols, so use
;;       emacs-lisp-mode-syntax-table.
;; 1996/03/20 dadams
;;     1. Added redefinitions of thing-at-point, form-at-point, with optional
;;        syntax table arg.
;;     2. Added: thing-nearest-point-with-bounds,
;;        bounds-of-thing-nearest-point, thing-nearest-point,
;;        form-nearest-point-with-bounds,
;;        bounds-of-form-nearest-point, form-nearest-point,
;;        word-nearest-point, sentence-nearest-point,
;;        sexp-nearest-point, number-nearest-point,
;;        list-nearest-point.
;;     3. symbol-at-point: Added optional syntax table arg.
;;     4. symbol-nearest-point-with-bounds: Now defined in terms of
;;        form-nearest-point-with-bounds.
;;     5. bounds-of-form-at-point: Added args THING and PRED.
;; 1996/03/20 dadams
;;     1. Added redefinition of bounds-of-thing-at-point: New arg SYNTAX-TABLE.
;;     2. thing-at-point-with-bounds, form-at-point-with-bounds,
;;        bounds-of-form-at-point, symbol-at-point-with-bounds,
;;        bounds-of-symbol-at-point, symbol-nearest-point-with-bounds,
;;        bounds-of-symbol-nearest-point, symbol-nearest-point,
;;        symbol-name-nearest-point: New arg SYNTAX-TABLE.
;; 1996/03/08 dadams
;;     1. Added: thing-at-point-with-bounds, form-at-point-with-bounds,
;;        bounds-of-form-at-point, symbol-at-point-with-bounds,
;;        bounds-of-symbol-at-point
;;     2. symbol-at-point: 2nd arg ('symbolp) to form-at-point to ensure interned.
;;     3. Added: symbol-nearest-point-with-bounds, symbol-name-nearest-point,
;;        bounds-of-symbol-nearest-point, symbol-nearest-point.
;;     4. symbol-nearest-point-with-bounds: Use symbol-at-point-with-bounds, not
;;        bounds-of-thing-at-point.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
