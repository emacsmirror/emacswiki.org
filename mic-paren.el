;;; mic-paren.el --- advanced highlighting of matching parentheses.

;; Version: 3.7 - 2001-12-21
;; Author: Mikael Sjödin (mic@docs.uu.se)
;;         Klaus Berndl  <berndl@sdm.de>
;; Keywords: languages, faces, parenthesis, matching
;;
;; Additional info:
;; Copyright (C) 1997 Mikael Sjödin (mic@docs.uu.se)
;; Maintenance and development (since v2.1): Klaus Berndl <berndl@sdm.de>
;; Original author: Mikael Sjödin  --  mic@docs.uu.se
;; Additional code by: Vinicius Jose Latorre <vinicius@cpqd.br>
;;                     Steven L Baur <steve@xemacs.org>
;;                     Klaus Berndl  <berndl@sdm.de>
;;
;; mic-paren.el is free software
;; 
;; This file is *NOT* (yet?) part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary

;; ----------------------------------------------------------------------
;; Short Description:
;;
;; Load this file, activate it and Emacs will display highlighting on
;; whatever parenthesis (and paired delimiter if you like this) matches
;; the one before or after point. This is an extension to the paren.el
;; file distributed with Emacs. The default behaviour is similar to
;; paren.el but more sophisticated. Normally you can try all default
;; settings to enjoy mic-paren.
;;
;; Or - if you are a LaTeX writer like the current maintainer - try the
;; following additional setup in your .emacs:
;;
;; ;; In LaTeX-mode we want this
;; (add-hook 'LaTeX-mode-hook
;;           (function (lambda ()
;;                       (paren-toggle-matching-quoted-paren 1)
;;                       (paren-toggle-matching-paired-delimiter 1))))
;;
;; Or - if you are programming in C like languages - try also:
;; (add-hook 'c-mode-common-hook
;;           (function (lambda ()
;;                        (paren-toggle-open-paren-context 1))))
;; ----------------------------------------------------------------------

;; ----------------------------------------------------------------------
;; Installation:
;;
;; o Place this file in a directory in your 'load-path and byte-compile
;;   it. You can surely ignore the warnings.
;; o Put the following in your .emacs file:
;;   (GNU Emacs supports mic-paren only within a window-system but XEmacs
;;   supports mic-paren also without X)
;;   (when (or (string-match "XEmacs\\|Lucid" emacs-version) window-system)
;;      (require 'mic-paren) ; loading
;;      (paren-activate)     ; activating
;;      ;;; set here any of the customizable variables of mic-paren:
;;      ;;; ...
;;   )
;; o Restart your Emacs. mic-paren is now installed and activated!
;; o To list the possible customizations enter `C-h f paren-activate' or
;;   go to the customization group `mic-paren-matching'.

;; ----------------------------------------------------------------------
;; Long Description:
;;
;; mic-paren.el is an extension and replacement to the packages paren.el
;; and stig-paren.el for Emacs. When mic-paren is active Emacs normal
;; parenthesis matching is deactivated. Instead parenthesis matching will
;; be performed as soon as the cursor is positioned at a parenthesis. The
;; matching parenthesis (or the entire expression between the
;; parentheses) is highlighted until the cursor is moved away from the
;; parenthesis. Features include:
;; o Both forward and backward parenthesis matching (simultaneously if
;;   cursor is between two expressions).
;; o Indication of mismatched parentheses.
;; o Recognition of "escaped" (also often called "quoted") parentheses.
;; o Option to match "escaped" parens too, especially in (La)TeX-mode
;;   (e.g. matches expressions like "\(foo bar\)" properly).
;; o Offers two functions as replacement for forward-sexp and
;;   backward-sexp which handle properly quoted parens (s.a.). These new
;;   functions can automatically be bounded to the original binding of
;;   the standard forward-sexp and backward-sexp functions.
;; o Option to activate matching of paired delimiter (i.e. characters with
;;   syntax '$'). This is useful for writing in LaTeX-mode for example.
;; o Option to select in which situations (always, never, if match, if
;;   mismatch) the entire expression should be highlighted or only the
;;   matching parenthesis.
;; o Message describing the match when the matching parenthesis is off-screen
;;   (vertical and/or horizontal). Message contains either the linenumber or
;;   the number of lines between the two matching parens. Option to select in
;;   which cases this message should be displayed.
;; o Optional delayed highlighting (useful on slow systems),
;; o Functions to activate/deactivate mic-paren.el are provided.
;; o Numerous options to control the behaviour and appearance of
;;   mic-paren.el.
;;
;; mic-paren.el was originally developed and tested under Emacs 19.28 -
;; 20.3. It should work on earlier and forthcoming Emacs versions. XEmacs
;; compatibility has been provided by Steven L Baur <steve@xemacs.org>.
;; Jan Dubois (jaduboi@ibm.net) provided help to get mic-paren to work in
;; OS/2. Mic-paren versions >= v2.1 are only tested with recent Emacsen
;; (FSF Emacs >= 20.3.1 and XEmacs >= 21.1) but should also work with
;; earlier versions of (X)Emacs.
;;
;; This file can be obtained from "The EmacsWiki" and here from the
;; packages-site: http://www.emacswiki.org/elisp/index.html

;; ----------------------------------------------------------------------
;; Available customizable options:
;; - `paren-priority'
;; - `paren-overlay-priority'
;; - `paren-sexp-mode'
;; - `paren-highlight-at-point'
;; - `paren-highlight-offscreen'
;; - `paren-display-message'
;; - `paren-message-linefeed-display'
;; - `paren-message-no-match'
;; - `paren-message-show-linenumber'
;; - `paren-message-truncate-lines'
;; - `paren-ding-unmatched'
;; - `paren-delay'
;; - `paren-dont-touch-blink'
;; - `paren-match-face'
;; - `paren-mismatch-face'
;; - `paren-no-match-paren'
;; - `paren-bind-modified-sexp-functions'
;; Available customizable faces:
;; - `paren-face-match'
;; - `paren-face-mismatch'
;; - `paren-face-no-match'
;; Available commands:
;; - `paren-activate'
;; - `paren-deactivate'
;; - `paren-toggle-matching-paired-delimiter'
;; - `paren-toggle-matching-quoted-paren'
;; - `paren-toggle-open-paren-context'
;; - `paren-forward-sexp'
;; - `paren-backward-sexp'
;; ----------------------------------------------------------------------

;; IMPORTANT NOTES (important for people who have customized mic-paren
;;                  from within elisp):
;; - In version >= 3.3 the prefix "mic-" has been removed from the
;;   command-names 'mic-paren-forward-sexp' and 'mic-paren-backward-sexp'.
;;   Now all user-functions and -options begin with the prefix "paren-"
;;   because this package should be a replacement of the other
;;   paren-packages like paren.el and stig-paren.el!
;; - In version >= 3.2 the prefix "mic-" has been removed from the
;;   command-names 'mic-paren-toggle-matching-quoted-paren' and
;;   'mic-paren-toggle-matching-paired-delimiter'.
;; - In versions >= 3.1 mic-paren is NOT auto. activated after loading.
;; - In versions >= 3.0 the variable 'paren-face' has been renamed to
;;   `paren-match-face'.

;; ----------------------------------------------------------------------
;; Versions:
;; v3.7    + Removed the message "You should be in LaTeX-mode!".
;;         + Fixed a bug in `paren-toggle-matching-quoted-paren'.
;;         + Fixed some misspellings in the comments and docs.
;;
;; v3.6    + Fixed a very small bug in `mic-paren-horizontal-pos-visible-p'.
;;         + The informational messages like "Matches ... [+28]" which are
;;           displayed if the matching paren is offscreen, do not longer
;;           wasting the log.
;;
;; v3.5    + No mic-paren-messages are displayed if we are in isearch-mode.
;;         + Matching quoted parens is switched on if entering a minibuffer.
;;           This is useful for easier inserting regexps, e.g. with
;;           `query-replace-regexp'. Now \(...\) will be highlighted in the
;;           minibuffer.
;;         + New option `paren-message-show-linenumber': You can determine the
;;           computation of the offscreen-message-linenumber: Either the
;;           number of lines between the two matching parens or the absolute
;;           linenumber (Thank you for the idea and a first implementation to
;;           Eliyahu Barzilay <eli@cs.bgu.ac.il>).
;;         + New option `paren-message-truncate-lines': If mic-paren messages
;;           should be truncated ot not (has only an effect in GNU Emacs 21).
;;           (Thank you for the idea and a first implementation to Eliyahu
;;           Barzilay <eli@cs.bgu.ac.il>).
;;
;; v3.4    + Corrected some bugs in the backward-compatibility for older
;;           Emacsen. Thanks to Tetsuo Tsukamoto <czkmt@remus.dti.ne.jp>.
;;
;; v3.3    + Now the priority of the paren-overlays can be customized
;;           (option `paren-overlay-priority'). For a description of the
;;           priority of an overlay see in the emacs-lisp-manual the node
;;           "Overlays". This option is mainly useful for experienced
;;           users which use many packages using overlays to perform their
;;           tasks.
;;         + Now you can determine what line-context will be displayed if
;;           the matching open paren is offscreen. In functional
;;           programming languages like lisp it is useful to display the
;;           following line in the echo-area if the opening matching paren
;;           has no preceding text in the same line.
;;           But in procedural languages like C++ or Java it is convenient
;;           to display the first previous non empty line in this case
;;           instead of the following line. Look at the new variable
;;           `paren-open-paren-context-backward' and the related toggling
;;           function `paren-toggle-open-paren-context' for a detailed
;;           description of this new feature.
;;         + In addition to the previous described new feature you can
;;           specify how a linefeed in the message (e.g. if the matching
;;           paren is offscreen) is displayed. This is mainly because the
;;           standard echo-area display of a linefeed (^J) is bad to read.
;;           Look at the option `paren-message-linefeed-display'.
;;         + Solved a little bug in the compatibility-code for Emacsen
;;           not supporting current customize-feature.
;;         + Removed the prefix "mic-" from the commands
;;           'mic-paren-forward-sexp' and 'mic-paren-backward-sexp'. For
;;           an explanation look at comments for version v3.2.
;;
;; v3.2    + The prefix "mic-" has been removed from the commands
;;           'mic-paren-toggle-matching-quoted-paren' and
;;           'mic-paren-toggle-matching-paired-delimiter'. This is because
;;           of consistency. Now all user-variables, -faces and -commands
;;           begin with the prefix "paren-" and all internal functions and
;;           variables begin with the prefix "mic-paren-".
;;         + Now you can exactly specify in which situations the whole
;;           sexp should be highlighted (option `paren-sexp-mode'):
;;           Always, never, if match or if mismatch. Tested with Gnus
;;           Emacs >= 20.3.1 and XEmacs >= 21.1.
;;
;; v3.1    + From this version on mic-paren is not auto. loaded. To
;;           activate it you must call `paren-activate' (either in your
;;           .emacs or manually with M-x). Therefore the variable
;;           `paren-dont-activate-on-load' is obsolet and has been
;;           removed.
;;         + Now mic-paren works also in older Emacsen without the
;;           custom-feature. If the actual custom-library is provided
;;           mic-paren use them and is full customizable otherwise normal
;;           defvars are used for the options.
;;         + Fix of a bug displaying a message if the matching paren is
;;           horizontal out of view.
;;         + All new features are now tested with XEmacs >= 21.1.6
;;
;; v3.0    + Checking if matching paren is horizontally offscreen (in case
;;           of horizontal scrolling). In that case the message is
;;           displayed in the echo-area (anlogue to vertical offscreen).
;;           In case of horizontal offscreen closing parenthesis the
;;           displayed message is probably wider than the frame/window. So
;;           you can only read the whole message if you are using a
;;           package like mscroll.el (scrolling long messages) in GNU
;;           Emacs.
;;         + Now full customizable, means all user-options and -faces now
;;           can be set with the custom-feature of Emacs. On the other
;;           side this means this version of mic-paren only works with an
;;           Emacs which provides the custom-package!
;;         + In case of the matching paren is offscreen now the displayed
;;           message contains the linenumber of the matching paren too.
;;         This version is only tested with Gnu Emacs >= 20.4 and not with
;;         any XEmacs!
;;         Implemented by Klaus Berndl <berndl@sdm.de>
;;
;; v2.3    No additional feature but replacing 'char-bytes and
;;         'char-before with 'mic-char-bytes and 'mic-char-before to
;;         prevent the global-namespace. Now the new features of v2.1
;;         and v2.2 are also tested with XEmacs!
;;
;; v2.2    Adding the new feature for matching paired delimiter. Not
;;         tested with XEmacs. Implemented by Klaus Berndl <berndl@sdm.de>
;;
;; v2.1    Adding the new feature for matching escaped parens too. Not
;;         tested with XEmacs. Implemented by Klaus Berndl <berndl@sdm.de>
;;
;; v2.0    Support for MULE and Emacs 20 multibyte characters added. Inspired
;;         by the suggestion and code of Saito Takaaki
;;         <takaaki@is.s.u-tokyo.ac.jp>
;;
;; v1.9    Avoids multiple messages/dings when point has not moved.  Thus,
;;	    mic-paren no longer overwrites messages in minibuffer.  Inspired by
;;	    the suggestion and code of Barzilay Eliyahu <eli@cs.bgu.ac.il>.
;;
;; v1.3.1  Some spelling corrected (from Vinicius Jose Latorre
;;	    <vinicius@cpqd.br> and Steven L Baur <steve@xemacs.org>)
;;
;; v1.3    Added code from Vinicius Jose Latorre <vinicius@cpqd.br> to
;;	    highlight unmatched parentheses (useful in minibuffer)
;;
;; v1.2.1  Fixed stuff to work with OS/2 emx-emacs
;;           - checks if x-display-colour-p is bound before calling it
;;           - changed how X/Lucid Emacs is detected
;;         Added automatic load of the timer-feature (+ variable to disable
;;         the loading)

;; TODO:
;;

;;; Code

(defvar mic-paren-version "3.7"
  "Version string for mic-paren.")

;;; ======================================================================
;; Compatibility stuff 
;; BLOB to make custom stuff work even without customize
(eval-and-compile
  (condition-case ()
      (require 'custom)
    (error nil))
  (unless (fboundp 'defgroup)
    (defmacro defgroup (&rest rest) nil))
  (unless (fboundp 'defcustom)
    (defmacro defcustom (sym val str &rest rest)
      (` (defvar (, sym) (, val) (, str)))))
  (unless (fboundp 'defface)
    (defmacro defface (sym val str &rest rest)
      (` (defvar (, sym) (make-face '(, sym)) (, str))))))

  
;;; ======================================================================
;;; here begin the user options

(defgroup mic-paren-matching nil
  "Showing advanced (un)matching of parens and expressions."
  :prefix "paren-"
  :group 'paren-matching)

(defcustom paren-priority 'both
  "*Defines the behaviour of mic-paren when point is between a
closing and an opening parenthesis. This means what should be done in a
situation like this: \(a b)\(c d)
                           ^
                         point
- 'close means highlight the parenthesis matching the close-parenthesis
  before the point \(highlight opening paren before 'a').
- 'open means highlight the parenthesis matching the open-parenthesis after
  the point \(highlight closing paren after 'd').
- 'both means highlight both parenthesis matching the parenthesis beside
  the point \(highlight opening before 'a' and closing after 'd')."
  :type '(choice (const :tag "Match close" close)
                 (const :tag "Match open" open)
                 (const :tag "Match both" both))
  :group 'mic-paren-matching)

(defcustom paren-overlay-priority 999
 "*Specify paren overlay priority \(Integer >= 0\). For a description of the
priority of an overlay see in the emacs-lisp manual the node Overlays.
Normally you don't want to change the default-value!"
 :set (function
       (lambda (symbol value)
        (set symbol (if (< value 0) (* -1 value) value))))
 :initialize 'custom-initialize-default
 :type 'integer
 :group 'mic-paren-matching)

(defcustom paren-sexp-mode nil
  "*Defines in which situations the whole sexp should be highlighted.
This means the whole s-expression between the matching parenthesis is
highlighted instead of only the matching/mismatching parenthesis.

- t: Always highlight the whole s-expression.
- nil: Never highlight the whole s-expression.
- 'match: Highlight the whole s-expression only if the parens match.
- 'mismatch: Highlight the whole s-expression only if the parens don't match."
  :type '(choice (const :tag "Never sexp-mode" nil)
                 (const :tag "Always sexp-mode" t)
                 (const :tag "If match" match)
                 (const :tag "If mismatch" mismatch))
  :group 'mic-paren-matching)


(defcustom paren-highlight-at-point t
  "*If non-nil and point is after a close parenthesis, both the close
and open parenthesis is highlighted. If nil, only the open parenthesis is
highlighted."
  :type '(choice (const :tag "Highlight both" t)
                 (const :tag "Highlight open" nil))
  :group 'mic-paren-matching)


(defcustom paren-highlight-offscreen nil
  "*If non-nil mic-paren will highlight text which is not visible in the
current buffer.

This is useful if you regularly display the current buffer in multiple
windows or frames. For instance if you use follow-mode \(by
andersl@csd.uu.se), however it may slow down your Emacs.

\(This variable is ignored \(treated as non-nil) if you set paren-sexp-mode to
non-nil.)"
  :type 'boolean
  :group 'mic-paren-matching)


(defcustom paren-display-message 'only
  "*Display message if matching parenthesis is off-screen.
Possible settings are:
- 'always: message is always displayed regardless if offscreen or not
- 'only:   message is only displayed when matching is offscreen
- 'never:  never a message is displayed."
  :type '(choice (const :tag "Display always" always)
                 (const :tag "Display if offscreen" only)
                 (const :tag "No Message display" never))
  :group 'mic-paren-matching)

(defcustom paren-message-linefeed-display " RET "
  "*How a linefeed in the matching paren context message is displayed.
There are three predefined values:
- Displays linefeeds with \" RET \" in the message.
- Displays linefeeds with a SPACE in the message.
- Displays linefeeds in the standard-form, means with \"^J\".
But you can also specify any user-defined string for it.

For further explanations about message displaying look at
`paren-display-message'."
  :type '(choice (const :tag "Display with \"RET\"" :value " RET ")
                 (const :tag "Display with a SPACE" :value " ")
                 (const :tag "Standard" :value "^J")
                 (string :tag "User defined"))
  :group 'mic-paren-matching)

(defcustom paren-message-show-linenumber 'sexp
  "*Determine the computation of the offscreen-message-linenumber.
If the matching paren is offscreen, then maybe a message with the context of
the matching paren and it´s linenumber is displayed \(depends on the setting
in `paren-display-message'). Here the computation of the linenumber can be
determined:
- 'sexp: Display the number of lines between the matching parens. Then the
         number of lines is displayed as negativ number if the matching paren
         is somewhere above. Otherwise the number has a positive sign.
- 'absolute: Display the absolute linenumber of the machting paren computed
             from the beginning of the buffer."
  :type '(choice (const :tag "Count accros sexp" sexp)
                 (const :tag "Absolute number" absolute))
  :group 'mic-paren-matching)


(defcustom paren-message-no-match t
  "*Display message if no matching parenthesis is found."
  :type '(choice (const :tag "Display message" t)
                 (const :tag "No message" nil))
  :group 'mic-paren-matching)


(defcustom paren-message-truncate-lines t
  "*Non nil means truncate lines for all messages mic-paren can display.
This option has only an effect with GNU Emacs 21.x!"
  :type 'boolean
  :group 'mic-paren-matching)


(defcustom paren-ding-unmatched nil
  "*Non nil means make noise if the cursor is at an unmatched
parenthesis or no matching parenthesis is found.
Even if nil, typing an unmatched parenthesis produces a ding."
  :type 'boolean
  :group 'mic-paren-matching)


(defcustom paren-delay nil
  "*This variable controls when highlighting is done.
The variable has different meaning in different versions of Emacs.

In Emacs 19.29 and below:
  This variable is ignored.

In Emacs 19.30:
  A value of nil will make highlighting happen immediately \(this may slow
  down your Emacs if running on a slow system). Any non-nil value will
  delay highlighting for the time specified by post-command-idle-delay.

In Emacs 19.31 and above:
  A value of nil will make highlighting happen immediately \(this may slow
  down your Emacs if running on a slow system). If not nil, the value
  should be a number \(possible a floating point number if your Emacs
  support floating point numbers). The number is the delay in seconds
  before mic-paren performs highlighting.

If you change this variable when mic-paren is active you have to
re-activate \(with M-x paren-activate) mic-paren for the change to take
effect."
  :type '(choice (number :tag "Delay time")
                 (const :tag "No delay" nil))
  :group 'mic-paren-matching)


(defcustom paren-dont-touch-blink nil
  "*Non-nil means not to change the value of blink-matching-paren
when mic-paren is activated of deactivated.
If nil mic-paren turns of blinking when activated and turns on blinking when
deactivated."
  :type 'boolean
  :group 'mic-paren-matching)


(defcustom paren-dont-load-timer (not (string-match "XEmacs\\|Lucid"
                                                    emacs-version))
  "*If non-nil mic-paren will not try to load the timer-feature when
loaded.

\(I have no idea why Emacs user ever want to set this to non-nil but I hate
packages which loads/activates stuff I don't want to use so I provide this
way to prevent the loading if someone doesn't want timers to be loaded.)"
  :type 'boolean
  :group 'mic-paren-matching)


(defcustom paren-bind-modified-sexp-functions t
  "*Automatic binding of the new sexp-functions to the old bindings.
If non nil mic-paren checks at load-time the keybindings for the functions
`forward-sexp' and `backward-sexp' and binds the modified sexp-functions
`paren-forward-sexp' and `paren-backward-sexp' to exactly these
bindings if and only if matching quoted/escaped parens is turned on by
`paren-toggle-matching-quoted-paren'. These new binding is done only
for the buffer local-key-map, therefore if you activate the quoted matching
only in some modes from within a hook only in these buffers the new binding
is active and 'in all other not.
If you deactivate the quoted matching feature by
`paren-toggle-matching-quoted-paren' then `forward-sexp' and
`backward-sexp' will be bound again to their original key-bindings!"
  :type 'boolean
  :group 'mic-paren-matching)

;;; ------------------------------
;;; Faces
;;; ------------------------------

(defface paren-face-match
  '((((class color)) (:background "turquoise"))
    (t (:background "gray")))
  "Mic-paren mode face used for a matching paren."
  :group 'faces
  :group 'mic-paren-matching)

(defface paren-face-mismatch
  '((((class color)) (:foreground "white" :background "purple"))
    (t (:reverse-video t)))
  "Mic-paren mode face used for a mismatching paren."
  :group 'faces
  :group 'mic-paren-matching)

(defface paren-face-no-match
  '((((class color)) (:foreground "black" :background "yellow"))
    (t (:reverse-video t)))
  "Mic-paren mode face used for an unmatched paren."
  :group 'faces
  :group 'mic-paren-matching)


(defcustom paren-match-face 'paren-face-match
  "*Face to use for showing the matching parenthesis."
  :type 'face
  :group 'mic-paren-matching)


(defcustom paren-mismatch-face 'paren-face-mismatch
  "*Face to use when highlighting a mismatched parenthesis."
  :type 'face
  :group 'mic-paren-matching)


(defcustom paren-no-match-face 'paren-face-no-match
  "*Face to use when highlighting an unmatched parenthesis."
  :type 'face
  :group 'mic-paren-matching)


;;; End of User Options:
;;; ======================================================================

;;; Below there are only variables and options which either should be not
;;; set directly but with toggle-functions or pure internal variables

(defvar paren-match-quoted-paren nil
  "*Non-nil causes to match properly quoted \(or escaped\) parens \(e.g. in
TeX-files, e.g. \"\\\(x-3y + z = 7\\\)\"\). FSF-Emacs can not match quoted
parens, so we must temporally deactivate the quoting until emacs has done
its sexp-parsing. Therefore emacs itself does not \(can not!\) take into
consideration if either both matched parens are quoted or none. But
nevertheless we do this! Only symmetric balanced parens are matched: means
either both matching parens must we quoted or none, otherwise they we will
be highlighted as mismatched.
This package offers also two slightly modified versions of forward-sexp
\(resp. backward-sexp\):
`paren-forward-sexp'\(`paren-backward-sexp'\). This versions can also
jump to escaped/quoted parens.
If this variable is not nil and `paren-bind-modified-sexp-functions' is set
to non nil then `paren-toggle-matching-quoted-paren' will also toggle
the original binding of `forward-sexp' \(resp. backward-sexp\) between the
original functions and the modified equivalents.

Do NOT set this variable directly but use
`paren-toggle-matching-quoted-paren' to activate/deactivate/toggle this
feature!. The best method is to do this in a mode hook, e.g.:
\(add-hook \'LaTeX-mode-hook
          \(function \(lambda \(\)
                      \(paren-toggle-matching-quoted-paren 1\)\)\)\)")

(make-variable-buffer-local 'paren-match-quoted-paren)

(defvar paren-match-paired-delimiter nil
"*If not nil then characters with syntax '$' \(means paired delimiter\)
will be matched if possible (e.g. in LaTeX \"$...$\" is equal with
\"\\(...\\)\"\) . Unlike to parens quoted/escaped paired delimiter will
never match.

Do NOT set this variable directly but use
`paren-toggle-matching-paired-delimiter' to activate/deactivate/toggle
this feature!. The best method is to do this in a mode hook, e.g.:
\(add-hook \'LaTeX-mode-hook
          \(function \(lambda \(\)
                      \(paren-toggle-matching-paired-delimiter 1\)\)\)\)")

(make-variable-buffer-local 'paren-match-paired-delimiter)

(defvar paren-open-paren-context-backward nil
"*Determines which context of the matching open paren will be displayed
if the matching open paren is offscreen or `paren-display-message' is
'always \(look at the documentation of `paren-display-message'\) and the
matching open paren has no previous text in the same line.
Meaning of the setting:
- nil: Contents of the **next** not empty and not whitespace-line will be
  displayed. This value is useful for example in functional programming
  languages like \(emacs\)lisp.
- not nil: Contents of the first **previous** not empty and not only
  whitespace-line will be displayed. This value is useful for example in
  procedural programming languages like C, C++, Java etc.

Lets take a look at a short example:
In languages like C++ we often have situations like
  if \(i > VALUE\)
  \{
      // some code
  \}
With a value non nil the displayed opening-brace context would be
\"if \(i > VALUE\)^J\{\" but with nil it would be \"\{^J // some code\"
which would be in C++ lesser useful as the non nil version.
\(The ^J stands for a newline in the buffer\).

Do NOT set this variable directly but use `paren-toggle-open-paren-context'
to change the value of this option!. The best method is to do this in a
mode hook, e.g.:
\(add-hook \'c-common-mode-hook
           \(function \(lambda \(\)
                         \(paren-toggle-open-paren-context 1\)\)\)\)")

(make-variable-buffer-local 'paren-open-paren-context-backward)

(defconst mic-paren-original-keybinding-of-sexp-functions
  (list (car (where-is-internal 'forward-sexp))
        (car (where-is-internal 'backward-sexp))))


;;; Compatibility.
;;; Try to make mic-paren work in different Emacs flavours.

;; XEmacs compatibility (mainly by Steven L Baur <steve@xemacs.org>)
(eval-and-compile
  (if (string-match "\\(Lucid\\|XEmacs\\)" emacs-version)
      (progn
        (fset 'mic-make-overlay 'make-extent)
        (fset 'mic-delete-overlay 'delete-extent)
        (fset 'mic-overlay-put 'set-extent-property)
        (defun mic-cancel-timer (timer) (delete-itimer timer))
        (fset 'mic-run-with-idle-timer 'start-itimer)
        )
    (fset 'mic-make-overlay 'make-overlay)
    (fset 'mic-delete-overlay 'delete-overlay)
    (fset 'mic-overlay-put 'overlay-put)
    (fset 'mic-cancel-timer 'cancel-timer)
    (fset 'mic-run-with-idle-timer 'run-with-idle-timer)
    ))

;; MULE and Emacs 20 multibyte char compatibility.
;; Implemented by defining dummys for functions which do not exist in vanilla
;; Emacs.

(if (fboundp 'char-bytes)
    (fset 'mic-char-bytes 'char-bytes)
  (defun mic-char-bytes (ch)
    "Returns 1 for all input CH.
Function defined by mic-paren to be compatible with multibyte Emacses."
    1))

(if (fboundp 'char-before)
    (fset 'mic-char-before 'char-before)
  (defun mic-char-before (pos)
    "Return character in current buffer preceding position POS.
POS is an integer or a buffer pointer.
If POS is out of range, the value is nil.
Function defined by mic-paren to be compatible with multibyte Emacses."
    (char-after (1- pos))))

(defun mic-paren-nolog-message (&rest args)
  "Works exactly like `message' but does not log the message"
  (let ((msg (cond ((or (null args)
                        (null (car args)))
                    nil)
                   ((null (cdr args))
                    (car args))
                   (t
                    (apply 'format args)))))
    ;; Now message is either nil or the formated string.
    (if (fboundp 'display-message)
        ;; XEmacs way of preventing log messages.
        (if msg
            (display-message 'no-log msg)
          (clear-message 'no-log))
      ;; Emacs way of preventing log messages.
      (let ((message-log-max nil))
        (if msg
            (message "%s" msg)
          (message nil))))
    msg))


;;; ======================================================================
;;; User Functions:

;;;###autoload
(defun paren-activate ()
  "Activates mic-paren parenthesis highlighting.
paren-activate deactivates the paren.el and stig-paren.el packages if they are
active !
The following options are available via the customize-feature:
  `paren-priority'
  `paren-overlay-priority'
  `paren-sexp-mode'
  `paren-highlight-at-point'
  `paren-highlight-offscreen'
  `paren-display-message'
  `paren-message-linefeed-display'
  `paren-message-no-match'
  `paren-message-show-linenumber'
  `paren-message-truncate-lines'
  `paren-ding-unmatched'
  `paren-delay'
  `paren-dont-touch-blink'
  `paren-match-face'
  `paren-mismatch-face'
  `paren-no-match-face'
  `paren-bind-modified-sexp-functions'
The following options are settable via toggling functions \(look at the
documentation of these options for the names of these functions\):
  `paren-match-quoted-paren'
  `paren-match-paired-delimiter'
  `paren-open-paren-context-backward'"
  (interactive)
  ;; Deactivate mic-paren.el (To remove redundant hooks)
  (paren-deactivate)
  ;; Deactivate paren.el if loaded
  (if (boundp 'post-command-idle-hook)
      (remove-hook 'post-command-idle-hook 'show-paren-command-hook))
  (remove-hook 'post-command-hook 'show-paren-command-hook)
  (and (boundp 'show-paren-overlay)
       show-paren-overlay
       (mic-delete-overlay show-paren-overlay))
  (and (boundp 'show-paren-overlay-1)
       show-paren-overlay-1
       (mic-delete-overlay show-paren-overlay-1))
  ;; Deactivate stig-paren.el if loaded
  (if (boundp 'post-command-idle-hook)
      (remove-hook 'post-command-idle-hook 'stig-paren-command-hook))
  (remove-hook 'post-command-hook 'stig-paren-command-hook)
  (remove-hook 'post-command-hook 'stig-paren-safe-command-hook)
  (remove-hook 'pre-command-hook 'stig-paren-delete-overlay)
  ;; Deactivate Emacs standard parenthesis blinking
  (or paren-dont-touch-blink
      (setq blink-matching-paren nil))

  (cond(
        ;; If timers are available use them
        ;; (Emacs 19.31 and above)
        (featurep 'timer)
        (if (numberp paren-delay)
            (setq mic-paren-idle-timer
                  (mic-run-with-idle-timer paren-delay t
                                           'mic-paren-command-idle-hook))
          (add-hook 'post-command-hook 'mic-paren-command-hook)))
       ;; If the idle hook exists assume it is functioning and use it
       ;; (Emacs 19.30)
       ((and (boundp 'post-command-idle-hook)
             (boundp 'post-command-idle-delay))
        (if paren-delay
            (add-hook 'post-command-idle-hook 'mic-paren-command-idle-hook)
          (add-hook 'post-command-hook 'mic-paren-command-hook)))
       ;; Check if we (at least) have a post-comand-hook, and use it
       ;; (Emacs 19.29 and below)
       ((boundp 'post-command-hook)
        (add-hook 'post-command-hook 'mic-paren-command-hook))
       ;; Not possible to install mic-paren hooks
       (t (error "Cannot activate mic-paren in this Emacs version")))
  ;; we want matching quoted parens is the minibuffer so easier inserting
  ;; paren-expressions i a rexexp.
  (add-hook 'minibuffer-setup-hook
            'mic-paren-minibuffer-setup-hook)
  (add-hook 'minibuffer-exit-hook
            'mic-paren-minibuffer-exit-hook))



;;;###autoload
(defun paren-deactivate ()
  "Deactivates mic-paren parenthesis highlighting"
  (interactive)
  ;; Deactivate (don't bother to check where/if mic-paren is acivte, just
  ;; delete all possible hooks and timers)
  (if (boundp 'post-command-idle-hook)
      (remove-hook 'post-command-idle-hook 'mic-paren-command-idle-hook))
  (if mic-paren-idle-timer
      (mic-cancel-timer mic-paren-idle-timer))
  (remove-hook 'post-command-hook 'mic-paren-command-hook)
  (remove-hook 'minibuffer-setup-hook
               'mic-paren-minibuffer-setup-hook)
  (remove-hook 'minibuffer-exit-hook
               'mic-paren-minibuffer-exit-hook)
  ;; Remove any old highlighs
  (mic-delete-overlay mic-paren-backw-overlay)
  (mic-delete-overlay mic-paren-point-overlay)
  (mic-delete-overlay mic-paren-forw-overlay)

  ;; Reactivate Emacs standard parenthesis blinking
  (or paren-dont-touch-blink
      (setq blink-matching-paren t))
  )

;;;###autoload
(defun paren-toggle-matching-paired-delimiter (arg &optional no-message)
"Toggle matching paired delimiter, force on with positive arg. Use this in
mode-hooks to activate or deactivate paired delimiter matching. If optional
second argument NO-MESSAGE is not nil then no message is displayed about the
current activation state of the paired-delimiter-matching feature."
  (interactive "P")
  (setq paren-match-paired-delimiter (if (numberp arg)
                                         (> arg 0)
                                       (not paren-match-paired-delimiter)))
  (if (not no-message)
      (message "Matching paired delimiter is %s"
               (if paren-match-paired-delimiter
                   "ON."
                 "OFF."))))


;;;###autoload
(defun paren-toggle-matching-quoted-paren (arg &optional no-message)
  "Toggle matching quoted parens, force on with positive arg. Use this in
mode-hooks to activate or deactivate quoted paren matching. If optional second
argument NO-MESSAGE is not nil then no message is displayed about the current
activation state of the quoted-paren-matching feature."
  (interactive "P")
  (setq paren-match-quoted-paren (if (numberp arg)
                                         (> arg 0)
                                       (not paren-match-quoted-paren)))
  ;; if matching quoted parens is active now bind the original binding of
  ;; forward-sexp and backward-sexp to the modified
  ;; versions paren-forward-sexp (resp. paren-backward-sexp)
  ;; if not bind it back to the original forward-sexp (resp. backward-sexp).
  (let ((key-forward-sexp (car
                           mic-paren-original-keybinding-of-sexp-functions))
        (key-backward-sexp (car
                            (cdr
                             mic-paren-original-keybinding-of-sexp-functions))))
    (if (and paren-bind-modified-sexp-functions
             key-backward-sexp
             key-forward-sexp)
        (if paren-match-quoted-paren
            (progn
              (local-set-key key-forward-sexp
                             (quote paren-forward-sexp))
              (local-set-key key-backward-sexp
                             (quote paren-backward-sexp)))
          (local-set-key key-forward-sexp (quote forward-sexp))
          (local-set-key key-backward-sexp (quote backward-sexp)))))
  (if (not no-message)
      (message "Matching quoted parens is %s"
               (if paren-match-quoted-paren
                   "ON."
                 "OFF."))))

;;;###autoload
(defun paren-toggle-open-paren-context (arg)
  "Toggle the determining of the context to display of the matching
open-paren, force backward context with positive arg. Use this in mode-hooks.
For a description of the meaning look at `paren-open-paren-context-backward'."
  (interactive "P")
  (setq paren-open-paren-context-backward
        (if (numberp arg)
            (> arg 0)
          (not paren-open-paren-context-backward))))

;;;###autoload
(defun paren-forward-sexp (&optional arg)
  "Acts like forward-sexp but can also handle quoted parens. Look at
`paren-match-quoted-paren' for a detailed comment."
  (interactive "p")
  (or arg (setq arg 1))
  (let* ((uncharquote-diff (if (< arg 0) 2 1))
         (match-check-diff (if (< arg 0) 1 2))
         (charquote (mic-paren-uncharquote (- (point) uncharquote-diff)))
         match-pos mismatch)
    ;; we must encapsulate this in condition-case so we regain control
    ;; after error and we can undo our unquoting if any done before!
    (condition-case ()
        (setq match-pos (scan-sexps (point) arg))
      (error nil))
    (mic-paren-recharquote charquote)
    (if (not match-pos)
        (buffer-end arg)
      (setq mismatch (if charquote
                         (not (mic-paren-is-following-char-quoted
                               (- match-pos match-check-diff)))
                       (mic-paren-is-following-char-quoted
                        (- match-pos match-check-diff))))
      (if (not mismatch)
          (goto-char match-pos)
        (forward-sexp arg)
        ))))

;;;###autoload
(defun paren-backward-sexp (&optional arg)
  "Acts like backward-sexp but can also matching quoted parens. Look at
`paren-match-quoted-paren' for a detailed comment"
  (interactive "p")
  (or arg (setq arg 1))
  (paren-forward-sexp (- arg)))


;;; ======================================================================
;;; Pure Internal variables:

(defvar mic-paren-backw-overlay (mic-make-overlay (point-min) (point-min))
  "Overlay for the open-paren which matches the close-paren before
point. When in sexp-mode this is the overlay for the expression before point.")

(defvar mic-paren-point-overlay (mic-make-overlay (point-min) (point-min))
  "Overlay for the close-paren before point.
\(Not used when is sexp-mode.)")

(defvar mic-paren-forw-overlay (mic-make-overlay (point-min) (point-min))
  "Overlay for the close-paren which matches the open-paren after
point. When in sexp-mode this is the overlay for the expression after point.")

(defvar mic-paren-idle-timer nil
  "Idle-timer. Used only in Emacs 19.31 and above \(and if paren-delay is
nil)")

(defvar mic-paren-previous-location [nil nil nil]
  "Records where point was the last time mic-paren performed some action.
Format is [POINT BUFFER WINDOW]")


;;; ======================================================================
;;; Internal function:



(defun mic-paren-command-hook ()
  (or executing-kbd-macro
      (input-pending-p)                 ;[This might cause trouble since the
                                        ; function is unreliable]
      (condition-case paren-error
          (mic-paren-highlight)
        (error
         (if (not (window-minibuffer-p (selected-window)))
             (message "mic-paren catched error (please report): %s"
                      paren-error))))))

(defun mic-paren-command-idle-hook ()
  (condition-case paren-error
      (mic-paren-highlight)
    (error
     (if (not (window-minibuffer-p (selected-window)))
         (message "mic-paren catched error (please report): %s"
                  paren-error)))))

;; helper macro to set a FACE and the value of `paren-overlay-priority'
;; to an OVERLAY.
(defmacro mic-paren-overlay-set (overlay face)
  (` (progn
       (mic-overlay-put (, overlay)
                        'face (, face))
       (mic-overlay-put (, overlay)
                        'priority paren-overlay-priority))))

(defun mic-paren-minibuffer-setup-hook ()
  (paren-toggle-matching-quoted-paren 1 t))

(defun mic-paren-minibuffer-exit-hook ()
  (paren-toggle-matching-quoted-paren -1 t))


(defun mic-paren-highlight ()
  "The main-function of mic-paren. Does all highlighting, dinging, messages,
cleaning-up."
  ;; Remove any old highlighting
  (mic-delete-overlay mic-paren-forw-overlay)
  (mic-delete-overlay mic-paren-point-overlay)
  (mic-delete-overlay mic-paren-backw-overlay)

  ;; Handle backward highlighting (when after a close-paren or a paired
  ;; delimiter):
  ;; If (positioned after a close-paren, and
  ;;    not before an open-paren when priority=open, and
  ;;    (paren-match-quoted-paren is t or the close-paren is not escaped))
  ;;    or
  ;;    (positioned after a paired delimiter, and
  ;;    not before a paired-delimiter when priority=open, and
  ;;    the paired-delimiter is not escaped))
  ;; then
  ;;      perform highlighting
  (if (or (and (eq (char-syntax (preceding-char)) ?\))
               (not (and (eq (char-syntax (following-char)) ?\()
                         (eq paren-priority 'open)))
               (or paren-match-quoted-paren
                   (not (mic-paren-is-following-char-quoted (- (point)
                                                               2)))))
          (and paren-match-paired-delimiter
               (eq (char-syntax (preceding-char)) ?\$)
               (not (and (eq (char-syntax (following-char)) ?\$)
                         (eq paren-priority 'open)))
               (not (mic-paren-is-following-char-quoted (- (point) 2)))))
      (let (open matched-paren charquote)
        ;; if we want to match quoted parens we must change the syntax of
        ;; the escape or quote-char temporarily. This will be undone later.
        (setq charquote (mic-paren-uncharquote (- (point) 2)))
        ;; Find the position for the open-paren
        (save-excursion
          (save-restriction
            (if blink-matching-paren-distance
                (narrow-to-region
                 (max (point-min)
                      (- (point) blink-matching-paren-distance))
                 (point-max)))
            (condition-case ()
                (setq open (scan-sexps (point) -1))
              (error nil))))

        ;; we must call matching-paren because scan-sexps don't care about
        ;; the kind of paren (e.g. matches '( and '}). matching-paren only
        ;; returns the character displaying the matching paren in buffer's
        ;; syntax-table (regardless of the buffer's current contents!).
        ;; Below we compare the results of scan-sexps and matching-paren
        ;; and if different we display a mismatch.
        (setq matched-paren (matching-paren (preceding-char)))
        ;; matching-paren can only handle characters with syntax ) or (
        (if (eq (char-syntax (preceding-char)) ?\$)
            (setq matched-paren (preceding-char)))

        ;; if we have changed the syntax of the escape or quote-char we
        ;; must undo this and we can do this first now.
        (mic-paren-recharquote charquote)

        ;; If match found
        ;;    highlight expression and/or print messages
        ;; else
        ;;    highlight unmatched paren
        ;;    print no-match message
        (if open
            (let ((mismatch (or (not matched-paren)
                                (/= matched-paren (char-after open))
                                (if charquote
                                    (not (mic-paren-is-following-char-quoted
                                          (1- open)))
                                  (mic-paren-is-following-char-quoted
                                   (1- open)))))
                  ;; check if match-pos is visible
                  (visible (and (pos-visible-in-window-p open)
                                (mic-paren-horizontal-pos-visible-p open))))
              ;; If highlight is appropriate
              ;;    highlight
              ;; else
              ;;    remove any old highlight
              (if (or visible paren-highlight-offscreen paren-sexp-mode)
                  ;; If sexp-mode
                  ;;    highlight sexp
                  ;; else
                  ;;    highlight the two parens
                  (if (mic-paren-sexp-mode-p mismatch)
                      (progn
                        (setq mic-paren-backw-overlay
                              (mic-make-overlay open (point)))
                        (if mismatch
                            (mic-paren-overlay-set mic-paren-backw-overlay
                                                   paren-mismatch-face)
                          (mic-paren-overlay-set mic-paren-backw-overlay
                                                 paren-match-face)))
                    (setq mic-paren-backw-overlay
                          (mic-make-overlay
                           open
                           (+ open
                              (mic-char-bytes (char-after open)))))
                    (and paren-highlight-at-point
                         (setq mic-paren-point-overlay
                               (mic-make-overlay
                                (- (point)
                                   (mic-char-bytes (preceding-char)))
                                (point))))
                    (if mismatch
                        (progn
                          (mic-paren-overlay-set mic-paren-backw-overlay
                                                 paren-mismatch-face)
                          (and paren-highlight-at-point
                               (mic-paren-overlay-set mic-paren-point-overlay
                                                      paren-mismatch-face)))
                      (mic-paren-overlay-set mic-paren-backw-overlay
                                             paren-match-face)
                      (and paren-highlight-at-point
                           (mic-paren-overlay-set mic-paren-point-overlay
                                                  paren-match-face)))))
              ;; Print messages if match is offscreen
              (and (not (eq paren-display-message 'never))
                   (or (not visible) (eq paren-display-message 'always))
                   (not (window-minibuffer-p (selected-window)))
                   (not isearch-mode)
                   (mic-paren-is-new-location)
                   (let ((message-truncate-lines paren-message-truncate-lines))
                     (mic-paren-nolog-message "%s %s"
                                              (if mismatch "MISMATCH:" "Matches")
                                              (mic-paren-get-matching-open-text open))))
              ;; Ding if mismatch
              (and mismatch
                   paren-ding-unmatched
                   (mic-paren-is-new-location)
                   (ding)))
          (setq mic-paren-backw-overlay
                (mic-make-overlay (point)
                                  (- (point)
                                     (mic-char-bytes (preceding-char)))))
          (mic-paren-overlay-set mic-paren-backw-overlay
                                 paren-no-match-face)
          (and paren-message-no-match
               (not (window-minibuffer-p (selected-window)))
               (not isearch-mode)
               (mic-paren-is-new-location)
               (mic-paren-nolog-message "No opening parenthesis found"))
          (and paren-message-no-match
               paren-ding-unmatched
               (mic-paren-is-new-location)
               (ding)))))

  ;; Handle forward highlighting (when before an open-paren or a paired
  ;; delimiter):
  ;; If (positioned before an open-paren, and
  ;;    not after a close-paren when priority=close, and
  ;;    (paren-match-quoted-paren is t or the open-paren is not escaped))
  ;;    or
  ;;    (positioned before a paired delimiter, and
  ;;    not after a paired-delimiter when priority=close, and
  ;;    the paired-delimiter is not escaped))
  ;; then
  ;;      perform highlighting
  (if (or (and (eq (char-syntax (following-char)) ?\()
               (not (and (eq (char-syntax (preceding-char)) ?\))
                         (eq paren-priority 'close)))
               (or paren-match-quoted-paren
                   (not (mic-paren-is-following-char-quoted (1-
                                                             (point))))))
          (and paren-match-paired-delimiter
               (eq (char-syntax (following-char)) ?\$)
               (not (and (eq (char-syntax (preceding-char)) ?\$)
                         (eq paren-priority 'close)))
               (not (mic-paren-is-following-char-quoted (1- (point))))))
      (let (close matched-paren charquote)
        ;; if we want to match quoted parens we must change the syntax of
        ;; the escape or quote-char temporarily. This will be undone later.
        (setq charquote (mic-paren-uncharquote (1- (point))))
        ;; Find the position for the close-paren
        (save-excursion
          (save-restriction
            (if blink-matching-paren-distance
                (narrow-to-region
                 (point-min)
                 (min (point-max)
                      (+ (point) blink-matching-paren-distance))))
            (condition-case ()
                (setq close (scan-sexps (point) 1))
              (error nil))))

        ;; for an explanation look above.
        (setq matched-paren (matching-paren (following-char)))
        (if (eq (char-syntax (following-char)) ?\$)
            (setq matched-paren (following-char)))

        ;; if we have changed the syntax of the escape or quote-char we
        ;; must undo this and we can do this first now.
        (mic-paren-recharquote charquote)

        ;; If match found
        ;;    highlight expression and/or print messages
        ;; else
        ;;    highlight unmatched paren
        ;;    print no-match message
        (if close
            (let ((mismatch (or (not matched-paren)
                                (/= matched-paren (mic-char-before close))
                                (if charquote
                                    (not (mic-paren-is-following-char-quoted
                                          (- close 2)))
                                  (mic-paren-is-following-char-quoted
                                   (- close 2)))))
                  ;; check if match-pos is visible
                  (visible (and (pos-visible-in-window-p close)
                                (mic-paren-horizontal-pos-visible-p close))))
              ;; If highlight is appropriate
              ;;    highlight
              ;; else
              ;;    remove any old highlight
              (if (or visible paren-highlight-offscreen paren-sexp-mode)
                  ;; If sexp-mode
                  ;;    highlight sexp
                  ;; else
                  ;;    highlight the two parens
                  (if (mic-paren-sexp-mode-p mismatch)
                      (progn
                        (setq mic-paren-forw-overlay
                              (mic-make-overlay (point) close))
                        (if mismatch
                            (mic-paren-overlay-set mic-paren-forw-overlay
                                                   paren-mismatch-face)
                          (mic-paren-overlay-set mic-paren-forw-overlay
                                                 paren-match-face)))
                    (setq mic-paren-forw-overlay
                          (mic-make-overlay
                           (- close
                              (mic-char-bytes (mic-char-before close)))
                           close))
                    (if mismatch
                        (mic-paren-overlay-set mic-paren-forw-overlay
                                               paren-mismatch-face)
                      (mic-paren-overlay-set mic-paren-forw-overlay
                                             paren-match-face))))

              ;; Print messages if match is offscreen
              (and (not (eq paren-display-message 'never))
                   (or (not visible) (eq paren-display-message 'always))
                   (not (window-minibuffer-p (selected-window)))
                   (not isearch-mode)
                   (mic-paren-is-new-location)
                   (let ((message-truncate-lines paren-message-truncate-lines))
                     (mic-paren-nolog-message "%s %s"
                                              (if mismatch "MISMATCH:" "Matches")
                                              (mic-paren-get-matching-close-text close))))
              ;; Ding if mismatch
              (and mismatch
                   (mic-paren-is-new-location)
                   paren-ding-unmatched
                   (ding)))
          (setq mic-paren-forw-overlay
                (mic-make-overlay (point)
                                  (+ (point)
                                     (mic-char-bytes (following-char)))))
          (mic-paren-overlay-set mic-paren-forw-overlay
                                 paren-no-match-face)
          (and paren-message-no-match
               (not (window-minibuffer-p (selected-window)))
               (not isearch-mode)
               (mic-paren-is-new-location)
               (mic-paren-nolog-message "No closing parenthesis found"))
          (and paren-message-no-match
               paren-ding-unmatched
               (mic-paren-is-new-location)
               (ding)))))

  ;; Store the points position in mic-paren-previous-location
  ;; Later used by mic-paren-is-new-location
  (or (window-minibuffer-p (selected-window))
      (progn
        (aset mic-paren-previous-location 0 (point))
        (aset mic-paren-previous-location 1 (current-buffer))
        (aset mic-paren-previous-location 2 (selected-window))))
  )

;;; --------------------------------------------------

(defun mic-paren-sexp-mode-p (mismatch)
  "Check if we must highlight the whole sexp and return t if we must"
  (cond ((eq paren-sexp-mode nil) nil)
        ((eq paren-sexp-mode t) t)
        ((eq paren-sexp-mode 'match) (not mismatch))
        ((eq paren-sexp-mode 'mismatch) mismatch)))
  
;;; --------------------------------------------------

(defun mic-paren-horizontal-pos-visible-p (match-pos)
  "Returns non nil if the MATCH-POS is horizontal visible otherwise nil \(in
case of horizontal scrolling)."
  (let ((window (selected-window)))
    (save-excursion
      (goto-char match-pos)
      (and (>= (- (current-column) (window-hscroll window)) 0)
           (< (- (current-column) (window-hscroll window))
              (window-width window))))))

;; (defun mic-paren-horizontal-pos-visible-p (match-pos)
;;   "Returns non nil if the MATCH-POS is horizontal visible otherwise nil \(in
;; case of horizontal scrolling)."
;;   (let ((match-column
;;          (save-excursion
;;            (goto-char match-pos)
;;            (current-column))))
;;     (if (> (window-hscroll) 0)
;;         (and (>= match-column (window-hscroll))
;;              (< match-column (+ (window-hscroll) (window-width))))
;;       (< match-column (window-width)))))

(defun mic-paren-get-matching-open-text (open)
  "Returns a string with the context around OPEN-paren."
  ;; If there's stuff on this line preceding the paren, then display text from
  ;; beginning of line to paren.
  ;;
  ;; If, however, the paren is at the beginning of a line (means only
  ;; whitespace before the paren), then skip whitespace forward and display
  ;; text from paren to end of the next line containing non-space text. But
  ;; if `paren-open-paren-context-backward' is non nil then skip
  ;; whitespaces backward and display text from beginning of previous line
  ;; to paren.
  (let* ((loc (if (eq paren-message-show-linenumber 'sexp)
                  (point) (point-min)))
         (str (save-excursion
                (goto-char open)
                (if (save-excursion
                      (skip-chars-backward " \t")
                      (not (bolp)))
                    (progn
                      (beginning-of-line)
                      (format "%s... [%s%d-]"
                              (buffer-substring (point) (1+ open))
                              (if (eq paren-message-show-linenumber 'sexp)
                                  "-" "")
                              (count-lines loc open)))
                  (let (paren-context-string)
                    (if (not paren-open-paren-context-backward)
                        (progn
                          (forward-char 1)
                          (skip-chars-forward "\n \t")
                          (end-of-line)
                          (setq paren-context-string (buffer-substring open (point))))
                      (skip-chars-backward "\n \t")
                      (beginning-of-line)
                      (setq paren-context-string (buffer-substring (point) (1+ open))))
                    (format "%s [%s%d]"
                            paren-context-string
                            (if (eq paren-message-show-linenumber 'sexp)
                                "-" "")
                            (count-lines loc open)))))))
    (while (string-match "[\n]" str)
      (setq str (replace-match paren-message-linefeed-display nil t str)))
    str))
    


(defun mic-paren-get-matching-close-text (close)
  "Returns a string with the context around CLOSE-paren."
  ;; The whole line up until the close-paren with "..." appended if there are
  ;; more text after the close-paren
  (let* ((loc (if (eq paren-message-show-linenumber 'sexp)
                  (point) (point-min)))
         (str (save-excursion
                (goto-char close)
                (forward-char -1)
                (skip-chars-backward "\n \t")
                (beginning-of-line)
                (format "%s%s [%s%d]"
                        (buffer-substring (point) close)
                        (progn
                          (goto-char close)
                          (if (looking-at "[ \t]*$")
                              ""
                            "..."))
                        (if (eq paren-message-show-linenumber 'sexp)
                            "+" "")                       
                        (count-lines loc close)))))
    (while (string-match "[\n]" str)
      (setq str (replace-match paren-message-linefeed-display nil t str)))
    str))



(defun mic-paren-is-new-location ()
  "Returns t if the points location is not the same as stored in
`mic-paren-previous-location', nil otherwise.

The variable `mic-paren-previous-location' is set by
`mic-paren-highlight'."
  (not (and (eq (point) (aref mic-paren-previous-location 0))
            (eq (current-buffer) (aref mic-paren-previous-location 1))
            (eq (selected-window) (aref mic-paren-previous-location 2)))))


(defun mic-paren-is-following-char-quoted (pnt)
  "returns true if character at point PNT escapes or quotes the following
char."
  (let ((n 0))
    (while (and (>= pnt (point-min))
                (or (eq (char-syntax (char-after pnt)) ?\\)
                    (eq (char-syntax (char-after pnt)) ?/)))
      (setq n (1+ n))
      (setq pnt (1- pnt)))
    (if (eq 0 (% n 2)) nil t)))

(defun mic-paren-uncharquote (pnt)
  "if the syntax of character <c> at point PNT is escape or quote and if the
character is not escaped or quoted itself then its syntax will be modified
to punctuation and multiple values \(<c> \"<syntax-of-c>\") will be returned;
otherwise nil."
  (let (c cs)
    (if (< pnt (point-min))
        nil
      (setq c (char-after pnt))
      (setq cs (char-syntax c))
      (if (not (and paren-match-quoted-paren
                    (mic-paren-is-following-char-quoted pnt)))
          nil
        (modify-syntax-entry c ".")
        (list c (char-to-string cs))))))

(defun mic-paren-recharquote (charquote)
  "CHARQUOTE is a 2-element-list: car is a character <c> and its cadr
is a syntaxstring <s>. The syntax of character <c> will be set to syntax
<s>. If CHARQUOTE is nil nothing will be done."
  (if charquote
      (modify-syntax-entry (car charquote) (car (cdr charquote)))))


;;; ======================================================================
;;; Initialisation when loading:

;;; Try to load the timer feature if its not already loaded
(or paren-dont-load-timer
    (featurep 'timer)
    (condition-case ()
        (require 'timer)
      (error nil)))

(provide 'mic-paren)
(provide 'paren)

;;; mic-paren.el ends here
