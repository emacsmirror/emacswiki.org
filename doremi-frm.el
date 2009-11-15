;;; doremi-frm.el --- Incrementally adjust face attributes and frame parameters.
;;
;; Filename: doremi-frm.el
;; Description: Incrementally adjust face attributes and frame parameters.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2004-2009, Drew Adams, all rights reserved.
;; Created: Sat Sep 11 10:40:32 2004
;; Version: 22.0
;; Last-Updated: Sat Nov 14 15:38:28 2009 (-0800)
;;           By: dradams
;;     Update #: 2942
;; URL: http://www.emacswiki.org/cgi-bin/wiki/doremi-frm.el
;; Keywords: frames, extensions, convenience, keys, repeat, cycle
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x
;;
;; Features that might be required by this library:
;;
;;   `avoid', `doremi', `eyedropper', `faces', `faces+',
;;   `frame-cmds', `frame-fns', `hexrgb', `misc-fns', `mwheel',
;;   `ring', `ring+', `strings', `thingatpt', `thingatpt+'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Do Re Mi commands to incrementally adjust face attributes and
;;    frame parameters using arrow keys or mouse wheel.
;;
;;  When you invoke the Do Re Mi iterative commands defined here, you
;;  can press and hold an up/down arrow key, or rotate the mouse
;;  wheel, to change face attributes or frame parameters.  For more
;;  information, see file `doremi.el' and the doc-string for function
;;  `doremi' in particular.
;;
;;  NOTE: Functions and variables in this library have the prefix
;;        `doremi-'.  In order to more easily distinguish commands
;;        that iterate in Do Re Mi fashion from other functions in the
;;        library, the iterative commands are suffixed with `+'.
;;
;;  Note on saving changes made with the commands defined here:
;;
;;    Some of the commands defined here change face and frame
;;    parameters.  You can save any changes you have made, by using
;;    Customize.  To visit a Customize buffer of all unsaved changes
;;    you have made, use command `customize-customized'.
;;
;;    Frame parameter changes, such as background color, can be saved
;;    for future use by all frames or all frames of a certain kind.
;;    For that, you must change the frame parameters of the
;;    correponding frame-alist variable.
;;
;;    There is no single variable for saving changes to parameters of
;;    the current frame.  Instead, there are several different
;;    frame-alist variables, which you can use to define different
;;    kinds of frames.  These include: `default-frame-alist',
;;    `initial-frame-alist', and `special-display-frame-alist'.  The
;;    complete list of such frame alist variables is available using
;;    function `frame-alist-var-names', defined in library
;;    `frame-cmds.el'.
;;
;;    Example: Suppose you change the background color of a frame and
;;    want to make that the default background color for new frames in
;;    the future.  You will need to update the value of variable
;;    `default-frame-alist' to use the `background-color' parameter
;;    setting of the changed frame.
;;
;;    You can easily copy one or all parameter values from any given
;;    frame to any frame alist (such as `default-frame-alist'), by
;;    using the commands `set-frame-alist-parameter-from-frame' and
;;    `set-all-frame-alist-parameters-from-frame'.  Those commands are
;;    defined in library `frame-cmds.el'.
;;
;;  Note on available color names:
;;
;;    Color names supported by your Emacs release and platform are
;;    those returned by function `x-color-names'.  This often includes
;;    names that are essentially the same as duplicates, e.g.,
;;    "LightBlue" and "light blue".  By default, Do Re Mi
;;    canonicalizes these names by lowercasing them and removing
;;    whitespace.  Then it removes the duplicates.  This behavior is
;;    governed by option `hexrgb-canonicalize-defined-colors-flag'.
;;    Customize that option to nil if you need the original names.
;;
;;
;;  User options defined here:
;;
;;    `doremi-frame-config-ring-size',
;;    `doremi-move-frame-wrap-within-display-flag',
;;    `doremi-push-frame-config-for-cmds-flag',
;;    `doremi-RGB-increment-factor', `doremi-wrap-color-flag'.
;;
;;
;;  Commands defined here:
;;
;;    `doremi-all-faces-bg+', `doremi-all-faces-fg+',
;;    `doremi-all-frames-bg+', `doremi-all-frames-fg+', `doremi-bg+',
;;    `doremi-bg-blue+', `doremi-bg-brightness+',
;;    `doremi-bg-color-name+', `doremi-bg-cyan+', `doremi-bg-green+',
;;    `doremi-bg-hue+', `doremi-bg-hue-stepping-saturation+',
;;    `doremi-bg-magenta+', `doremi-bg-purity+', `doremi-bg-red+',
;;    `doremi-bg-saturation+', `doremi-bg-value+',
;;    `doremi-bg-yellow+', `doremi-buffer-font-size+',
;;    `doremi-face-bg+', `doremi-face-bg-color-name+',
;;    `doremi-face-bg-hue-stepping-saturation+', `doremi-face-fg+',
;;    `doremi-face-fg-color-name+',
;;    `doremi-face-fg-hue-stepping-saturation+', `doremi-fg+',
;;    `doremi-fg-blue+', `doremi-fg-brightness+',
;;    `doremi-fg-color-name+', `doremi-fg-cyan+', `doremi-fg-green+',
;;    `doremi-fg-hue+', `doremi-fg-hue-stepping-saturation+',
;;    `doremi-fg-magenta+', `doremi-fg-purity+', `doremi-fg-red+',
;;    `doremi-fg-saturation+', `doremi-fg-value+',
;;    `doremi-fg-yellow+', `doremi-font+', `doremi-font-size+',
;;    `doremi-frame-configs+', `doremi-frame-font-size+',
;;    `doremi-frame-height+', `doremi-frame-horizontally+',
;;    `doremi-frame-vertically+', `doremi-frame-width+',
;;    `doremi-increment-background-color',
;;    `doremi-increment-color-component',
;;    `doremi-increment-face-bg-color',
;;    `doremi-increment-face-fg-color',
;;    `doremi-increment-foreground-color',
;;    `doremi-set-background-color', `doremi-set-foreground-color',
;;    `doremi-toggle-wrap-color', `doremi-undo-last-face-change',
;;    `doremi-undo-last-frame-color-change',
;;    `toggle-doremi-wrap-color'.
;;
;;
;;  Non-interactive functions defined here:
;;
;;    `doremi-adjust-increment-for-color-component',
;;    `doremi-all-faces-bg/fg-1', `doremi-all-frames-bg/fg-1',
;;    `doremi-bg-1', `doremi-bg/fg-color-name-1',
;;    `doremi-face-bg/fg-1', `doremi-face-bg/fg-color-name-1',
;;    `doremi-face-color-component',
;;    `doremi-face-hue-stepping-saturation', `doremi-face-set',
;;    `doremi-fg-1', `doremi-frame-color-component',
;;    `doremi-frame-config-wo-parameters',
;;    `doremi-frame-hue-stepping-saturation',
;;    `doremi-frame-new-position',
;;    `doremi-increment-background-color-1', `doremi-increment-color',
;;    `doremi-increment-face-color',
;;    `doremi-increment-face-color-read-args', `doremi-face-default',
;;    `doremi-increment-blue', `doremi-increment-foreground-color-1',
;;    `doremi-increment-frame-color', `doremi-increment-green',
;;    `doremi-increment-red', `doremi-push-current-frame-config',
;;    `doremi-push-frame-config-for-command', `doremi-read-component',
;;    `doremi-read-increment-arg', `doremi-set-frame-color',
;;    `doremi-wrap-or-limit-color-component'.
;;
;;
;;  Internal variables defined here:
;;
;;    `doremi-current-increment', `doremi-frame-config-ring',
;;    `doremi-last-face-value', `doremi-last-frame-color'.
;;
;;
;;  See also these related Do Re Mi libraries:
;;
;;    `doremi-mac.el' - Macro to define Do Re Mi commands and
;;                      automatically add them to a Do Re Mi menu.
;;    `doremi-cmd.el' - Do Re Mi commands not dealing with frames.
;;
;;  See also these files for other frame commands:
;;
;;     `autofit-frame.el' - Automatically fit each frame to its
;;                          selected window.  Uses `fit-frame.el'.
;;
;;     `fit-frame.el'     - 1) Fit a frame to its selected window.
;;                          2) Incrementally resize a frame.
;;
;;     `frame-cmds.el'    - Various frame and window commands.
;;
;;     `thumb-frm.el'     - Shrink frames to a thumbnail size and
;;                          restore them again.
;;
;;     `zoom-frm.el'      - Zoom a frame, so that its font becomes
;;                          larger or smaller.
;;
;;
;;  Put this in your init file (`~/.emacs'): (require 'doremi-frm)
;;
;;  Suggested key bindings:
;;
;;   (defalias 'doremi-prefix (make-sparse-keymap))
;;   (defvar doremi-map (symbol-function 'doremi-prefix)
;;     "Keymap for Do Re Mi commands.")
;;   (define-key global-map "\C-xt" 'doremi-prefix)
;;   (define-key doremi-map "a" 'doremi-all-faces-fg+)    ; "All"
;;   (define-key doremi-map "c" 'doremi-bg+)              ; "Color"
;;   (define-key doremi-map "f" 'doremi-face-fg+)         ; Face"
;;   (define-key doremi-map "h" 'doremi-frame-height+)
;;   (define-key doremi-map "t" 'doremi-font+)            ; "Typeface"
;;   (define-key doremi-map "u" 'doremi-frame-configs+)   ; "Undo"
;;   (define-key doremi-map "x" 'doremi-frame-horizontally+)
;;   (define-key doremi-map "y" 'doremi-frame-vertically+)
;;   (define-key doremi-map "z" 'doremi-font-size+))      ; "Zoom"
;;
;;  Customize the menu.  Uncomment this to try it out.
;;
;;   (defvar menu-bar-doremi-menu (make-sparse-keymap "Do Re Mi"))
;;   (define-key global-map [menu-bar doremi]
;;     (cons "Do Re Mi" menu-bar-doremi-menu))
;;   (define-key menu-bar-doremi-menu [doremi-frame-configs+]
;;     '(menu-item "Frame Configurations" . doremi-frame-configs+
;;       :help "Cycle among frame configurations recorded: `up'/`down'"))
;;   (define-key menu-bar-doremi-menu [doremi-font+]
;;     '(menu-item "Font" . doremi-font+
;;       :help "Successively cycle among fonts, choosing by name: `up'/`down'"))
;;   (when (fboundp 'text-scale-increase)    ; Emacs 23+.
;;     (define-key menu-bar-doremi-menu [doremi-buffer-font-size+]
;;       '(menu-item "Buffer Text Size (Zoom)" doremi-buffer-font-size+
;;         :help "Change text size for buffer incrementally: `up'/`down'")))
;;   (define-key menu-bar-doremi-menu [doremi-frame-font-size+]
;;     '(menu-item "Frame Font Size (Zoom)" doremi-frame-font-size+
;;       :help "Change font size for frame incrementally: `up'/`down'"))
;;   (define-key menu-bar-doremi-menu [doremi-all-frames-fg+]
;;     '(menu-item "All Frame Foregrounds..." doremi-all-frames-fg+
;;       :help "Change foreground of all frames incrementally: `up'/`down'"))
;;   (define-key menu-bar-doremi-menu [doremi-all-frames-bg+]
;;     '(menu-item "All Frame Backgrounds..." doremi-all-frames-bg+
;;       :help "Change background of all frames incrementally: `up'/`down'"))
;;   (define-key menu-bar-doremi-menu [doremi-undo-last-frame-color-change]
;;     '(menu-item "Undo Frame Color Change" doremi-undo-last-frame-color-change
;;       :enable doremi-last-frame-color
;;       :help "Undo the last frame color change by `doremi-fg+' or `doremi-bg+'"))
;;   (define-key menu-bar-doremi-menu [doremi-fg-color-name+]
;;     '(menu-item "Frame Foreground Name..." doremi-fg-color-name+
;;       :help "Change frame foreground color incrementally: `up'/`down'"))
;;   (define-key menu-bar-doremi-menu [doremi-fg+]
;;     '(menu-item "Frame Foreground..." doremi-fg+
;;       :help "Change frame foreground color incrementally: `up'/`down'"))
;;   (define-key menu-bar-doremi-menu [doremi-bg-color-name+]
;;     '(menu-item "Frame Background Name..." doremi-bg-color-name+
;;       :help "Change frame background color incrementally: `up'/`down'"))
;;   (define-key menu-bar-doremi-menu [doremi-bg+]
;;     '(menu-item "Frame Background..." doremi-bg+
;;       :help "Change frame background color incrementally: `up'/`down'"))
;;   (define-key menu-bar-doremi-menu [doremi-all-faces-fg+]
;;     '(menu-item "All Faces - Foreground..." doremi-all-faces-fg+
;;       :help "Change foreground color of all faces incrementally: `up'/`down'"))
;;   (define-key menu-bar-doremi-menu [doremi-all-faces-bg+]
;;     '(menu-item "All Faces - Background..." doremi-all-faces-bg+
;;       :help "Change background color of all faces incrementally: `up'/`down'"))
;;   (define-key menu-bar-doremi-menu [doremi-undo-last-face-change]
;;     '(menu-item "Undo Face Color Change" doremi-undo-last-face-change
;;       :enable (facep 'doremi-last-face) ; Actually, it's always non-nil.
;;       :help "Undo the last face color change by Do Re Mi"))
;;   (define-key menu-bar-doremi-menu [doremi-face-fg-color-name+]
;;     '(menu-item "Face Foreground Name..." doremi-face-fg-color-name+
;;       :help "Change foreground color name of a face incrementally: `up'/`down'"))
;;   (define-key menu-bar-doremi-menu [doremi-face-fg+]
;;     '(menu-item "Face Foreground..." doremi-face-fg+
;;       :help "Change foreground color of a face incrementally: `up'/`down'"))
;;   (define-key menu-bar-doremi-menu [doremi-face-bg-color-name+]
;;     '(menu-item "Face Background Name..." doremi-face-bg-color-name+
;;       :help "Change background color name of a face incrementally: `up'/`down'"))
;;   (define-key menu-bar-doremi-menu [doremi-face-bg+]
;;     '(menu-item "Face Background..." doremi-face-bg+
;;       :help "Change background color of a face incrementally: `up'/`down'"))
;;   (define-key menu-bar-doremi-menu [doremi-frame-vertically+]
;;     '(menu-item "Move Frame" doremi-frame-vertically+
;;       :help "Move frame incrementally: `up'/`down'/`left'/`right'"))
;;   (define-key menu-bar-doremi-menu [doremi-frame-height+]
;;     '(menu-item "Frame Size" doremi-frame-height+
;;       :help "Resize frame incrementally: `up'/`down'/`left'/`right'"))
;;
;;
;;  TO DO?
;;
;;    1. Factor out more common stuff between foreground and background.
;;    2. Make it easy to turn on and off doremi-push-frame-config stuff.
;;    3. Integrate more with Customize.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; 2009/11/14 dadams
;;     Added: doremi(-face)-(bg|fg)-hue-stepping-saturation+,
;;            doremi-(face|frame)-hue-stepping-saturation.  (No keys bound.)  Thx to Ahei.
;; 2009/11/10 dadams
;;     Added: doremi(-face)-bg/fg-color-name-1.  Thx to Ahei.
;;     doremi(-face)-(bg|fg)-color-name+: Use doremi(-face)-bg/fg-color-name-1.  Added args.
;; 2009/11/07 dadams
;;     Added: doremi-adjust-increment-for-color-component, doremi-face-bg/fg-1,
;;            doremi-face-color-component, doremi-increment-face-color
;;            doremi-face-default, doremi-increment-(blue|green|red),
;;            doremi-all-(frames|faces)-bg/fg-1, 
;;     doremi-all-frames-(bg|fg)+: Use *-all-frames-bg/fg-1, *-read-component.
;;                                 Set increment to 1 if nil.  Use only visible frames.
;;     doremi-face-(bg|fg)+:
;;       Added interactive-p arg.  Use *-face-bg/fg-1.  Set *-last-face-value only when
;;       interactive.  Wrap in condition-case for C-g.  Handle R, H, and list of
;;       increments, via *-face-bg/fg-1 and *-face-color-component.
;;     doremi-all-faces-(bg|fg)+: Use *-all-faces-bg/fg-1 (handles R, H, increment list).
;;     doremi-all-(frames|faces)-bg/fg-1 (new), doremi-(frame|face)-color-component:
;;       Use *-adjust-increment-for-color-component.
;;     doremi-increment-color: Use doremi-increment-(blue|green|red).
;; 2009/11/05 dadams
;;     Renamed all Do Re Mi iterative commands by appending +.
;; 2009/11/04 dadams
;;     Added: doremi-(bg|fg)-1, doremi-current-increment, doremi-frame-color-component,
;;            doremi-increment-(back|fore)ground-color-1,
;;            doremi-increment-frame-color, doremi-read-component, doremi-set-frame-color.
;;     doremi-read-increment-arg: Redefined - allow list of numbers, added LENGTH arg.
;;     doremi-increment-color: Allow a list value for increment.
;;     Use doremi-read-increment-arg everywhere, with args 3 and 1.
;;     doremi-(bg|fg), doremi-increment-(back|fore)ground-color:
;;       Added interactive-p arg.  Use doremi-read-component.
;;       Set *-last-frame-color only when interactive.
;;       Use *-1 helper fn.  Wrap in condition-case for C-g.
;;     doremi-(bg|fg): Handle R, H, and a list of increments, via *-(bg|fg)-1 and
;;                     *-frame-color-component.  
;;     doremi-set-(back|fore)ground-color: Use doremi-set-frame-color.
;; 2009/11/03 dadams
;;     Renamed: doremi-number-arg to doremi-read-increment-arg.
;; 2009/11/02 dadams
;;     Added: doremi-face-(bg|fg)-color-name, doremi-fg-color-name.  Thx to Ahei.
;;     doremi-face-(bg|fg): Added unwind-protect, to delete sample buffer & window.
;;                          Inhibit frame fitting.
;;     doremi-(bg|fg)-color-name, doremi-increment-color-component:
;;       Use the function hexrgb-defined-colors(-alist), not the constant.
;;     doremi-face-set: Don't define it for Emacs 20.
;; 2009/08/05 dadams
;;     doremi-RGB-increment-factor: Changed default value to 1.
;;     doremi-increment-*ground-color: Back up doremi-last-frame-color if interactive.
;; 2009/08/04 dadams
;;     Added: doremi-RGB-increment-factor.
;;     doremi(-all-(frames|faces)|-face)-(bg|fg):
;;       Use doremi-RGB-increment-factor, not 256.
;;     doremi-all-faces-(bg|fg): Scale by doremi-RGB-increment-factor (forgot to scale).
;;     Thx to Stefan Guath.
;; 2009/06/26 dadams
;;     doremi-frame-width, doremi-frame-horizontally:
;;       Use new key-list options, doremi-...-keys (not -key).
;; 2008/01/03 dadams
;;     doremi(-face)-(bg|fg), doremi-all-frames-(bg|fg):
;;       Scale increment arg by 256 for RGB.
;;     doremi-increment-color(-component): Do not scale INCREMENT arg by 256 for RGB.
;;     doremi-increment-color: Limit INCREMENT from -100 to 100 for HSV only.
;; 2007/12/31 dadams
;;     doremi-last-face-value: Use copy-face instead of internal-find-face.
;; 2007/12/30 dadams
;;     doremi-all-(faces|frames)-(bg|fg): Bound doremi-wrap-color-flag to nil.
;; 2007/11/01 dadams
;;     doremi-frame-(horizontally|vertically): Don't use doremi-number-arg.
;;     Lowercased all :groups.
;; 2007/10/26 dadams
;;     doremi-last-frame-color: Initial value is nil now.
;;     doremi-undo-last-frame-color-change: Added error message if no last frame color.
;; 2007/10/21 dadams
;;     Renamed: doremi-wrap-or-limit to doremi-wrap-or-limit-color-component.
;;              Redefined it using doremi-limit and doremi-wrap.
;; 2007/10/08 dadams
;;     Use lowercase for defgroup group.
;; 2007/09/30 dadams
;;     doremi-face-set: (setq attrs (cdr attrs)) -> (setq attrs (cddr attrs)).
;; 2006/06/23 dadams
;;     picked-(back|fore)ground -> eyedrop-picked-(back|fore)ground
;;     Require eyedropper.el or palette.el, depending on the Emacs version.
;;     doremi(-face)-(bg|fg):
;;       Added pickup-p arg. Use picked color if pickup-p arg or C-u (not <0).
;;     doremi-(bg|fg)-*: Call doremi-(bg|fg) with pickup-p arg.
;;     Bug fix, doremi(-face)-(bg|fg):
;;       Only pick up picked bg or fg on first call (interactive).
;;       Tolerate no load of pick-up code.
;; 2006/06/06 dadams
;;     Use hexrgb-defined-colors(-alist) instead of x-defined-colors.
;; 2006/05/30 dadams
;;     doremi-increment-color-component: Use hexrgb-color-name-to-hex.
;;     Removed: doremi-color-name-to-RGB.
;; 2006/01/07 dadams
;;      Added :link.
;; 2005/12/26: dadams
;;     Updated group and parent groups.
;; 2005/12/13 dadams
;;     doremi-increment-face-(b|f)g-color:
;;       Bug fix: Only update doremi-last-face(-value) when interactive.
;; 2005/08/09 dadams
;;     Added: doremi-wrap-color-flag, doremi-wrap-or-limit, doremi-toggle-wrap-color,
;;            toggle-doremi-wrap-color.
;;     doremi-increment-color: Take doremi-wrap-color-flag into account.
;;                             Use doremi-wrap-or-limit.
;; 2005/08/02 dadams
;;     Added: doremi-all-faces-(b|f)g, doremi-all-frames-(b|f)g,
;;            doremi-set-(back|fore)ground-color.
;;     doremi-(b|f)g, doremi-increment-(back|fore)ground-color,
;;       doremi-undo-last-frame-color-change, doremi-increment-color: Added frame arg.
;;     doremi-increment-color: Updated doc string.  Lower bound of increment is -100.
;;     doremi-increment-face-(b|f)g-color: Use nil frame arg to doremi-increment-color.
;;                                         Save face arg as last face.
;;     doremi-increment-(back|fore)ground-color: Use doremi-number-arg.
;;     doremi-(b|f)g, doremi-increment-(back|fore)ground-color:
;;       Use doremi-set-(back|fore)ground-color instead of set-(back|fore)ground-color.
;;     doremi-color-name-to-RGB: Use facemenu-read-color, instead of completing-read.
;;     doremi-last-face-value, doremi-last-frame-color: Better default values.
;;     doremi-undo-last-face-change: Error if no last face.
;;     Only require strings.el if read-number is not fboundp.
;; 2005/07/31 dadams
;;     Added: doremi-color-name-to-RGB, doremi-number-arg.
;;     doremi-frame-(horizontally|vertically), doremi-(bg|fg),
;;       doremi-increment-face-color-read-args, doremi-increment-color-component:
;;         Use doremi-number-arg.
;;     doremi-increment-color-component: Made into a command.
;;     doremi-face-(fg|bg): Use doremi-increment-face-color-read-args.
;; 2005/07/29 dadams
;;     Added: doremi-increment-color-component.
;; 2005/07/25 dadams
;;     Added: :prefix to defgroup.
;; 2005/07/17 dadams
;;     doremi-increment-color: Limit increment to 100 max.
;;     Mention in doc strings that increment max is 100.
;; 2005/07/02 dadams
;;     Added: doremi-fg*, doremi-increment-foreground-color,
;;            doremi-undo-last-frame-color-change, doremi-last-frame-color.
;; 2005/07/01 dadams
;;     doremi-face-[fb]g: Added treatment of negative prefix arg (use picked color).
;;     doremi-face-[fb]g-1: Use increment arg, already normalized by caller.
;;     doremi-undo-last-face-change: Use doremi-last-face directly.
;; 2005/06/30 dadams
;;     doremi-face-[fb]g:
;;       Also display sample of face before changes.
;;       Save face before changes, for doremi-undo-last-face-change.
;;       Error if face arg doesn't name a face.
;;     Added: doremi-last-face-value, doremi-undo-last-face-change.
;;     Removed: doremi-face-(fore|back)ground (to faces+.el as face-(fore|back)ground-20+.
;;     Hard require faces+.el.
;; 2005/06/28 dadams
;;     doremi-face-[fb]g: Pop up a sample.
;;     Added: doremi-face-[fb]g-1.
;; 2005/06/26 dadams
;;     doremi-increment-color: Fixed bug when face was a symbol, not a variable - use
;;       apply instead of eval funcall.
;; 2005/06/24 dadams
;;     doremi-face-[bf]g:
;;       1) No longer convert face to internal-get-face form.
;;       2) Use face, instead of face-name.
;;       3) No longer use doremi-face-set.  Use set-face-attribute or modify-face.
;;     doremi-increment-face-[bf]g-color: ensure face via facep, not internal-get-face.
;;     doremi-face-set: No longer use face-spec-set.  This should be OK for Emacs 22,
;;       but it is not used, for now.
;; 2005/05/29 dadams
;;     Renamed: doremi-frame-move-wrap-within-display ->
;;                doremi-move-frame-wrap-within-display-flag.
;; 2005/01/25 dadams
;;     doremi-face-bg, doremi-face-fg (bug fix):
;;       1) Use internal-get-face, not facemenu-get-face.
;;       2) Use face-name for face arg to doremi-face-set.
;;     doremi-increment-face-fg-color (and -bg-) (bug fix): Use internal-get-face.
;;     doremi-frame-move-wrap-within-display: defvar -> defcustom.
;; 2005/01/18 dadams
;;     Added Note on saving changes.
;; 2005/01/16 dadams
;;     1. Added: doremi-face-set, doremi-face-foreground, doremi-face-background.
;;     2. doremi-face-fg, doremi-face-bg, doremi-increment-face-fg-color,
;;        doremi-increment-face-bg-color: Use doremi-face-set and doremi-face-foreground
;;                                        or doremi-face-background.
;; 2005/01/15 dadams
;;     doremi-increment-color and functions that call it: default is hue.
;;     Added: doremi-bg-cyan, doremi-bg-magenta, doremi-bg-yellow.
;; 2005/01/09 dadams
;;     Renamed: doremi-bg-rgb to doremi-bg, doremi-increment-face-bg-hex to
;;       doremi-increment-face-bg-color, doremi-increment-face-fg-hex to
;;       doremi-increment-face-fg-color, doremi-face-bg-rgb to doremi-face-bg,
;;       doremi-face-fg-rgb to doremi-face-fg, doremi-increment-background-hex to
;;       doremi-increment-background-color.
;;     Treat HSV now too: doremi-bg, doremi-increment-background-color, doremi-face-fg,
;;       doremi-increment-face-fg-color, doremi-face-bg,
;;       doremi-increment-face-bg-color, doremi-bg-value.
;;     Added: doremi-bg-hue, doremi-bg-saturation, doremi-bg-value (HSV version),
;;       doremi-bg-brightness, doremi-bg-purity, doremi-push-frame-config-for-cmds-flag,
;;       doremi-increment-color, doremi-increment-face-color-read-args.
;;     doremi-increment-background-color, doremi-increment-face-bg-color,
;;       doremi-increment-face-fg-color: Factored out common parts to create
;;       doremi-increment-color and doremi-increment-face-color-read-args.
;;     Fixed to use characters, not symbols: doremi-bg-red, doremi-bg-green,
;;       doremi-bg-blue, doremi-bg-hue, doremi-bg-saturation, doremi-bg-value,
;;       doremi-bg-brightness, doremi-bg-purity.
;;     Do not do doremi-push-frame-config-for-command by default
;;       (doremi-push-frame-config-for-cmds-flag is nil).
;; 2005/01/08 dadams
;;     Moved doremi-grow-font to frame-cmds.el, and renamed it to enlarge-font.
;; 2005/01/07 dadams
;;     doremi-grow-font: Treat error when new size is too small.
;; 2005/01/01 dadams
;;     defvar -> defcustom.  Added (defgroup doremi-frm).
;; 2004/12/28 dadams
;;     doremi-bg-rgb:
;;       You can now chain from changing one parameter to another.
;;       Color parameter (r,g,b,v) is now character type, not symbol type.
;;       Changed arg order.
;;     doremi-increment-background-hex:
;;       COMPONENT is a character, not symbol.
;;       Changed arg order.
;;     Added: doremi-face-bg-rgb, doremi-face-fg-rgb, doremi-increment-face-bg-hex,
;;            doremi-increment-face-fg-hex.
;; 2004/11/28 dadams
;;     Rewrote doremi-frame-horizontally and doremi-frame-vertically to:
;;       1) move frame off the display
;;       2) wrap frame around display
;;     Added: doremi-frame-new-position, doremi-frame-move-wrap-within-display.
;;     Require frame-fns.el[c].  Hard require ring+.el[c].
;; 2004/10/17 dadams
;;     doremi-grow-font: Fixed for Emacs 21: set point size and width to "*"
;; 2004/10/11 dadams
;;     doremi-frame-(horizontally|vertically):
;;       1. If start off screen, move frame back on screen (no error).
;;       2. Use modify-frame-parameters, not set-frame-position, bc unchanging
;;          value could be a cons.
;;       3. Chain each off of the other, so can use all four arrows.
;; 2004/09/26 dadams
;;     Renamed do-re-mi* to doremi*.
;;     Prefixed everything here with doremi-.
;;     Removed "adjust", "cycle", and "move" from names.
;; 2004/09/23 dadams
;;     doremi-grow-font: Removed font-info stuff (unused).
;;     doremi-frame-width, doremi-frame-horizontally:
;;            Changed key sequences to events.
;; 2004/09/21 dadams
;;     doremi-push-frame-config-for-command: Message only if interactive-p.
;; 2004/09/20 dadams
;;     Added: doremi-bg-blue, doremi-bg-brightness,
;;            doremi-bg-green, doremi-bg-red,
;;            doremi-bg-rgb, doremi-increment-background-hex.
;;     Renamed doremi-adjust-bg-color to doremi-bg-color-name.
;;     Changed suggested binding C-xtc to doremi-bg-rgb.
;;     Apply doremi-push-frame-config-for-command to new commands.
;; 2004/09/19 dadams
;;     Corrected interactive spec for doremi-font-size
;; 2004/09/17 dadams
;;     Added non-nil allow-new-p to doremi-adjust-bg-color
;; 2004/09/11 dadams
;;     Created this from stuff in doremi.el and frame-cmds.el
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'doremi) ;; doremi, doremi-limit, doremi-wrap
(require 'hexrgb) ;; hexrgb-color-name-to-hex, hexrgb-color-values-to-hex,
                  ;; hexrgb-defined-colors, hexrgb-defined-colors-alist,
                  ;; hexrgb-increment-blue, hexrgb-increment-green, hexrgb-increment-red,
                  ;; hexrgb-hsv-to-rgb, hexrgb-rgb-to-hsv
(require 'ring+)  ;; ring-insert, ring-member, ring-next
(require 'frame-fns) ;; frame-geom-spec-cons, frame-geom-value-cons, get-a-frame
(require 'faces+) ;; face-background-20+, face-foreground-20+, Emacs 20: read-face-name
(if (fboundp 'defvaralias) ;; Emacs 22
    (require 'palette nil t) ;; eyedrop-picked-background, eyedrop-picked-foreground
  (require 'eyedropper nil t)) ;; eyedrop-picked-background, eyedrop-picked-foreground
(require 'frame-cmds nil t) ;; (no error if not found):
                            ;; frame-configuration-to-register, enlarge-font
                            ;; jump-to-frame-config-register
(unless (fboundp 'read-number)
  (require 'strings nil t)) ;; (no error if not found): read-number (std in Emacs 22)

(eval-when-compile (require 'cl)) ;; case (plus, for Emacs<21: pop)

;; Quiet the byte-compiler
(defvar text-scale-mode)                ; In `face-remap.el' (Emacs 23+)
(defvar text-scale-mode-amount)         ; In `face-remap.el' (Emacs 23+)

;;;;;;;;;;;;;;;;;;;;;;;;
 
;;; User Options (Variables)

(defgroup doremi-frame-commands nil
  "Commands to incrementally adjust face attributes and frame parameters."
  :prefix "doremi-" :group 'doremi :group 'frames :group 'faces
  :link `(url-link :tag "Send Bug Report"
          ,(concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=\
doremi-frm.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and library versions."))
  :link '(url-link :tag "Other Libraries by Drew"
          "http://www.emacswiki.org/cgi-bin/wiki/DrewsElispLibraries")
  :link '(url-link :tag "Download"
          "http://www.emacswiki.org/cgi-bin/wiki/doremi-frm.el")
  :link '(url-link :tag "Description"
          "http://www.emacswiki.org/cgi-bin/wiki/Doremi")
  :link '(emacs-commentary-link :tag "Commentary" "doremi-frm")
  )

(defcustom doremi-frame-config-ring-size 20
  "*Maximum number of stored frame configurations."
  :type 'integer :group 'Doremi-Frame-Commands)

(defcustom doremi-move-frame-wrap-within-display-flag t
  "*Non-nil means wrap frame movements within the display.
Commands `doremi-frame-horizontally+' and `doremi-frame-vertically+'
then move the frame back onto the display when it moves off of it.
If nil, you can move the frame as far off the display as you like."
  :type 'boolean :group 'doremi-frame-commands)

(defcustom doremi-push-frame-config-for-cmds-flag nil
  "*Non-nil means commands that change frame config save it first.
This is done by advising all commands that change frame configuration
when library `doremi-frm.el' is loaded."
  :type 'boolean :group 'Doremi-Frame-Commands)

(defcustom doremi-RGB-increment-factor 1
  "*Factor to scale up RGB incrementing for some Do Re Mi functions.
Because RGB incrementing is by nature finer scale than HSV
incrementing, some Do Re Mi commands automatically scale up the
incrementing by this factor, so you need not iterate (cycle) so many
times to see an appreciable change.  When this is the case, it is
noted for the individual function.

The scale factor to use depends on how many hex digits there are in
your color representations.  A scale factor of 16 (and an input
increment of 1) means that, for each RGB component, it is the second
component digit from the right, not the rightmost, that is incremented
with each key press.  A factor of 256 means that the third digit from
the right cycles.  The default value is 1: no scaling.

If the digit that would be cycled is greater than the length of your
color components, then no incrementation occurs.  For example, if the
colors you use have the format #RRGGBB, so that each component has two
hex digits, then a factor of 256 is not appropriate, since it leaves
the component value unchanged (wraparound).  In that case, change the
value.

In general, 256 is good for colors represented as #RRRRGGGGBBBB, 16 is
good for #RRRGGGBBB, and 1 (no scaling) is appropriate for #RRGGBB.

What counts is the color representation you use, not what Emacs can
actually display for your screen.  On most platforms, Emacs can really
only display 8-bit color components, so #RRGGBB is the best it can do.
But you might well have defined your colors using the format
#RRRRGGGGBBBB.  That's OK, and it lets you see information reflecting
a more precise correspondance between RGB codes and color names, but
that extra precision is in fact ignored by Emacs and your display.

Personally, I use the longer format, ##RRRRGGGGBBBB, because I like to
see more info about the colors I use, even though my display cannot
really distinguish that many.  I also use libraries `hexrgb.el' and
`palette.el', and I convert color information between various formats
\(RGB, HSV, color names).  So I prefer to use the finer-grained
format, even though I can't see all the differences it provides.
Thus, I customize this option to 256.

The commands that use this option to scale up incrementing do so for
convenience.  You can always use other commands that perform no such
scaling.  For example, `doremi-bg+' scales RGB, but you can use
`doremi-increment-background-color' instead, for finer tuning."
  :type 'integer :group 'doremi-frame-commands)

(defcustom doremi-wrap-color-flag t
  "*Non-nil means wrap color changes around past the max and min.
For example, if non-nil, a current color value has FFFF as the red
component, and the red component is incremented by 1, then the result
has a red component of 0000.  If nil, the same example yields FFFF,
because the red component is already at its maximum."
  :type 'boolean :group 'doremi-frame-commands)
 
;;; Internal Variables

(defvar doremi-frame-config-ring (make-ring doremi-frame-config-ring-size)
  "Frame configuration ring.")

;; An Emacs 22 bug doesn't let us add t as a third arg here for `copy-face'.
(defvar doremi-last-face-value (cons 'doremi-last-face
                                     (copy-face 'default 'doremi-last-face))
  "Previous value of the last face changed by Do Re Mi.
That is, changed by `doremi-face-*' or `doremi-undo-last-face-change',
but not by `doremi-all-faces-*'.

A `consp' with the face name as `car' and the face value as `cdr'.
The face named `doremi-last-face' is a copy of the face before the
change.

Command `doremi-undo-last-face-change' swaps the `cdr' with the
current value of the face named by the `car', so it toggles between
the last two values of the face.")

(defvar doremi-last-frame-color nil
  "Previous value of last frame color changed by Do Re Mi.
That is, changed by `doremi-fg+' or `doremi-bg+' (or
`doremi-undo-last-frame-color-change' or
`doremi-increment-*ground-color' when used interactively), but not by
`doremi-all-frames-fg+' or `doremi-all-frames-bg+'.

A `consp' with `foreground-color' or `background-color' as `car' and
the color as `cdr'.

Command `doremi-undo-last-frame-color-change' swaps this with the
current color, so it toggles between the last two values.")

(defvar doremi-current-increment 0
  "Increment input by user for current Do Re Mi command.")
 
;;; Miscellaneous Do Re Mi Frame Commands

;; This command uses an incremental growth function, `enlarge-font',
;; which is defined in `frame-cmds.el'.
;;;###autoload
(defalias 'doremi-font-size+ 'doremi-frame-font-size+)
(defun doremi-frame-font-size+ (&optional increment frame)
  "Change font size for FRAME by INCREMENT.
Interactively, INCREMENT is given by the prefix argument.
Optional FRAME parameter defaults to current frame."
  (interactive "p")
  (doremi (lambda (inc) (enlarge-font inc frame)
            (cdr (assq 'font (frame-parameters frame))))
          (cdr (assq 'font (frame-parameters frame)))
          increment
          t))

;; This command uses an incremental growth function, `text-scale-increase',
;; which is defined in `face-remap.el' or (enhanced) in `face-remap+.el'.
;;;###autoload
(when (fboundp 'text-scale-increase)    ; Emacs 23+.
  (defun doremi-buffer-font-size+ (&optional increment)
    "Change font size for current buffer by INCREMENT steps.
Interactively, INCREMENT is given by the prefix argument."
    (interactive "p")
    (unless (require 'face-remap nil t)
      (error "This command requires library `face-remap.el'"))
    (doremi (lambda (inc)
              (let ((text-scale-mode-step  1.1)) (text-scale-increase inc))
              (if text-scale-mode text-scale-mode-amount 0))
            (if text-scale-mode text-scale-mode-amount 0)
            increment
            t)))

;; You can replace the enumeration list (x-list-fonts "*") with a list
;; of fonts you have.  A short list is easier to work with, but you
;; can use long lists like these too:
;;   (x-list-fonts "*")
;;   (append w32-fixed-font-alist (list (generate-fontset-menu)))
;;
;; For example, you can use a short list like this:
;;
;; ("-*-Garamond-normal-i-*-*-*-*-96-96-p-*-iso8859-2"
;;  "-*-*-normal-r-*-*-15-112-96-96-c-*-fontset-iso8859_1_15"
;;  "-*-Arial-bold-i-*-*-*-*-96-96-p-*-iso8859-1"
;;  "-*-Century Gothic-bold-i-*-*-*-*-96-96-p-*-iso8859-5"
;;  "-*-Microsoft Sans Serif-normal-r-*-*-*-*-96-96-p-*-iso8859-4")
;;
;;;###autoload
(defun doremi-font+ ()
  "Successively cycle among fonts, choosing by name.
Operates on the current frame. Cycled font list is (x-list-fonts \"*\")."
  (interactive)
  (doremi (lambda (newval) (set-frame-font newval) newval)
          (frame-parameter (selected-frame) 'font)
          nil                           ; ignored
          nil                           ; ignored
          (x-list-fonts "*")
          'extend))


;; This command uses an absolute setting function.  It rebinds `doremi-up-keys'
;; and `doremi-down-keys' so they are more intuitive for width.
;;;###autoload
(defun doremi-frame-width+ (&optional increment frame)
  "Change width of current frame incrementally.
Width of frame FRAME is increased in increments of amount INCREMENT."
  (interactive "p")
  (let ((doremi-up-keys          '(left)) ; More intuitive keys for width.
        (doremi-boost-up-keys    '(M-left))
        (doremi-down-keys        '(right))
        (doremi-boost-down-keys  '(M-right)))
    (doremi (lambda (new-val) (set-frame-width frame new-val) new-val)
            (frame-width frame)
            (- increment)))             ; Reverse, so arrows correspond.
  (when (member (car unread-command-events)
                (append doremi-up-keys   doremi-boost-up-keys
                        doremi-down-keys doremi-boost-down-keys))
    (doremi-frame-height+ increment frame)))

;; This command uses an absolute setting function.
;;;###autoload
(defun doremi-frame-height+ (&optional increment frame)
  "Change height of current frame incrementally.
Height of frame FRAME is increased in increments of amount INCREMENT."
  (interactive "p")
  (doremi (lambda (new-val) (set-frame-height frame new-val) new-val)
          (frame-height frame)
          (- increment))                ; Reverse, so arrows correspond.
  (when (member (car unread-command-events) '(left right M-left M-right))
    (doremi-frame-width+ increment frame)))

;; ;; This does the same thing as `doremi-frame-height+'.
;; ;; Example command that uses an incrementing function, `enlarge-frame',
;; ;; defined in `frame-cmds.el'.
;; (defun doremi-frame-height-bis+ (&optional increment frame)
;;   "Change frame height incrementally."
;;   (interactive "p")
;;   (doremi (lambda (inc) (enlarge-frame inc frame) (frame-height frame))
;;             (frame-height frame)
;;             (- increment)               ; Reverse, so arrows correspond.
;;             t))

;; Move frame left/right incrementally.
;; This command uses an incremental growth function.
;; Rebinds `doremi-up-keys' and `doremi-down-keys': more intuitive for horizontal.
;; Uses default increment value of 10.
;;;###autoload
(defun doremi-frame-horizontally+ (&optional increment frame)
  "Move frame left/right incrementally.
Prefix arg is the INCREMENT to move (default value interactively: 10).
FRAME defaults to the selected frame.

Variable `doremi-move-frame-wrap-within-display-flag' controls whether
or not you can move the frame completely off the display. The default
behavior (value `t') is to wrap frame movement around the display."
  (interactive
   (list (if current-prefix-arg (prefix-numeric-value current-prefix-arg) 10)))
  (setq increment  (or increment 10)    ; 1 is too small
        frame      (or frame (selected-frame)))
  (let ((doremi-up-keys          '(left)) ; More intuitive keys for width.
        (doremi-boost-up-keys    '(M-left))
        (doremi-down-keys        '(right))
        (doremi-boost-down-keys  '(M-right)))
    (doremi (lambda (incr)
              (modify-frame-parameters
               frame
               (list (list 'left '+ (doremi-frame-new-position frame 'left incr))))
              (frame-geom-spec-cons (assq 'left (frame-parameters frame)) frame))
            (frame-geom-spec-cons (assq 'left (frame-parameters frame)) frame)
            (- increment)               ; Reverse, so arrows correspond.
            t))
  (when (member (car unread-command-events)
                (append doremi-up-keys   doremi-boost-up-keys 
                        doremi-down-keys doremi-boost-down-keys))
    (doremi-frame-vertically+ increment frame)))

;; Move frame up/down incrementally.
;; This command uses an incremental growth function.
;; Uses default increment value of 10.
;;;###autoload
(defun doremi-frame-vertically+ (&optional increment frame)
  "Move frame up/down incrementally.
Prefix arg is the INCREMENT to move (default value interactively: 10).
FRAME defaults to the selected frame.

Variable `doremi-move-frame-wrap-within-display-flag' controls whether or
not you can move the frame completely off the display. The default
behavior (value `t') is to wrap frame movement around the display."
  (interactive
   (list (if current-prefix-arg (prefix-numeric-value current-prefix-arg) 10)))
  (setq increment  (or increment 10)    ; 1 is too small
        frame      (or frame (selected-frame)))
  (doremi (lambda (incr)
            (modify-frame-parameters
             frame
             (list (list 'top '+ (doremi-frame-new-position frame 'top incr))))
            (frame-geom-spec-cons (assq 'top (frame-parameters frame)) frame))
          (frame-geom-spec-cons (assq 'top (frame-parameters frame)) frame)
          (- increment)                 ; Reverse, so arrows correspond.
          t)
  (when (member (car unread-command-events) '(left right M-left M-right))
    (doremi-frame-horizontally+ increment frame)))

(defun doremi-frame-new-position (frame type incr)
  "Return the new TYPE position of FRAME, incremented by INCR.
TYPE is `left' or `top'.
INCR is the increment to use when changing the position."
  (let ((new-pos
         (+ incr (cadr (frame-geom-value-cons
                        type (cdr (assq type (frame-parameters frame)))))))
        (display-dimension
         (if (eq 'left type) (x-display-pixel-width) (x-display-pixel-height)))
        (frame-dimension
         (if (eq 'left type) (frame-pixel-width frame) (frame-pixel-height frame))))
    (if (not doremi-move-frame-wrap-within-display-flag)
        new-pos
      (when (< new-pos (- frame-dimension)) (setq new-pos  display-dimension))
      (when (> new-pos display-dimension) (setq new-pos  (- frame-dimension)))
      new-pos)))

;;;###autoload
(defun doremi-push-current-frame-config ()
  "Push the current frame configuration to `doremi-frame-config-ring'
after removing frame parameters `buffer-list' and `minibuffer'."
  (let ((curr-conf  (doremi-frame-config-wo-parameters (current-frame-configuration)
                                                       '(buffer-list minibuffer))))
    (unless (ring-member doremi-frame-config-ring curr-conf)
      (ring-insert doremi-frame-config-ring curr-conf))))


;;;###autoload
(defun doremi-frame-config-wo-parameters (frame-config params-to-remove)
  "A copy of FRAME-CONFIG, but without frame parameters PARAMS-TO-REMOVE."
  (cons 'frame-configuration
        (mapcar (lambda (fr+parms)
                  (let ((parms  (copy-sequence (nth 1 fr+parms))))
                    (list (car fr+parms) ; frame
                          (delete-if (lambda (parm) (memq (car parm) params-to-remove))
                                     parms)
                          (nth 2 fr+parms)))) ; window config
                (cdr frame-config))))   ; frames alist


;; NOTE: Frame parameters `buffer-list' and `minibuffer' are ignored
;;       when determining if two frame configurations are equal here.
;;;###autoload
(defun doremi-push-frame-config-for-command (command)
  "Advise COMMAND to save frame configuration.
You can restore previous frame configurations with \\[doremi-frame-configs+]."
  (when (featurep 'ring+)
    (eval
     `(defadvice ,command (around doremi-push-frame-config-for-command activate)
       "Saves frame configuration. You can restore previous frame configuration \
with \\[doremi-frame-configs+]."
       (doremi-push-current-frame-config)
       (when (fboundp 'frame-configuration-to-register) ; Defined in `frame-cmds.el'
         (frame-configuration-to-register frame-config-register))
       ad-do-it                         ; COMMAND code is executed here.
       (when (interactive-p)
         (message
          (substitute-command-keys
           (if (fboundp 'jump-to-frame-config-register) ; Defined in `frame-cmds.el'
               (format "Use `\\[jump-to-frame-config-register]' (`C-x r j %c') or \
`\\[doremi-frame-configs+]' to restore frames as before (undo)." frame-config-register)
             "Use `\\[doremi-frame-configs+]' to restore frames as before (undo)."))))))))


;; Undo (rotate) frame configuration changes made by the
;; frame-changing commands defined here (see mapcar, at end of file).
;;
;; Note:
;;;###autoload
(defun doremi-frame-configs+ ()
  "Cycle among frame configurations recorded in `doremi-frame-config-ring'."
  (interactive)
  (when (featurep 'ring+)
    (doremi (lambda (newval)            ; Cycle among previous frame configs.
              (set-frame-configuration (ring-next doremi-frame-config-ring newval))
              newval)
            (doremi-frame-config-wo-parameters (current-frame-configuration)
                                               '(buffer-list minibuffer))
            nil
            nil
            doremi-frame-config-ring
            t)))
 
;;; Background Frame Color Commands

;; (This is for both background and foreground changes.)
;;;###autoload
(defun doremi-undo-last-frame-color-change (&optional frame)
  "Restore last frame color changed by `doremi-fg+' or `doremi-bg+'.
This acts as a toggle between the last two values.
Optional arg FRAME defaults to the selected frame.
  The last frame-color change must have been to FRAME, or the result
  will likely not be what you expect.
Note: This does not undo changes made by `doremi-all-frames-fg+' or
`doremi-all-frames-bg+'"
  (interactive)
  (unless doremi-last-frame-color (error "No undo - no last frame color."))
  (let ((temp  (assq (car doremi-last-frame-color) (frame-parameters frame))))
    (modify-frame-parameters (or frame (selected-frame)) `(,doremi-last-frame-color))
    (setq doremi-last-frame-color  temp)))

;; Do not use this non-interactively - use `doremi-bg-fg-color-name-1'.
;;;###autoload
(defun doremi-bg-color-name+ (&optional frame interactive-p)
  "Successively cycle among background colors, choosing by name.
Operates on FRAME, which is the current frame when interactive."
  (interactive (list (selected-frame) t))
  (let ((curr-bg   (assq 'background-color (frame-parameters frame))))
    (when interactive-p (setq doremi-last-frame-color  curr-bg))
    (condition-case nil
        (doremi-bg/fg-color-name-1 'background-color frame (car curr-bg))
      (quit (modify-frame-parameters frame (list curr-bg))))))

;;;###autoload
(defun doremi-bg-red+ (&optional increment)
  "Change frame background red value incrementally.  See `doremi-bg+'.
Prefix arg is the INCREMENT to change."
  (interactive "p")
  (doremi-bg+ ?r increment))

;;;###autoload
(defun doremi-bg-green+ (&optional increment)
  "Change frame background green value incrementally.  See `doremi-bg+'.
Prefix arg is the INCREMENT to change."
  (interactive "p")
  (doremi-bg+ ?g increment nil (consp current-prefix-arg)))

;;;###autoload
(defun doremi-bg-blue+ (&optional increment)
  "Change frame background blue value incrementally.  See `doremi-bg+'.
Prefix arg is the INCREMENT to change."
  (interactive "p")
  (doremi-bg+ ?b increment nil (consp current-prefix-arg)))

;;;###autoload
(defun doremi-bg-cyan+ (&optional increment)
  "Change frame background cyan value incrementally.  See `doremi-bg+'.
Prefix arg is the INCREMENT to change."
  (interactive "p")
  (doremi-bg+ ?r (- increment) nil (consp current-prefix-arg)))

;;;###autoload
(defun doremi-bg-magenta+ (&optional increment)
  "Change frame background green value incrementally.  See `doremi-bg+'.
Prefix arg is the INCREMENT to change."
  (interactive "p")
  (doremi-bg+ ?g (- increment) nil (consp current-prefix-arg)))

;;;###autoload
(defun doremi-bg-yellow+ (&optional increment)
  "Change frame background blue value incrementally.  See `doremi-bg+'.
Prefix arg is the INCREMENT to change."
  (interactive "p")
  (doremi-bg+ ?b (- increment) nil (consp current-prefix-arg)))

;;;###autoload
(defun doremi-bg-hue+ (&optional increment)
  "Change frame background hue incrementally.  See `doremi-bg+'.
Prefix arg is the INCREMENT to change."
  (interactive "p")
  (doremi-bg+ ?h increment nil (consp current-prefix-arg)))

;;;###autoload
(defun doremi-bg-saturation+ (&optional increment)
  "Change frame background color saturation incrementally.
Prefix arg is the INCREMENT to change.  See `doremi-bg+'."
  (interactive "p")
  (doremi-bg+ ?s increment nil (consp current-prefix-arg)))

;;;###autoload
(defun doremi-bg-value+ (&optional increment)
  "Change frame background brightness (HSV \"value\") incrementally.
Prefix arg is the INCREMENT to change.  See `doremi-bg+'."
  (interactive "p")
  (doremi-bg+ ?v increment nil (consp current-prefix-arg)))

(defalias 'doremi-bg-brightness+ 'doremi-bg-value+)
(defalias 'doremi-bg-purity+ 'doremi-bg-saturation+)

;; Do not use this non-interactively - use `doremi-bg-1'.
;;;###autoload
(defun doremi-bg+ (component &optional increment frame pickup-p interactive-p)
  "Change FRAME's background color incrementally.
Optional arg FRAME defaults to the selected frame.

You are prompted for the color COMPONENT to increment/decrement (a
character):
  `r' - red
  `g' - green
  `b' - blue
  `h' - hue (basic color)
  `s' - saturation (purity)
  `v' - value (brightness)

  `R' - red, green, and blue, at the same time
  `H' - hue, saturation, and value, at the same time

`R' and `H' increment all components of the respective color spaces,
according to the value of INCREMENT.

You can at any time change, to increment/decrement a different color
component (r, g, b, h, s, v, R, or H).  For example, you can type `r'
and use the arrow keys or mouse wheel to change the red component,
then type `b' and use the arrows or wheel to change the blue
component, and so on, all in the same call to `doremi-bg+'.

Tip: To increment or decrement the cyan, magenta, or yellow component,
     just decrement or increment the red, green, or blue component,
     respectively.  CMY is just the opposite direction from RGB.

INCREMENT is the increment to change.  The value can be a number or a
list of 3 numbers.  The default value is 1.  You can use a prefix
argument to specify a number value.  Otherwise, you are prompted to
input the value.

If the value is a list of 3 numbers, they are used to increment the
individual components red, green, and blue, respectively, as well as
hue, saturation, and value, respectively.  If you change the
component(s) to increment, then the original input INCREMENT is
reapplied.

For example, if INCREMENT is (0.2 -0.5 1.1) and the initial COMPONENT
value is `R', then red is incremented by 0.2, green by -0.5, and blue
by 1.1.  If you then hit `h', hue is incremented by 0.2.  If you then
hit `b', blue is incremented by 1.1.

For RGB, INCREMENT is actually multiplied by
`doremi-RGB-increment-factor', for convenience.  If you need finer
control than that provides, use command
`doremi-increment-background-color' to refine the color.  If it seems
that no incrementing occurs, then reduce
`doremi-RGB-increment-factor'.

The initial color value is converted to a hexadecimal RGB (red, green,
blue) string that starts with \"#\".  The initial value is the current
background color of the selected frame.

If `eyedrop-picked-background' is non-nil and you use plain `C-u'
instead of a numeric prefix argument (or, non-interactively, PICKUP-P
is non-nil), then the frame background is first set to the value of
`eyedrop-picked-background'.  This happens only if library
`eyedropper.el' or `palette.el' is loaded.  This lets you pick up a
background color from somewhere, using `eyedrop-pick-background-at-*',
and then use that as the initial value for `doremi-bg+'.

Colors can be expressed in Emacs as color names or hex RGB strings.
Depending on your operating system, the RGB components for a given
Emacs color can have different numbers of hex digits.  For example, on
one system RGB component values might vary from 000 to FFF; on another
system they might vary from 0000 to FFFF.  Incrementing or
decrementing a given color's RGB spec makes it roll over when the
limit (say 000 or FFF) is reached.

As for all Do Re Mi incrementation commands, use
`doremi-boost-up-keys' and `doremi-boost-down-keys' for faster
incrementation.  The change is `doremi-boost-scale-factor' times
faster than for `doremi-up-keys' and `doremi-down-keys'."
  (interactive (list (doremi-read-component) (doremi-read-increment-arg 3 1) nil nil t))
  (when interactive-p
    (setq doremi-last-frame-color  (assq 'background-color (frame-parameters frame))))
  (when (and (or pickup-p (and interactive-p (or (consp current-prefix-arg))))
             (boundp 'eyedrop-picked-background) eyedrop-picked-background)
    (doremi-set-background-color eyedrop-picked-background frame))
  (let ((curr-bg   (assq 'background-color (frame-parameters frame)))
        (curr-frm  frame))
    (condition-case nil
        (doremi-bg-1 component increment frame)
      (quit (modify-frame-parameters curr-frm (list curr-bg))))))

;; Do not use this non-interactively - use `doremi-frame-hue-stepping-saturation'.
;;;###autoload
(defun doremi-bg-hue-stepping-saturation+ (&optional increment frame pickup-p
                                           interactive-p)
  "Increment frame background hue, stepping saturation down after each cycle.
Repeatedly increment hue until it reaches its maximum.  Then increment
saturation once.  Then repeatedly increment hue again - and so on.

You can think of this as moving along a row of the hue x saturation
color plane, then down to the next row and across, and so on.

See `doremi-bg+' for more info (e.g. other args)."
  (interactive (list (doremi-read-increment-arg 3 1) nil nil t))
  (when interactive-p
    (setq doremi-last-frame-color  (assq 'background-color (frame-parameters frame))))
  (when (and (or pickup-p (and interactive-p (or (consp current-prefix-arg))))
             (boundp 'eyedrop-picked-background) eyedrop-picked-background)
    (doremi-set-background-color eyedrop-picked-background frame))
  (unless increment (setq increment  1))
  (let ((curr-bg   (assq 'background-color (frame-parameters frame)))
        (curr-frm  frame))
    (condition-case nil
        (doremi-frame-hue-stepping-saturation 'background-color increment frame)
      (quit (modify-frame-parameters curr-frm (list curr-bg))))))

;;;###autoload
(defun doremi-all-frames-bg+ (component increment)
  "Change background color of all visible frames incrementally.
You are prompted for the color COMPONENT to increment.
Prefix arg is the INCREMENT to change; the default value is 1.

See command `doremi-bg+'.  This command behaves similarly, but it
changes the background color of all frames, not just one frame.

NOTE: You cannot use `C-g' to cancel and revert changes you make using
this command, and you cannot use `doremi-undo-last-frame-color-change'
to undo changes.  (There is no single initial color to revert to,
since multiple frames are affected.)

For RGB, INCREMENT is multiplied by `doremi-RGB-increment-factor', for
convenience.  If you need finer control than that provides, use
command `doremi-increment-background-color' to refine the color.  If
it seems that no incrementing occurs, then reduce
`doremi-RGB-increment-factor'.

Option `doremi-wrap-color-flag' is bound to nil during this command,
which means that an individual color change stops when the limit is
reached."
  (interactive (list (doremi-read-component) (doremi-read-increment-arg 3 1)))
  (unless increment (setq increment  1))
  (doremi-all-frames-bg/fg-1 'background-color component increment))

;; Do not use this non-interactively - use `doremi-increment-background-color-1'.
;;;###autoload
(defun doremi-increment-background-color (component increment
                                          &optional frame interactive-p)
  "Change frame background color by INCREMENT of color COMPONENT.
You are prompted for the color COMPONENT to increment/decrement.
COMPONENT and INCREMENT are as for `doremi-increment-color'.
Optional arg FRAME defaults to the selected frame.  See `doremi-bg+'."
  (interactive (list (doremi-read-component) (doremi-read-increment-arg 3 1)
                     nil 'interactive-p))
  (when interactive-p
    (setq doremi-last-frame-color  (assq 'background-color (frame-parameters frame))))
  (let ((curr-bg  (assq 'background-color (frame-parameters frame)))
        (curr-frm  frame))
    (condition-case nil
        (doremi-increment-background-color-1 component increment frame)
      (quit (modify-frame-parameters curr-frm (list curr-bg))))))

;; A function like this should be available in Emacs.
;;;###autoload
(defun doremi-set-background-color (color-name &optional frame interactive-p)
  "Set the background color of the FRAME to COLOR-NAME.
When called interactively, prompt for the name of the color to use.
To get the frame's current background color, use `frame-parameters'.
This is the same as `set-background-color', except that this accepts a
FRAME parameter."
  (interactive (list (facemenu-read-color) nil 'interactive-p))
  (when interactive-p
    (setq doremi-last-frame-color  (assq 'background-color (frame-parameters frame))))
  (doremi-set-frame-color 'background-color color-name frame))
 
;;; Foreground Frame Color Commands

;; Do not use this non-interactively - use `doremi-bg-fg-color-name-1'.
;;;###autoload
(defun doremi-fg-color-name+ (&optional frame interactive-p)
  "Successively cycle among foreground colors, choosing by name.
Operates on FRAME, which is the current frame when interactive."
  (interactive (list (selected-frame) t))
  (let ((curr-fg   (assq 'foreground-color (frame-parameters frame))))
    (when interactive-p (setq doremi-last-frame-color  curr-fg))
    (condition-case nil
        (doremi-bg/fg-color-name-1 'foreground-color frame (car curr-fg))
      (quit (modify-frame-parameters frame (list curr-fg))))))

;;;###autoload
(defun doremi-fg-red+ (&optional increment)
  "Change frame foreground red value incrementally.  See `doremi-fg+'.
Prefix arg is the INCREMENT to change."
  (interactive "p")
  (doremi-fg+ ?r increment nil (consp current-prefix-arg)))

;;;###autoload
(defun doremi-fg-green+ (&optional increment)
  "Change frame foreground green value incrementally.  See `doremi-fg+'.
Prefix arg is the INCREMENT to change."
  (interactive "p")
  (doremi-fg+ ?g increment nil (consp current-prefix-arg)))

;;;###autoload
(defun doremi-fg-blue+ (&optional increment)
  "Change frame foreground blue value incrementally.  See `doremi-fg+'.
Prefix arg is the INCREMENT to change."
  (interactive "p")
  (doremi-fg+ ?b increment nil (consp current-prefix-arg)))

;;;###autoload
(defun doremi-fg-cyan+ (&optional increment)
  "Change frame foreground cyan value incrementally.  See `doremi-fg+'.
Prefix arg is the INCREMENT to change."
  (interactive "p")
  (doremi-fg+ ?r (- increment) nil (consp current-prefix-arg)))

;;;###autoload
(defun doremi-fg-magenta+ (&optional increment)
  "Change frame foreground green value incrementally.  See `doremi-fg+'.
Prefix arg is the INCREMENT to change."
  (interactive "p")
  (doremi-fg+ ?g (- increment) nil (consp current-prefix-arg)))

;;;###autoload
(defun doremi-fg-yellow+ (&optional increment)
  "Change frame foreground blue value incrementally.  See `doremi-fg+'.
Prefix arg is the INCREMENT to change."
  (interactive "p")
  (doremi-fg+ ?b (- increment) nil (consp current-prefix-arg)))

;;;###autoload
(defun doremi-fg-hue+ (&optional increment)
  "Change frame foreground hue incrementally.  See `doremi-fg+'.
Prefix arg is the INCREMENT to change."
  (interactive "p")
  (doremi-fg+ ?h increment nil (consp current-prefix-arg)))

;;;###autoload
(defun doremi-fg-saturation+ (&optional increment)
  "Change frame foreground color saturation incrementally.
See `doremi-fg+'.  Prefix arg is the INCREMENT to change."
  (interactive "p")
  (doremi-fg+ ?s increment nil (consp current-prefix-arg)))

;;;###autoload
(defun doremi-fg-value+ (&optional increment)
  "Change frame foreground brightness (HSV \"value\") incrementally.
See `doremi-fg+'.  Prefix arg is the INCREMENT to change."
  (interactive "p")
  (doremi-fg+ ?v increment nil (consp current-prefix-arg)))

(defalias 'doremi-fg-brightness+ 'doremi-fg-value+)
(defalias 'doremi-fg-purity+ 'doremi-fg-saturation+)

;; Do not use this non-interactively - use `doremi-frame-hue-stepping-saturation'.
;;;###autoload
(defun doremi-fg-hue-stepping-saturation+ (&optional increment frame pickup-p
                                           interactive-p)
  "Increment frame foreground hue, stepping saturation down after each cycle.
See `doremi-bg-hue-stepping-saturation+'.
`doremi-fg-hue-stepping-saturation+' is the same, with \"foreground\"
substituted for \"background\"."
  (interactive (list (doremi-read-increment-arg 3 1) nil nil t))
  (when interactive-p
    (setq doremi-last-frame-color  (assq 'foreground-color (frame-parameters frame))))
  (when (and (or pickup-p (and interactive-p (or (consp current-prefix-arg))))
             (boundp 'eyedrop-picked-foreground) eyedrop-picked-foreground)
    (doremi-set-foreground-color eyedrop-picked-foreground frame))
  (unless increment (setq increment  1))
  (let ((curr-fg   (assq 'foreground-color (frame-parameters frame)))
        (curr-frm  frame))
    (condition-case nil
        (doremi-frame-hue-stepping-saturation 'foreground-color increment frame)
      (quit (modify-frame-parameters curr-frm (list curr-fg))))))

;;;###autoload
(defun doremi-fg+ (component &optional increment frame pickup-p interactive-p)
  "Change FRAME's foreground color incrementally.
See `doremi-bg+'; `doremi-fg+' is the same, with \"foreground\"
substituted for \"background\"."
  (interactive (list (doremi-read-component) (doremi-read-increment-arg 3 1) nil nil t))
  (when interactive-p
    (setq doremi-last-frame-color  (assq 'foreground-color (frame-parameters frame))))
  (when (and (or pickup-p (and interactive-p (or (consp current-prefix-arg))))
             (boundp 'eyedrop-picked-foreground) eyedrop-picked-foreground)
    (doremi-set-foreground-color eyedrop-picked-foreground frame))
  (let ((curr-fg   (assq 'foreground-color (frame-parameters frame)))
        (curr-frm  frame))
    (condition-case nil
        (doremi-fg-1 component increment frame)
      (quit (modify-frame-parameters curr-frm (list curr-fg))))))

;;;###autoload
(defun doremi-all-frames-fg+ (component increment)
  "Change foreground color of all visible frames incrementally.
You are prompted for the color COMPONENT to increment.
Prefix arg is the INCREMENT to change; the default value is 1.

See command `doremi-fg+'.  This command behaves similarly, but it
changes the foreground color of all frames, not just one frame.

NOTE: You cannot use `C-g' to cancel and revert changes you make using
this command, and you cannot use `doremi-undo-last-frame-color-change'
to undo changes.  (There is no single initial color to revert to,
since multiple frames are affected.)

For RGB, INCREMENT is multiplied by `doremi-RGB-increment-factor', for
convenience.  If you need finer control than this, use command
`doremi-increment-foreground-color' to refine the color.  If it seems
that no incrementing occurs, then reduce
`doremi-RGB-increment-factor'.

Option `doremi-wrap-color-flag' is bound to nil during this command,
which means that an individual color change stops when the limit is
reached."
  (interactive (list (doremi-read-component) (doremi-read-increment-arg 3 1)))
  (unless increment (setq increment  1))
  (doremi-all-frames-bg/fg-1 'foreground-color component increment))

;; Do not use this non-interactively - use `doremi-increment-foreground-color-1'.
;;;###autoload
(defun doremi-increment-foreground-color (component increment
                                          &optional frame interactive-p)
  "Change foreground color of FRAME by INCREMENT of color COMPONENT.
You are prompted for the color COMPONENT to increment/decrement.
COMPONENT and INCREMENT are as for `doremi-increment-color'.
Optional arg FRAME defaults to the selected frame.  See `doremi-bg+'."
  (interactive (list (doremi-read-component) (doremi-read-increment-arg 3 1)
                     nil 'interactive-p))
  (when interactive-p
    (setq doremi-last-frame-color  (assq 'foreground-color (frame-parameters frame))))
  (let ((curr-fg  (assq 'foreground-color (frame-parameters frame)))
        (curr-frm  frame))
    (condition-case nil
        (doremi-increment-foreground-color-1 component increment frame)
      (quit (modify-frame-parameters curr-frm (list curr-fg))))))

;; A function like this should be available in Emacs.
;;;###autoload
(defun doremi-set-foreground-color (color-name &optional frame interactive-p)
  "Set the foreground color of the FRAME to COLOR-NAME.
When called interactively, prompt for the name of the color to use.
To get the frame's current foreground color, use `frame-parameters'.
This is the same as `set-foreground-color', except that this accepts a
FRAME parameter."
  (interactive (list (facemenu-read-color) nil 'interactive-p))
  (when interactive-p
    (setq doremi-last-frame-color  (assq 'foreground-color (frame-parameters frame))))
  (doremi-set-frame-color 'foreground-color color-name frame))
 
;;; Face and Color Commands

(defalias 'toggle-doremi-wrap-color 'doremi-toggle-wrap-color)
;;;###autoload
(defun doremi-toggle-wrap-color ()
  "Toggle value of `doremi-wrap-color-flag'."
  (interactive)
  (setq doremi-wrap-color-flag  (not doremi-wrap-color-flag)))

;; (This is for both background and foreground changes.)
;;;###autoload
(defun doremi-undo-last-face-change ()
  "Return last face changed by `doremi-face-*' to its previous value.
This acts as a toggle between the last two values of the face.
Note: This does not undo changes made by `doremi-all-faces-fg+' or
`doremi-all-faces-bg+'."
  (interactive)
  (unless (facep 'doremi-last-face) (error "No undo - no last face."))
  (let ((face  (car doremi-last-face-value)))
    (copy-face face 'doremi-temp-face)  ; Save current value face.
    (copy-face 'doremi-last-face face)  ; Restore previous value.
    (setq doremi-last-face-value        ; Be able to get back the changed value.
          (cons face (copy-face 'doremi-temp-face 'doremi-last-face)))))

;;;###autoload
(defun doremi-face-bg+ (face component &optional increment pickup-p interactive-p)
  "Change background color of FACE incrementally.
The color is changed on all frames.
You are prompted for the FACE, the color COMPONENT to increment.
Unless you use a prefix argument, you are prompted for the INCREMENT.

See command `doremi-bg+'.  This command behaves the same, except that
it is the background color of FACE that is changed, not the frame
background color.

For RGB, INCREMENT is actually multiplied by
`doremi-RGB-increment-factor', for convenience.  If you need finer
control than this, use command `doremi-increment-face-bg-color' to
refine the color.  If it seems that no incrementing occurs, then
reduce `doremi-RGB-increment-factor'.

If `eyedrop-picked-background' is non-nil and you use plain `C-u'
instead of a numeric prefix argument (or, non-interactively, PICKUP-P
is non-nil), then the face background is first set to the value of
`eyedrop-picked-background'.  This happens only if library
`eyedropper.el' or `palette.el' is loaded.  This lets you pick up a
background color from somewhere, using `eyedrop-pick-background-at-*',
and then use that as the initial value for `doremi-face-bg+'."
  (interactive `(,@(doremi-increment-face-color-read-args) nil 'interactive-p))
  (unless (facep face)
    (error "Command `doremi-face-bg+': FACE arg is not a face name: %s" face))
  (when interactive-p
    (copy-face face 'doremi-last-face)
    (setq doremi-last-face-value  (cons face 'doremi-last-face)))
  (when (and (or pickup-p (and interactive-p (or (consp current-prefix-arg))))
             (boundp 'eyedrop-picked-background) eyedrop-picked-background)
    (set-face-background face eyedrop-picked-background))
  (let ((curr-bg  (face-background-20+ face nil 'default)))
    (condition-case nil
        (doremi-face-bg/fg-1 'background-color face component increment)
      (quit (set-face-background face curr-bg)))))

;; Do not use this non-interactively - use `doremi-face-hue-stepping-saturation'.
;;;###autoload
(defun doremi-face-bg-hue-stepping-saturation+ (face &optional increment pickup-p
                                                interactive-p)
  "Increment FACE background hue, stepping saturation down after each cycle.

See command `doremi-bg-hue-stepping-saturation+'.  This command
behaves the same, except that it is the background color of FACE that
is changed, not the frame background color.
See `doremi-face-bg+' for more info (e.g. other args)."
  (interactive (list (if (< emacs-major-version 21)
                         (read-face-name "Face to change: ")
                       (read-face-name "Face to change"))
                     (doremi-read-increment-arg 3 1)
                     nil
                     t))
  (unless (facep face)
    (error "Command `doremi-face-bg-hue-stepping-saturation+': \
FACE arg is not a face name: %s" face))
  (when interactive-p
    (copy-face face 'doremi-last-face)
    (setq doremi-last-face-value  (cons face 'doremi-last-face)))
  (when (and (or pickup-p (and interactive-p (or (consp current-prefix-arg))))
             (boundp 'eyedrop-picked-background) eyedrop-picked-background)
    (set-face-background face eyedrop-picked-background))
  (let ((curr-bg  (face-background-20+ face nil 'default)))
    (condition-case nil
        (doremi-face-hue-stepping-saturation 'background-color face increment)
      (quit (set-face-background face curr-bg)))))

;; Do not use this non-interactively - use `doremi-bg-fg-color-name-1'.
;;;###autoload
(defun doremi-face-bg-color-name+ (face &optional interactive-p)
  "Successively cycle among background colors for FACE, choosing by name.
The color is changed on all frames.
You are prompted for the FACE.

See command `doremi-bg-color-name+'.  This command behaves the same,
except that it is the background color of FACE that is changed, not
the frame background color."
  (interactive (list (read-face-name "Face to change: ") t))
  (when interactive-p
    (copy-face face 'doremi-last-face)
    (setq doremi-last-face-value  (cons face 'doremi-last-face)))
  (let ((curr-bg  (face-background-20+ face nil 'default)))
    (condition-case nil
        (doremi-face-bg/fg-color-name-1 'background-color face)
      (quit (set-face-background face curr-bg)))))
    
;;;###autoload
(defun doremi-all-faces-bg+ (component increment)
  "Change background color of all faces incrementally, for all frames.
See command `doremi-face-bg+'.  This command behaves similarly, but it
is the background color of all faces that is changed, not one face.

For RGB, INCREMENT is actually multiplied by
`doremi-RGB-increment-factor'.  If you need finer control than this,
use command `doremi-increment-face-bg-color' to refine the color.  If
it seems that no incrementing occurs, then reduce
`doremi-RGB-increment-factor'.

Option `doremi-wrap-color-flag' is bound to nil during this command.

NOTE: You cannot use `C-g' to cancel and revert changes you make using
this command, and you cannot use `doremi-undo-last-face-change' to
undo changes.  (There is no single initial color to revert to, since
multiple faces are affected.)"
  (interactive (list (doremi-read-component) (doremi-read-increment-arg 3 1)))
  (unless increment (setq increment  1))
  (doremi-all-faces-bg/fg-1 'background-color component increment))

;;;###autoload
(defun doremi-increment-face-bg-color (face component increment)
  "Change background color of FACE by INCREMENT of COMPONENT.
The color is changed on all frames.
You are prompted for the FACE and the color COMPONENT to increment.
COMPONENT and INCREMENT are as for `doremi-increment-color'."
  (interactive (doremi-increment-face-color-read-args))
  (unless (facep face)
    (error "Command `doremi-increment-face-bg-color': FACE arg is not a face: %s" face))
  (when (interactive-p)
    (copy-face face 'doremi-last-face)
    (setq doremi-last-face-value  (cons face 'doremi-last-face)))
  (doremi-increment-color component increment
                          (or (face-background-20+ face nil 'default)
                              (cdr (assq 'background-color (frame-parameters))))
                          'set-face-background nil face)
  (face-background-20+ face nil 'default)) ; Return new value.

;;;###autoload
(defun doremi-face-fg+ (face component &optional increment pickup-p interactive-p)
  "Change foreground color of FACE incrementally.
See `doremi-face-bg+'; `doremi-face-fg+' is the same, with
\"foreground\" substituted for \"background\"."
  (interactive `(,@(doremi-increment-face-color-read-args) nil 'interactive-p))
  (unless (facep face)
    (error "Command `doremi-face-fg+': FACE arg is not a face name: %s" face))
  (when interactive-p
    (copy-face face 'doremi-last-face)
    (setq doremi-last-face-value  (cons face 'doremi-last-face)))
  (when (and (or pickup-p (and interactive-p (or (consp current-prefix-arg))))
             (boundp 'eyedrop-picked-foreground) eyedrop-picked-foreground)
    (set-face-foreground face eyedrop-picked-foreground))
  (let ((curr-fg  (face-foreground-20+ face nil 'default)))
    (condition-case nil
        (doremi-face-bg/fg-1 'foreground-color face component increment)
      (quit (set-face-foreground face curr-fg)))))

;; Do not use this non-interactively - use `doremi-face-hue-stepping-saturation'.
;;;###autoload
(defun doremi-face-fg-hue-stepping-saturation+ (face &optional increment pickup-p
                                                interactive-p)
  "Increment FACE background hue, stepping saturation down after each cycle.
See `doremi-face-bg+' for info about the other args."
  (interactive (list (if (< emacs-major-version 21)
                         (read-face-name "Face to change: ")
                       (read-face-name "Face to change"))
                     (doremi-read-increment-arg 3 1)
                     nil
                     t))
  (unless (facep face)
    (error "Command `doremi-face-fg-hue-stepping-saturation+': \
FACE arg is not a face name: %s" face))
  (when interactive-p
    (copy-face face 'doremi-last-face)
    (setq doremi-last-face-value  (cons face 'doremi-last-face)))
  (when (and (or pickup-p (and interactive-p (or (consp current-prefix-arg))))
             (boundp 'eyedrop-picked-foreground) eyedrop-picked-foreground)
    (set-face-foreground face eyedrop-picked-foreground))
  (let ((curr-fg  (face-foreground-20+ face nil 'default)))
    (condition-case nil
        (doremi-face-hue-stepping-saturation 'foreground-color face increment)
      (quit (set-face-foreground face curr-fg)))))

;; Do not use this non-interactively - use `doremi-bg-fg-color-name-1'.
;;;###autoload
(defun doremi-face-fg-color-name+ (face &optional interactive-p)
  "Successively cycle among foreground colors for FACE, choosing by name.
The color is changed on all frames.
You are prompted for the FACE.

See command `doremi-fg-color-name+'.  This command behaves the same,
except that it is the foreground color of FACE that is changed, not
the frame foreground color."
  (interactive (list (read-face-name "Face to change: ") t))
  (when interactive-p
    (copy-face face 'doremi-last-face)
    (setq doremi-last-face-value  (cons face 'doremi-last-face)))
  (let ((curr-fg  (face-foreground-20+ face nil 'default)))
    (condition-case nil
        (doremi-face-bg/fg-color-name-1 'foreground-color face)
      (quit (set-face-foreground face curr-fg)))))
    
;;;###autoload
(defun doremi-all-faces-fg+ (component increment)
  "Change foreground color of all faces incrementally, for all frames.
See command `doremi-face-fg+'.  This command behaves similarly, but it
is the foreground color of all faces that is changed, not one face.

For RGB, INCREMENT is actually multiplied by
`doremi-RGB-increment-factor'.  If you need finer control than this,
use command `doremi-increment-face-fg-color' to refine the color.  If
it seems that no incrementing occurs, then reduce
`doremi-RGB-increment-factor'.

Option `doremi-wrap-color-flag' is bound to nil during this command.

NOTE: You cannot use `C-g' to cancel and revert changes you make using
this command, and you cannot use `doremi-undo-last-face-change' to
undo changes.  (There is no single initial color to revert to, since
multiple faces are affected.)"
  (interactive (list (doremi-read-component) (doremi-read-increment-arg 3 1)))
  (unless increment (setq increment  1))
  (doremi-all-faces-bg/fg-1 'foreground-color component increment))

;;;###autoload
(defun doremi-increment-face-fg-color (face component increment)
  "Change foreground color of FACE by INCREMENT of COMPONENT.
The color is changed on all frames.
You are prompted for the FACE and the color COMPONENT to increment.
COMPONENT and INCREMENT are as for `doremi-increment-color'."
  (interactive (doremi-increment-face-color-read-args))
  (unless (facep face)
    (error "Command `doremi-increment-face-fg-color': FACE arg is not a face: %s" face))
  (when (interactive-p)
    (copy-face face 'doremi-last-face)
    (setq doremi-last-face-value  (cons face 'doremi-last-face)))
  (doremi-increment-color component increment
                          (or (face-foreground-20+ face nil 'default)
                              (cdr (assq 'foreground-color (frame-parameters))))
                          'set-face-foreground nil face)
  (face-foreground-20+ face nil 'default)) ; Return new value.

;;;###autoload
(defun doremi-increment-color-component (component color increment)
  "Increase COMPONENT (RGB or HSV) of COLOR by INCREMENT.
Returns a hexadecimal RGB code (a string) for the new color, of the
form #RRRRGGGGBBBB (RRRR: red, GGGG: green, BBBB: blue).

COMPONENT is the color component to increment (a character):
  `r' - red
  `g' - green
  `b' - blue
  `h' - hue (basic color)
  `s' - saturation (purity)
  `v' - value (brightness)
  The default is `h' (hue).
COLOR is a string representing a color.  It can be a color name or a
  hexadecimal RGB string of the form #RRRRGGGGBBBB.
INCREMENT is the increment to increase the value component of COLOR."
  (interactive
   (list (read-char-exclusive           ; Not `doremi-read-component', since no `R', `H'.
          "Adjust red, green, blue, hue, saturation, or value? [rgbhsv]: ")
         ;; Cannot use `facemenu-read-color' here, because we allow "#...".
         (completing-read "Color (name or #rrrrggggbbbb): " (hexrgb-defined-colors-alist))
         (doremi-read-increment-arg 3 1)))      
  (setq color  (hexrgb-color-name-to-hex color))
  (let ((hlen  (/ (1- (length color)) 3)) ; length of one hex color, R, G, or B
        result)
    (setq result
          (case component
            (?r (hexrgb-increment-red color hlen increment))
            (?g (hexrgb-increment-green color hlen increment))
            (?b (hexrgb-increment-blue color hlen increment))
            (otherwise
             ;; Convert RGB to HSV.  Convert range 0-65535 to range 0.0-1.0.
             (let* ((rgb         (x-color-values color))
                    (red         (/ (float (nth 0 rgb)) 65535.0))
                    (green       (/ (float (nth 1 rgb)) 65535.0))
                    (blue        (/ (float (nth 2 rgb)) 65535.0))
                    (hsv         (hexrgb-rgb-to-hsv red green blue))
                    (hue         (nth 0 hsv))
                    (saturation  (nth 1 hsv))
                    (value       (nth 2 hsv)))
               (case component
                 (?h (setq hue  (+ hue (/ increment 100.0)))
                     (when (> hue 1.0) (setq hue  (1- hue))))
                 (?v (setq value  (+ value (/ increment 100.0)))
                     (when (> value 1.0) (setq value  (1- value))))
                 (?s (setq saturation  (+ saturation (/ increment 100.0)))
                     (when (> saturation 1.0) (setq saturation  (1- saturation))))
                 ;; Default input COMPONENT is hue.
                 (otherwise (setq hue  (+ hue (/ increment 100.0)))
                            (when (> hue 1.0) (setq hue  (1- hue)))))
               (hexrgb-color-values-to-hex
                (mapcar (lambda (x) (floor (* x 65535.0)))
                        (hexrgb-hsv-to-rgb hue saturation value)))))))
    (when (interactive-p) (message result))
    result))
 
;;; Helper Functions for Face and Frame Color Commands

(defun doremi-read-component ()
  "Read a color-component character, one of [rgbhsvRH]."
  (read-char-exclusive
         "Adjust red, green, blue, hue, saturation, value, \
all RGB, or HSV? [rgbhsvRH]: "))

(defun doremi-read-increment-arg (&optional length default)
  "Read a Do Re Mi increment argument.
With a prefix argument, use its numerical value.
Otherwise prompt for user input, which must be a number or a list of
numbers.
LENGTH is the number of list elements to allow.  If nil, no limit.
DEFAULT is used if input is empty."
  (when (numberp default) (setq default  (number-to-string default)))
  (setq doremi-current-increment
        (or (and current-prefix-arg (prefix-numeric-value current-prefix-arg))
            (let ((input  (read-from-minibuffer
                           (format "Increment (# or Lisp list of %s%s#s): "
                                   (or length "") (if length " " ""))
                           nil nil 'read nil default))
                  (ok-p   t))
              (while (not (or (numberp input)
                              (and (consp input)
                                   (or (not length)
                                       (and (eq length (length input))
                                            (dotimes (ii (1- length) ok-p)
                                              (setq ok-p (numberp (elt input ii)))))))))
                (message "Not a number or a list of %s%snumbers - try again"
                         (or length "") (if length " " ""))
                (sit-for 1)
                (setq input  (read-from-minibuffer
                              (format "Increment (# or Lisp list of %s%s#s): "
                                      (or length "") (if length " " ""))
                              nil nil 'read nil default)))
              input))))

(defun doremi-increment-face-color-read-args ()
  "Read arguments for functions `doremi*-face-*'.
That is, for functions `doremi-face-bg+', `doremi-face-fg+',
`doremi-increment-face-bg-color', and
`doremi-increment-face-fg-color'.
The arguments read are the face to change, the color component to
increment, and, if no prefix argument, the increment (amount).
If a prefix arg was used, then its numerical value is used as the
increment."
  (list (if (< emacs-major-version 21)
            (read-face-name "Face to change: ")
          (read-face-name "Face to change"))
        (doremi-read-component) (doremi-read-increment-arg 3 1)))

(defun doremi-all-frames-bg/fg-1 (frame-parameter component increment)
  "Iteratively INCREMENT color FRAME-PARAMETER on all frames for COMPONENT.
This is a Do Re Mi loop: increment in response to user actions.
FRAME-PARAMETER can be `background-color' or `foreground-color'.
COMPONENT and INCREMENT are as for `doremi-increment-color'."
  (setq increment  (doremi-adjust-increment-for-color-component component increment))
  (let ((doremi-wrap-color-flag  nil))
    ;; DO RE MI, according to the INCREMENT type: number or list.
    (if (atom increment)
        (doremi (lambda (inc)
                  (dolist (fr  (visible-frame-list))
                    (doremi-increment-frame-color frame-parameter component inc fr))
                  "- N/A -")            ; Irrelevant
                "- N/A -"               ; Irrelevant
                increment
                t)
      (doremi (lambda (inc)
                (dolist (fr  (visible-frame-list))
                  (let ((comp  component))
                    (setq comp  (if (eq comp ?R) ?r ?h))
                    (doremi-increment-frame-color frame-parameter comp (car inc) fr)
                    (setq comp  (if (eq comp ?r) ?g ?s))
                    (doremi-increment-frame-color frame-parameter comp (cadr inc) fr)
                    (setq comp  (if (eq comp ?g) ?b ?v))
                    (doremi-increment-frame-color frame-parameter comp (caddr inc) fr)))
                "- N/A -")              ; Irrelevant
              "- N/A -"                 ; Irrelevant
              increment
              t))
    ;; Recurse with the NEXT-COMPONENT.  Revert increment to `doremi-current-increment'.
    (let ((next-component  (pop unread-command-events)))
      (when (member next-component '(?r ?g ?b ?h ?s ?v ?R ?H))
        (doremi-all-frames-bg/fg-1 frame-parameter next-component
                                   doremi-current-increment)))))

(defun doremi-bg-1 (component &optional increment frame)
  "Non-interactive version of `doremi-bg+'."
  (unless increment (setq increment  1))
  (doremi-frame-color-component 'background-color component increment frame))

(defun doremi-fg-1 (component &optional increment frame)
  "Non-interactive version of `doremi-fg+'."
  (unless increment (setq increment  1))
  (doremi-frame-color-component 'foreground-color component increment frame))

(defun doremi-frame-color-component (frame-parameter component increment &optional frame)
  "Iteratively INCREMENT color FRAME-PARAMETER of FRAME for COMPONENT.
This is a Do Re Mi loop: increment in response to user actions.
FRAME-PARAMETER can be `background-color' or `foreground-color'.
COMPONENT and INCREMENT are as for `doremi-increment-color'."
  (setq increment  (doremi-adjust-increment-for-color-component component increment))
  ;; DO RE MI, according to the INCREMENT type: number or list.
  (if (atom increment)
      (doremi (lambda (inc)
                (doremi-increment-frame-color frame-parameter component inc frame)
                (cdr (assq frame-parameter (frame-parameters frame))))
              (cdr (assq frame-parameter (frame-parameters frame)))
              increment
              t)
    (doremi (lambda (inc)
              (let ((comp  component))
                (setq comp  (if (eq comp ?R) ?r ?h))
                (doremi-increment-frame-color frame-parameter comp (car inc) frame)
                (setq comp  (if (eq comp ?r) ?g ?s))
                (doremi-increment-frame-color frame-parameter comp (cadr inc) frame)
                (setq comp  (if (eq comp ?g) ?b ?v))
                (doremi-increment-frame-color frame-parameter comp (caddr inc) frame)
                (cdr (assq frame-parameter (frame-parameters frame)))))
            (cdr (assq frame-parameter (frame-parameters frame)))
            increment
            t))
  ;; Recurse with the NEXT-COMPONENT.  Revert increment to `doremi-current-increment'.
  (let ((next-component  (pop unread-command-events)))
    (frame-update-face-colors frame)    ; Update the way faces display
    (when (member next-component '(?r ?g ?b ?h ?s ?v ?R ?H))
      (doremi-frame-color-component frame-parameter next-component
                                    doremi-current-increment frame))))

(defun doremi-adjust-increment-for-color-component (component increment)
  "Return INCREMENT, but adjusted for color COMPONENT.
For [rgbhsv], return a number.  For RH, return a list of 3 numbers.
For [Rrgb], scale the number(s) by `doremi-RGB-increment-factor'."
  (let ((new  increment))
    (if (atom new)
        (when (memq component '(?R ?H)) (setq new  (list new new new)))
      (case component
        ((?r ?h) (setq new  (car new)))
        ((?g ?s) (setq new  (cadr new)))
        ((?b ?v) (setq new  (caddr new)))))
    (when (memq component '(?r ?g ?b)) (setq new  (* new doremi-RGB-increment-factor)))
    (when (eq component ?R)
      (setq new  (mapcar (lambda (in) (* in doremi-RGB-increment-factor)) new)))
    new))
  
(defun doremi-increment-background-color-1 (component increment &optional frame)
  "Non-interactive version of `doremi-increment-background-color'."
  (doremi-increment-frame-color 'background-color component increment frame))

(defun doremi-increment-foreground-color-1 (component increment &optional frame)
  "Non-interactive version of `doremi-increment-foreground-color'."
  (doremi-increment-frame-color 'foreground-color component increment frame))

(defun doremi-increment-frame-color (frame-parameter component increment &optional frame)
  "Change color FRAME-PARAMETER by INCREMENT of color COMPONENT.
FRAME-PARAMETER can be `background-color' or `foreground-color'.
COMPONENT and INCREMENT are as for `doremi-increment-color'.
Optional arg FRAME defaults to the selected frame.  See `doremi-bg+'."
  (doremi-increment-color component increment
                          (cdr (assq frame-parameter (frame-parameters frame)))
                          (if (eq frame-parameter 'background-color)
                              'doremi-set-background-color
                            'doremi-set-foreground-color)
                          (or frame (selected-frame)))
  ;; $$$$$$ (frame-update-face-colors frame)    ; Update the way faces display
  (cdr (assq frame-parameter (frame-parameters frame)))) ; Return new value.

(defun doremi-frame-hue-stepping-saturation (frame-parameter increment &optional frame)
  "Increment frame hue for FRAME-PARAMETER, stepping saturation per cycle.
See `doremi-bg+' for info about the other args."
  (doremi (lambda (inc)
            (let ((hue  (hexrgb-hue (or (cdr (assq frame-parameter
                                                   (frame-parameters frame)))
                                        (if (eq frame-parameter 'background-color)
                                            "White"
                                          "Black")))))
              (when (or (> hue 0.99) (< hue 0.01))
                (cond ((> hue 0.9999) (setq hue  0.0))
                      ((< hue 0.0001) (setq hue  1.0)))
                (doremi-increment-frame-color frame-parameter ?s (- inc) frame)
                (doremi-increment-frame-color frame-parameter ?h inc frame)))
            (doremi-increment-frame-color frame-parameter ?h inc frame)
            (cdr (assq frame-parameter (frame-parameters frame))))
          (cdr (assq frame-parameter (frame-parameters frame)))
          increment
          t))

(defun doremi-bg/fg-color-name-1 (frame-parameter frame init-color)
  "Helper function for `doremi-bg-color-name+' and `doremi-fg-color-name+'."
  (let ((set-fn  (if (eq frame-parameter 'background-color)
                     #'set-background-color
                   #'set-foreground-color)))
    (doremi (lambda (newval) (funcall set-fn newval) newval)
            init-color
            nil                         ; ignored
            nil                         ; ignored
            (hexrgb-defined-colors)     ; Enumeration list
            t)
    (frame-update-face-colors frame)))

(defun doremi-increment-face-color (frame-parameter face component increment)
  "Change color FRAME-PARAMETER of FACE by INCREMENT of color COMPONENT.
FRAME-PARAMETER can be `background-color' or `foreground-color'.
COMPONENT and INCREMENT are as for `doremi-increment-color'.
The color is changed on all frames."
  (doremi-increment-color component increment
                          (doremi-face-default frame-parameter face)
                          (if (eq frame-parameter 'foreground-color)
                              #'set-face-foreground
                            #'set-face-background)
                          nil
                          face)
  (doremi-face-default frame-parameter face)) ; Return new value.

(defun doremi-face-default (frame-parameter face)
  "Return a default FACE value for FRAME-PARAMETER.
The FACE's current FRAME-PARAMETER value or, if none, the frame's."
  (or (if (eq frame-parameter 'foreground-color)
          (face-foreground-20+ face nil 'default)
        (face-background-20+ face nil 'default))
      (cdr (assq frame-parameter (frame-parameters)))))

(defun doremi-face-hue-stepping-saturation (frame-parameter face increment)
  "Increment FACE hue for FRAME-PARAMETER, stepping saturation per cycle.
See `doremi-bg+' for info about the other args."
  (doremi (lambda (inc)
            (let ((hue  (hexrgb-hue (or (doremi-face-default frame-parameter face)
                                        (if (eq frame-parameter 'background-color)
                                            "White"
                                          "Black")))))
              (when (or (> hue (- 1.0 (/ inc 100.0))) (< hue (/ inc 100.0)))
                (cond ((> hue (- 1.0 (/ inc 1000.0))) (setq hue  0.0))
                      ((< hue (/ inc 1000.0)) (setq hue  1.0)))
                (doremi-increment-face-color frame-parameter face ?s (- inc))))
            (doremi-increment-face-color frame-parameter face ?h inc)
            (doremi-face-default frame-parameter face))
          (doremi-face-default frame-parameter face)
          increment
          t))

(defun doremi-all-faces-bg/fg-1 (frame-parameter component increment)
  "Iteratively INCREMENT color FRAME-PARAMETER of all faces for COMPONENT.
This is a Do Re Mi loop: increment in response to user actions.
FRAME-PARAMETER can be `background-color' or `foreground-color'.
COMPONENT and INCREMENT are as for `doremi-increment-color'."
  (setq increment  (doremi-adjust-increment-for-color-component component increment))
  (let ((doremi-wrap-color-flag  nil)
        (fn                      (if (eq frame-parameter 'background-color)
                                     'doremi-increment-face-bg-color
                                   'doremi-increment-face-fg-color)))
    (if (atom increment)
        (doremi (lambda (inc)
                  (dolist (face  (face-list)) (funcall fn face component inc))
                  "- N/A -")            ; Irrelevant
                "- N/A -"               ; Irrelevant
                increment
                t)
      (doremi (lambda (inc)
                (dolist (face  (face-list))
                  (let ((comp  component))
                    (setq comp  (if (eq comp ?R) ?r ?h))
                    (funcall fn face comp (car inc))
                    (setq comp  (if (eq comp ?r) ?g ?s))
                    (funcall fn face comp (cadr inc))
                    (setq comp  (if (eq comp ?g) ?b ?v))
                    (funcall fn face comp (caddr inc))))
                "- N/A -")              ; Irrelevant
              "- N/A -"                 ; Irrelevant
              increment
              t))
    ;; Recurse with the NEXT-COMPONENT.  Revert increment to `doremi-current-increment'.
    (let ((next-component  (pop unread-command-events)))
      (when (member next-component '(?r ?g ?b ?h ?s ?v ?R ?H))
        (doremi-all-faces-bg/fg-1 frame-parameter next-component
                                  doremi-current-increment)))))

(defun doremi-face-bg/fg-1 (frame-parameter face component &optional increment)
  "Iteratively INCREMENT color FRAME-PARAMETER of FACE for COMPONENT.
This is a Do Re Mi loop: increment in response to user actions.
FRAME-PARAMETER can be `background-color' or `foreground-color'.
COMPONENT and INCREMENT are as for `doremi-increment-color'."
  (unless increment (setq increment  1))
  (unwind-protect
       (progn
         (let* ((special-display-regexps         nil)
                (after-make-frame-functions      nil)
                (fit-frame-inhibit-fitting-flag  t)
                (sample-text
                 (format "\n    Sample text in face `%s'\n" face))
                (pop-up-frame-alist
                 (append '((name . "*Face Sample*") (height . 5) (auto-raise . t)
                           (minibuffer) (tool-bar-lines . 0) (menu-bar-lines . 0)
                           (vertical-scroll-bars))
                         `((width ,@ (+ 4 (length sample-text))))
                         (frame-parameters))))
           (with-temp-buffer
             (get-buffer-create "*Face Sample*")
             (pop-to-buffer "*Face Sample*")
             (insert sample-text)
             (goto-char 2)
             (put-text-property 6 (progn (goto-char (point-min)) (forward-line 2) (point))
                                'face face)
             (save-excursion (insert (format "    Previous value of `%s'" face)))
             (put-text-property (point) (save-excursion (forward-line 1) (point))
                                'face 'doremi-last-face)
             (goto-char (point-min))
             (setq buffer-read-only  t)
             (doremi-face-color-component frame-parameter face component increment)
             (if (one-window-p t) (delete-frame) (delete-window))))
         (when (get-buffer "*Face Sample*") (kill-buffer "*Face Sample*"))
         (if (eq frame-parameter 'foreground-color)
             (let ((new-fg  (face-foreground-20+ face nil 'default)))
               (set-face-foreground face new-fg)
               (put face 'customized-face (list (list 't (list :foreground new-fg)))))
           (let ((new-bg  (face-background-20+ face nil 'default)))
             (set-face-background face new-bg)
             (put face 'customized-face (list (list 't (list :background new-bg))))))
         (put face 'face-modified nil)
         (message (substitute-command-keys
                   "Use `\\[doremi-undo-last-face-change]' to return to previous face \
value. Use `\\[customize-face]' to revisit changes.")))
    (when (get-buffer "*Face Sample*") (kill-buffer "*Face Sample*"))
    (let ((fr  (get-a-frame "*Face Sample*"))) (when fr (delete-frame fr)))))

(defun doremi-face-color-component (frame-parameter face component increment)
  "Iteratively INCREMENT color FRAME-PARAMETER of FACE for COMPONENT.
The color is changed on all frames.
This is a Do Re Mi loop: increment in response to user actions.
FRAME-PARAMETER can be `background-color' or `foreground-color'.
COMPONENT and INCREMENT are as for `doremi-increment-color'.
INCREMENT is scaled here, for RGB, by `doremi-RGB-increment-factor'."
  (setq increment  (doremi-adjust-increment-for-color-component component increment))
  ;; DO RE MI, according to the INCREMENT type: number or list.
  (if (atom increment)
      (doremi (lambda (inc)
                (doremi-increment-face-color frame-parameter face component inc)
                (doremi-face-default frame-parameter face))
              (doremi-face-default frame-parameter face)
              increment
              t)
    (doremi (lambda (inc)
              (let ((comp  component))
                (setq comp  (if (eq comp ?R) ?r ?h))
                (doremi-increment-face-color frame-parameter face comp (car inc))
                (setq comp  (if (eq comp ?r) ?g ?s))
                (doremi-increment-face-color frame-parameter face comp (cadr inc))
                (setq comp  (if (eq comp ?g) ?b ?v))
                (doremi-increment-face-color frame-parameter face comp (caddr inc)))
              (doremi-face-default frame-parameter face))
            (doremi-face-default frame-parameter face)
            increment
            t))
  ;; Recurse with the NEXT-COMPONENT.  Revert increment to `doremi-current-increment'.
  (let ((next-component  (pop unread-command-events)))
    (when (member next-component '(?r ?g ?b ?h ?s ?v ?R ?H))
      (doremi-face-color-component frame-parameter face next-component
                                   doremi-current-increment))))

(defun doremi-increment-color (component increment color set-fn
                               &optional frame &rest args)
  "Increment COLOR by INCREMENT of COMPONENT and use for FRAME.
COMPONENT is as for `doremi-bg+' (which see).
For HSV components, INCREMENT is limited here to range -100 to 100.
COLOR is the color to change.
SET-FN is the function used to set the new FRAME parameter value.
  It can set the background or foreground color.
  It must accept FRAME as its final argument.
Optional arg FRAME is passed to SET-FN as its last argument.  It does
  *not* default to the selected frame, because for some SET-FNs, such
  as `set-face-foreground', nil means all frames.
ARGS are additional arguments for SET-FN, which appear before the
  color in the calling sequence.  For example, if SET-FN is
  `set-face-foreground', ARGS can be a list containing the face whose
  foreground is to be set."
  (unless (string-match "#" color)      ; Convert color name to #hhh...
    (setq color  (hexrgb-color-values-to-hex (x-color-values color))))
  (setq increment  (case component
                     ((?R ?H) (if (consp increment)
                                  increment
                                (list increment increment increment)))
                     ((?r ?h) (if (consp increment) (car increment) increment))
                     ((?g ?s) (if (consp increment) (cadr increment) increment))
                     ((?b ?v) (if (consp increment) (caddr increment) increment))))
  (let ((hlen  (/ (1- (length color)) 3))) ; length of one hex color, R, G, or B
    (case component
      (?R (doremi-increment-red   hlen increment color set-fn frame args)
          (doremi-increment-green hlen increment color set-fn frame args)
          (doremi-increment-blue  hlen increment color set-fn frame args))
      (?r (doremi-increment-red   hlen increment color set-fn frame args))
      (?g (doremi-increment-green hlen increment color set-fn frame args))
      (?b (doremi-increment-blue  hlen increment color set-fn frame args))
      (otherwise                        ; HSV
       (setq increment  (if (atom increment)
                            (max (min increment 100) -100)
                          (list (max (min (car increment)   100) -100)
                                (max (min (cadr increment)  100) -100)
                                (max (min (caddr increment) 100) -100))))

       ;; Convert RGB to HSV.  Convert range 0-65535 to range 0.0-1.0.
       (let* ((rgb         (x-color-values color))
              (red         (/ (float (nth 0 rgb)) 65535.0))
              (green       (/ (float (nth 1 rgb)) 65535.0))
              (blue        (/ (float (nth 2 rgb)) 65535.0))
              (hsv         (hexrgb-rgb-to-hsv red green blue))
              (hue         (nth 0 hsv))
              (saturation  (nth 1 hsv))
              (value       (nth 2 hsv)))
         (case component
           (?H (setq hue
                     (doremi-wrap-or-limit-color-component
                      (+ hue (/ (if (atom increment) increment (car increment)) 100.0)))

                     saturation
                     (doremi-wrap-or-limit-color-component
                      (+ saturation (/ (if (atom increment) increment (cadr increment))
                                       100.0)))

                     value
                     (doremi-wrap-or-limit-color-component
                      (+ value (/ (if (atom increment) increment (caddr increment))
                                  100.0)))))
           (?h (setq hue  (doremi-wrap-or-limit-color-component ; Default is HUE.
                           (+ hue (/ (if (atom increment) increment (car increment))
                                     100.0)))))
           (?s (setq saturation  (doremi-wrap-or-limit-color-component
                                  (+ saturation (/ (if (atom increment)
                                                       increment
                                                     (cadr increment))
                                                   100.0)))))
           (?v (setq value  (doremi-wrap-or-limit-color-component
                             (+ value (/ (if (atom increment) increment (caddr increment))
                                         100.0)))))
           (t  (setq hue  (doremi-wrap-or-limit-color-component ; Default is HUE.
                           (+ hue (/ (if (atom increment) increment (car increment))
                                     100.0))))))
         (apply set-fn (append args
                               (list (hexrgb-color-values-to-hex
                                      (mapcar (lambda (x) (floor (* x 65535.0)))
                                              (hexrgb-hsv-to-rgb hue saturation value))))
                               (list frame))))))))

(defun doremi-increment-red (hlen increment color set-fn frame args)
  "Increment the red component of COLOR using SET-FN, for FRAME.
HLEN is the number of hex digits for the component.
See `doremi-increment-color' for the other args."
  (apply set-fn (append args (list (hexrgb-increment-red
                                    color hlen
                                    (if (atom increment) increment (car increment))
                                    doremi-wrap-color-flag))
                        (list frame))))

(defun doremi-increment-green (hlen increment color set-fn frame args)
  "Increment the green component of COLOR using SET-FN, for FRAME.
HLEN is the number of hex digits for the component.
See `doremi-increment-color' for the other args."
  (apply set-fn (append args (list (hexrgb-increment-green
                                    color hlen
                                    (if (atom increment) increment (cadr increment))
                                    doremi-wrap-color-flag))
                        (list frame))))

(defun doremi-increment-blue (hlen increment color set-fn frame args)
  "Increment the blue component of COLOR using SET-FN, for FRAME.
HLEN is the number of hex digits for the component.
See `doremi-increment-color' for the other args."
  (apply set-fn (append args (list (hexrgb-increment-blue
                                    color hlen
                                    (if (atom increment) increment (caddr increment))
                                    doremi-wrap-color-flag))
                        (list frame))))

(defun doremi-wrap-or-limit-color-component (value)
  "Limit color component VALUE between 0.0 and 1.0.
Wrap around if `doremi-wrap-color-flag'."
  (if doremi-wrap-color-flag
      (doremi-wrap value  0.0  1.0)
    (doremi-limit value  0.0  1.0)))

(defun doremi-face-bg/fg-color-name-1 (frame-parameter face)
  "Helper for `doremi-face-bg-color-name+', `doremi-face-fg-color-name+'."
  (unless (facep face)
    (error "Command `doremi-face-bg-color-name+': FACE arg is not a face name: %s" face))
  (let ((set-fn     (if (eq frame-parameter 'background-color)
                        #'set-face-background
                      #'set-face-foreground))
        (access-fn  (if (eq frame-parameter 'background-color)
                        #'face-background-20+
                      #'face-foreground-20+))
        (attribute  (if (eq frame-parameter 'background-color) :background :foreground)))
    (unwind-protect
         (progn
           (let* ((special-display-regexps         nil)
                  (after-make-frame-functions      nil)
                  (fit-frame-inhibit-fitting-flag  t)
                  (sample-text
                   (format "\n    Sample text in face `%s'\n" face))
                  (pop-up-frame-alist
                   (append '((name . "*Face Sample*") (height . 5) (auto-raise . t)
                             (minibuffer) (tool-bar-lines . 0) (menu-bar-lines . 0)
                             (vertical-scroll-bars))
                           `((width ,@ (+ 4 (length sample-text))))
                           (frame-parameters))))
             (copy-face face 'doremi-last-face)
             (setq doremi-last-face-value  (cons face 'doremi-last-face))
             (with-temp-buffer
               (get-buffer-create "*Face Sample*")
               (pop-to-buffer "*Face Sample*")
               (insert sample-text)
               (goto-char 2)
               (put-text-property
                6 (progn (goto-char (point-min)) (forward-line 2) (point)) 'face face)
               (save-excursion (insert (format "    Previous value of `%s'" face)))
               (put-text-property (point) (save-excursion (forward-line 1) (point))
                                  'face 'doremi-last-face)
               (goto-char (point-min))
               (setq buffer-read-only  t)
               (doremi (lambda (newval) (funcall set-fn face newval) newval)
                       (or (funcall access-fn face nil 'default)
                           (cdr (assq frame-parameter (frame-parameters))))
                       nil              ; ignored
                       nil              ; ignored
                       (hexrgb-defined-colors) ; Enumeration list
                       t)
               (if (one-window-p t) (delete-frame) (delete-window))))
           (let ((new-color  (funcall access-fn face nil 'default)))
             (if (fboundp 'set-face-attribute)
                 (set-face-attribute face nil attribute new-color)
               (modify-face face nil new-color nil nil nil nil))
             (put face 'customized-face
                  (list (list 't (list attribute new-color)))))
           (put face 'face-modified nil)
           (message (substitute-command-keys
                     "Use `\\[doremi-undo-last-face-change]' to return to previous face \
value. Use `\\[customize-face]' to revisit changes.")))
      (when (get-buffer "*Face Sample*") (kill-buffer "*Face Sample*"))
      (let ((fr  (get-a-frame "*Face Sample*"))) (when fr (delete-frame fr))))))

;; A function like this should be available as part of the Customize
;; code, but there is none.  
;; This is OK for Emacs 22, but won't work for Emacs 20, because of `set-face-attribute'.
;; We don't bother to use this now.
(when (fboundp 'set-face-attribute)
  (defun doremi-face-set (face spec)
    "Tell Customize that FACE has been set to value SPEC.
SPEC is as for `defface'."
    (let ((attrs  (face-spec-choose spec)))
      (while attrs
        (let ((attribute  (car attrs))
              (value      (cadr attrs)))
          (when attribute 
            (set-face-attribute face nil attribute value)))
        (setq attrs  (cddr attrs))))
    (put face 'customized-face spec)
    (message (substitute-command-keys
              "Use `\\[customize-face]' to revisit changes."))))

(defun doremi-set-frame-color (frame-parameter color-name &optional frame)
  "Set the color FRAME-PARAMETER of FRAME to COLOR-NAME.
FRAME-PARAMETER can be `background-color' or `foreground-color'."
  (modify-frame-parameters (or frame (selected-frame))
                           (list (cons frame-parameter color-name)))
  (frame-update-face-colors (or frame (selected-frame))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Apply `doremi-push-frame-config-for-command' to all commands that change
;; frame configuration.  Only do this if `doremi.el' is loaded, so
;; can use `doremi-frame-configs+'.
(when (and doremi-push-frame-config-for-cmds-flag (featurep 'doremi))
  (mapcar 'doremi-push-frame-config-for-command
          '(doremi-bg+ doremi-bg-blue+ doremi-bg-brightness+ doremi-bg-color-name+
            doremi-bg-cyan+ doremi-bg-green+ doremi-bg-hue+ doremi-bg-magenta+
            doremi-bg-red+ doremi-bg-saturation+ doremi-bg-value+ doremi-bg-yellow+
            doremi-buffer-font-size+ doremi-face-bg+ doremi-face-bg-color-name+
            doremi-face-fg+ doremi-face-fg-color-name+ doremi-fg+ doremi-fg-blue+
            doremi-fg-brightness+ doremi-fg-color-name+ doremi-fg-cyan+
            doremi-fg-green+ doremi-fg-hue+ doremi-fg-magenta+ doremi-fg-red+
            doremi-fg-saturation+ doremi-fg-value+ doremi-fg-yellow+ doremi-font+
            doremi-frame-font-size+ doremi-frame-height+ doremi-frame-width+
            doremi-increment-background-color doremi-increment-face-bg-color
            doremi-increment-face-fg-color enlarge-font
            doremi-frame-horizontally+ doremi-frame-vertically+
            doremi-undo-last-face-change doremi-undo-last-frame-color-change)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'doremi-frm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; doremi-frm.el ends here
