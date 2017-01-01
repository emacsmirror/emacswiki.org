;;; thumb-frm.el --- Commands for thumbnail frames.
;;
;; Filename: thumb-frm.el
;; Description: Commands for thumbnail frames.
;; Author: Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 2004-2017, Drew Adams, all rights reserved.
;; Created: Fri Dec 10 16:44:55 2004
;; Version: 0
;; Package-Requires: ((frame-fns "0") (frame-cmds "0"))
;; Last-Updated: Sun Jan  1 11:47:41 2017 (-0800)
;;           By: dradams
;;     Update #: 1793
;; URL: http://www.emacswiki.org/thumb-frm.el
;; Doc URL: http://www.emacswiki.org/FisheyeWithThumbs
;; Keywords: frame, icon
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x, 24.x, 25.x
;;
;; Features that might be required by this library:
;;
;;   `avoid', `frame-cmds', `frame-fns', `misc-fns', `strings',
;;   `thingatpt', `thingatpt+'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Shrink frames to a thumbnail size and restore them again.
;;
;;  User option `thumfr-frame-parameters' defines the appearance of
;;  the thumbnail frames.  If you change it and set the new definition
;;  for the current session you can immediately see the effect in all
;;  of your thumbnail frames.
;;
;;  By default, `thumfr-frame-parameters' gives you thumbnail frames
;;  that:
;;
;;   * have no menu bar or tool bar
;;   * have a 6x6-pixel scroll bar
;;   * are 30% transparent when not selected (opaque when selected)
;;
;;  Commands `thumfr-toggle-thumbnail-frame', `thumfr-thumbify-frame',
;;  and `thumfr-dethumbify-frame' thumbify and dethumbify an
;;  individual frame.  If option `thumfr-thumbify-dont-iconify-flag'
;;  is non-nil (the default), then keys (e.g. `C-z') that normally
;;  iconify and deiconify will instead thumbify and dethumbify.
;;
;;  Command `thumfr-thumbify-other-frames', alias `thumfr-fisheye',
;;  shrinks all frames except the selected frame to a thumbnail size.
;;  The thumbnail frames are stacked from top to bottom, left to right
;;  on your display.  This provides a kind of "fisheye" view of the
;;  frames you are using.  Command `thumfr-dethumbify-all-frames'
;;  restores all thumbnail frames to full size.
;;
;;  Command `thumfr-stack-thumbnail-frames' neatly stacks all of the
;;  thumbnail frames along a display edge.  You can use it at any
;;  time, and it is called automatically by `thumfr-fisheye'.  Which
;;  display edge to stack along (left, right, top, or bottom), and
;;  which direction (up, down, to-left or to-right), is determined by
;;  option `thumfr-stack-display-edge'.  The stacking order is
;;  determined by option `thumfr-sort-function'.  You can turn sorting
;;  on and off using command
;;  `thumfr-toggle-sort-thumbnail-frame-stack'.
;;
;;  By default, this library provides thumbifying and dethumbifying as
;;  a *replacement* for iconifying and deiconifying.  Whenever option
;;  `thumfr-thumbify-dont-iconify-flag' is non-nil, loading the
;;  library changes the standard commands `iconify-frame' and
;;  `iconify-or-deiconify-frame' so that they use thumbnail frames
;;  instead of desktop icons.  To prevent this thumbnail behavior, you
;;  can customize `thumfr-thumbify-dont-iconify-flag' to nil.
;;  Alternatively, you can deactivate (`ad-deactivate') the advice
;;  imposed here on these functions, to restore their original
;;  behavior.
;;
;;  The original behavior of commands `iconify-frame' and
;;  `iconify-or-deiconify-frame' is available using commands
;;  `thumfr-really-iconify-frame' and
;;  `thumfr-really-iconify-or-deiconify-frame'.  In particular, these
;;  commands can be used to iconify even if you bind [iconify-frame]
;;  in `special-event-map' (see below).
;;
;;  You can iconify or deiconify all thumbnail frames (that is, only
;;  the thumbnail frames), to get them out of the way and bring them
;;  back.  Use commands `thumfr-iconify-thumbnail-frames' and
;;  `thumfr-deiconify-thumbnail-frames' to do this.
;;
;;  Emacs built-in function `raise-frame' is redefined here to also
;;  dethumbify.  The original behavior of `raise-frame' is available
;;  in command `thumfr-only-raise-frame'.
;;
;;  You can cycle among the visible frames in two ways, applying
;;  `thumfr-fisheye' to each in turn.  The first way is using commands
;;  `thumfr-fisheye-previous-frame' and `thumfr-fisheye-next-frame'
;;  (which you can bind to, for instance, `C-M-prior' and `C-m-next').
;;  The second way is using command `thumfr-doremi-thumbnail-frames+'
;;  plus the arrow keys or mouse wheel.
;;
;;  To be able to use `thumfr-doremi-thumbnail-frames+', you need
;;  library `doremi-frm.el' (which in turn requires libraries
;;  `hexrgb.el', `faces+.el', `doremi.el', and perhaps `ring+.el',
;;  depending on your Emacs version).  The only libraries strictly
;;  required by `thumb-frm.el' are `frame-fns.el' and `frame-cmds.el'.
;;
;;  A more comprehensive, lower-level way of substituting thumbifying
;;  for iconifying is to do the following in your init file:
;;
;;    (define-key special-event-map [iconify-frame]
;;                'thumfr-thumbify-frame-upon-event)
;;
;;  In effect, this thumbifies any frame as soon as it is iconified,
;;  no matter how it was iconified.  In particular, this will let you
;;  use the window-manager "minimize" frame button (usually at the
;;  upper left or right frame corner) to thumbify.
;;
;;  Be aware of this fact/feature, which is true of any binding on
;;  keymap `special-event-map': The event interrupts any key sequence
;;  in progress, to invoke its command, and then the key sequence as a
;;  whole is processed, ignoring the special event.
;;
;;  For example, assuming that event `iconify-frame' occurs whenever
;;  you use a window-manager button (e.g. in the title bar) or menu
;;  item to minimize (iconify) a frame, if you do `C-x
;;  <click-the-minimize-button> b', then: (1) as soon as you click the
;;  minimize button the frame is thumbified, and (2) when you hit `b'
;;  the key sequence `C-x b' is processed (e.g. `switch-to-buffer'
;;  prompts you for a buffer name).  I mention this not because it is
;;  a useful feature but in order to avoid confusion.
;;
;;  If you do bind `thumfr-thumbify-frame-upon-event' to
;;  `iconify-frame', be aware that `thumfr-thumbify-dont-iconify-flag'
;;  will no longer have any effect: Emacs will *always* thumbify
;;  instead of iconify (except for functions `really-iconify-*frame',
;;  which are designed to counter this).  If you try this behavior and
;;  then wish to cancel it, to once again allow iconification, use
;;  this code:
;;
;;  In Emacs 20 or prior:
;;    (define-key special-event-map [iconify-frame] 'ignore-event)
;;
;;  In Emacs 21 or later:
;;    (define-key special-event-map [iconify-frame] 'ignore)
;;
;;
;;  Other user options (variables) not mentioned above are these:
;;
;;    `thumfr-font-difference'             - Zoom of thumbnail frames.
;;    `thumfr-rename-when-thumbify-flag'   - Rename frame to buffer.
;;    `thumfr-stack-display-edge'         - Display edge for stacking.
;;    `window-mgr-title-bar-pixel-width'   - Thickness of title bar.
;;
;;
;;  IMPORTANT:
;;
;;    Thumbnail frames are *FULLY FUNCTIONAL*.  In particular, their
;;    buffers are *NOT* read-only in any way.  You can edit their
;;    buffers normally, even if you can't see what you're doing :-).
;;
;;    You can also scroll and search their buffers.  You can thumbify
;;    the frame of a progressive output buffer, to monitor the output
;;    from 4,000 meters as it is produced.  In other words, you can
;;    interactive with thumbnail frames in the usual ways.  They are
;;    not special; they are just small.
;;
;;  Functions defined here:
;;
;;    `set-frame-parameter' (Emacs < 22),
;;    `thumfr-culled-thumbnail-frames',
;;    `thumfr-deiconify-thumbnail-frames', `thumfr-delete-if-not',
;;    `thumfr-dethumbify-all-frames', `thumfr-dethumbify-frame',
;;    `thumfr-doremi-thumbnail-frames+', `thumfr-fisheye',
;;    `thumfr-fisheye-next-frame', `thumfr-fisheye-previous-frame',
;;    `thumfr--frame-parameters-:set-function',
;;    `thumfr-iconify-thumbnail-frames', `thumfr-only-raise-frame',
;;    `thumfr-nset-difference', `thumfr-next-stack-position',
;;    `thumfr-really-iconify-frame',
;;    `thumfr-really-iconify-or-deiconify-frame', `thumfr-remove-if',
;;    `thumfr-remove-if-not', `thumfr-set-difference',
;;    `thumfr-stack-thumbnail-frames', `thumfr-sort-by-name',
;;    `thumfr-sort-by-window-id', `thumfr-thumbify-frame',
;;    `thumfr-thumbnail-frame-p', `thumfr-thumbnail-frames',
;;    `thumfr-thumbify-frame-upon-event',
;;    `thumfr-thumbify-other-frames', `thumfr-thumfr-parameter-p',
;;    `thumfr-toggle-sort-thumbnail-frame-stack',
;;    `thumfr-toggle-thumbnail-frame'.
;;
;;
;;  User options (variables) defined here:
;;
;;    `thumfr-font-difference', `thumfr-thumbify-dont-iconify-flag',
;;    `thumfr-frame-parameters', `thumfr-rename-when-thumbify-flag',
;;    `thumfr-sort-function', `thumfr-stack-display-edge',
;;    `window-mgr-title-bar-pixel-width'.
;;
;;
;;  Internal variables defined here:
;;
;;    `thumfr-last-row-show', `thumfr-last-sort-function',
;;    `thumfr-next-stack-xoffset', `thumfr-next-stack-yoffset'.
;;
;;
;;  ***** NOTE: These EMACS functions have been ADVISED or REDEFINED:
;;
;;  `iconify-frame' - Thumbify if `thumfr-thumbify-dont-iconify-flag'.
;;  `iconify-or-deiconify-frame' - Similar to `iconify-frame', plus
;;                                 dethumbify if already a thumbnail.
;;  `raise-frame' - Dethumbify also, if a thumbnail.
;;
;;
;;  Put this in your init file (`~/.emacs'): (require 'thumb-frm)
;;
;;  Suggested key bindings:
;;
;;   (global-set-key [(shift mouse-3)]
;;                   'thumfr-toggle-thumbnail-frame)
;;   (global-set-key [(shift control mouse-3)]
;;                   'thumfr-thumbify-other-frames)
;;   (global-set-key [(shift control ?z)]
;;                   'thumfr-thumbify-other-frames)
;;   (global-set-key [(shift control ?p)]
;;                   'thumfr-fisheye-previous-frame)
;;   (global-set-key [(shift control ?n)]
;;                   'thumfr-fisheye-next-frame)
;;   (global-set-key [(control meta ?z)]
;;                   'thumfr-really-iconify-or-deiconify-frame)
;;
;;   ;; Make the window-manager "minimize" button thumbify instead.
;;   (define-key special-event-map [iconify-frame]
;;               'thumfr-thumbify-frame-upon-event)
;;
;;   ;; Add `thumfr-doremi-thumbnail-frames+' to the Do Re Mi commands
;;   ;; - see library `doremi-frm.el'.
;;   (unless (fboundp 'doremi-prefix)
;;     (defalias 'doremi-prefix (make-sparse-keymap))
;;     (defvar doremi-map (symbol-function 'doremi-prefix)
;;       "Keymap for Do Re Mi commands."))
;;   (define-key global-map "\C-xt"  'doremi-prefix)
;;   (define-key global-map "\C-xte"
;;               'thumfr-doremi-thumbnail-frames+) ; "Eye"
;;
;;   Keep in mind also that if `thumfr-thumbify-dont-iconify-flag' is
;;   non-nil then keys bound to (de-)iconifying commands, such as
;;   `C-z', will instead (de)thumbify.  (This is not true, however, if
;;   `thumfr-thumbify-frame-upon-event' is bound to `iconify-frame'.
;;   Such a binding causes `thumfr-thumbify-dont-iconify-flag' not to
;;   have any effect - Emacs *always* thumbifies instead of
;;   iconifying, except for commands like `really-iconify-*frame'.)
;;
;;
;;  See also these libraries for other frame commands:
;;
;;     `autofit-frame.el' - Automatically fit each frame to its
;;                          selected window.  Uses `fit-frame.el'.
;;
;;     `fit-frame.el'     - 1) Fit a frame to its selected window.
;;                          2) Incrementally resize a frame.
;;
;;     `doremi-frm.el'    - Incrementally adjust frame properties
;;                          using arrow keys and/or mouse wheel.
;;
;;     `frame-cmds.el'    - Miscellaneous frame and window commands.
;;
;;     `zoom-frm.el'      - Zoom a frame, so that its font becomes
;;                          larger or smaller.
;;
;;  Acknowledgements (thanks):
;;    Michael Cadilhac [Michael.Cadilhac-@t-lrde.epita.fr] for a tip
;;      on using `discard-input' to effectively nullify
;;      `special-event-map' bindings (used in
;;      `really-iconify-[or-deiconify]frame').
;;
;;  TO DO?:
;;
;;     Make thumbnail frames read-only, to prevent inadvertent
;;     changes.  How to do so? Could make all buffers in frame's
;;     buffer-list r-o, but that would affect the buffer on
;;     non-thumbnail frames too.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2015/09/13 dadams
;;     thumfr-frame-parameters: Added alpha parameter to default value.
;;     thumfr-thumbify-frame: Added (alpha . 100) to NON-TF-PARAMS, if absent from FRAME.
;; 2014/11/17 dadams
;;     Added: thumfr--frame-parameters-:set-function.
;;     thumfr-frame-parameters: Use thumfr--frame-parameters-:set-function.  Need for Emacs 20.
;; 2014/08/27 dadams
;;     thumfr-frame-parameters: Added scroll-bar-height.
;; 2013/08/09 dadams
;;     Added: thumfr-thumfr-parameter-p.
;;     *-(de)thumbify-frame: Exclude thumfr-thumfr-parameter-p.
;; 2013/08-07 dadams
;;     Rewrite: Store info in frame parameters instead of vars thumfr(-non)-thumbnail-frames.
;;       Removed: thumfr(-non)-thumbnail-frames (vars), thumfr-cull-thumbnail-frames.
;;       *-frame-parameters, *-(de)thumbify-frame, *-thumbnail-frame-p,
;;         *-toggle-thumbnail-frame, *-thumbify-other-frames,
;;         *-(stack|(de)iconify)-thumbnail-frames, thumfr-sort-by-(name|window-id):
;;           Use frames & frame parameters thumfr(-non)-thumbnail-frame, not alist frame specs.
;;       Added: thumfr-thumbnail-frames (function), thumfr-remove-if(-not),
;;              thumfr-culled-thumbnail-frames, set-frame-parameter (Emacs < 22).
;;       thumfr-dethumbify-all-frames: use thumfr-culled-thumbnail-frames, not *-cull-*.
;; 2013/05/15 dadams
;;     thumfr-thumbify-frame: Handle font-too-small signal: try smaller thumfr-font-difference.
;; 2013/04/21 dadams
;;     thumfr-dethumbify-all-frames: Added prefix arg behavior (arg ICONIFIED-ALSO-P).
;;     thumfr-cull-thumbnail-frames: Added optional arg KEEP-ICONIFIED-P.
;;     thumfr-deiconify-thumbnail-frames: Pass arg to thumfr-cull-thumbnail-frames (bug fix).
;; 2013/04/17 dadams
;;     thumfr-frame-parameters: Added :set.  Added (scroll-bar-width . 6).
;;     Moved defvars before defcustoms (thumfr-frame-parameters uses thumfr-thumbnail-frames).
;;     Removed defadvice for *-mode (just use :set for thumfr-frame-parameters).
;; 2012/03/02 dadams
;;     Added thumfr-delete-if-not, thumfr-(n)set-difference: to avoid runtime load of cl.el.
;;     thumfr-thumbify-other-frames: Use thumfr-nset-difference.
;;     thumfr-cull-thumbnail-frames: Use thumfr-delete-if-not.
;; 2011/12/03 dadams
;;     Fixed menu-bar-mode, tool-bar-mode, scroll-bar-mode, fringe-mode: Frame is car of entry.
;; 2011/11/25 dadams
;;     menu-bar-mode, tool-bar-mode, scroll-bar-mode, fringe-mode: Ensure frame is live.
;; 2011/04/16 dadams
;;     iconify-frame: Fix for lexbind Emacs 24: replace named arg FRAME by (ad-get-arg 0).
;; 2011/01/04 dadams
;;     Added autoload cookies for defgroup, defcustom, and cmd thumfr-stack-thumbnail-frames.
;; 2009/11/07 dadams
;;     Renamed: thumfr-doremi-thumbnail-frames to thumfr-doremi-thumbnail-frames+.
;; 2009/05/17 dadams
;;     Added advice for: menu-bar-mode, tool-bar-mode, scroll-bar-mode, fringe-mode.
;;     Renamed everything to have prefix thumfr- (not thumbfrm-).
;;     Renamed: frame-thumbnail-font-difference to thumfr-font-difference,
;;              thumbify-instead-of-iconify-flag to thumfr-thumbify-dont-iconify-flag,
;;              rename-frame-when-thumbify-flag to thumfr-rename-when-thumbify-flag.
;; 2009/04/01 dadams
;;     frame-thumbnail-font-difference:
;;       Use 8 as default value.  Added note about error message in doc string.
;; 2009/03/28 dadams
;;     Removed require of doremi-frm.el - moved it to command doremi-thumbnail-frames.
;; 2009/01/30 dadams
;;     frame-thumbnail-font-difference: Removed temporary hack - Emacs 23 bug #119 was fixed.
;;     thumbfrm-frame-parameters: Don't remove scroll bar.  Users can do that by customizing.
;; 2008/12/13 dadams
;;     frame-thumbnail-font-difference: Hacked it for Emacs 23 - use 8, not 10 pts.
;; 2008/09/21 dadams
;;     thumbfrm-sort-by-name: Respect read-file-name-completion-ignore-case.
;; 2007/09/14 dadams
;;      (de)thumbify-frame: Wrap frame changes in condition-case and undo them if error.
;; 2006/08/07 dadams
;;      dethumbify-frame: raise-frame and give it focus.  Reported Emacs bug:
;;                        sends front Windows app to the bottom of the stack.
;; 2006/01/07 dadams
;;      Added :link for sending bug report.
;; 2006/01/06 dadams
;;      Added :link.
;; 2005/12/03 dadams
;;     Changed suggested bindings.
;; 2005/07/25 dadams
;;     Added :prefix to defgroup.
;; 2005/06/26 dadams
;;     raise-frame: Cleaned up doc string.
;; 2005/05/15 dadams
;;     Renamed: minibuffer-frame to 1on1-minibuffer-frame.
;; 2005/05/09 dadams
;;     Added: thumbnail-frame-p.
;; 2005/05/06 dadams
;;     Added: thumbfrm-next-stack-xoffset and thumbfrm-next-stack-yoffset.  Use them in
;;            thumbify-frame and stack-thumbnail-frames to 1) return thumbified frame to
;;            its stacked position and 2) ensure that thumbifying uses next stack position.
;;     Added: thumbify-frame-upon-event.
;; 2005/04/19 dadams
;;     Protected fset 'only-raise-frame with fboundp.
;; 2005/01/26 dadams
;;     Added: fisheye-next-frame, fisheye-previous-frame.
;;     thumbfrm-stack-display-edge: use right+down as default value.
;; 2005/01/08 dadams
;;     Renamed doremi-grow-font to enlarge-font.  It is now defined in frame-cmds.el.
;; 2005/01/04 dadams
;;     Added rename-frame-when-thumbify-flag; use it in thumbify-frame.
;; 2004/12/25 dadams
;;     stack-thumbnail-frames: Allow any display edge in any direction.
;;     Added: next-stack-position, thumbfrm-frame-parameters,
;;            thumbfrm-last-row-show, thumbfrm-sort-by-window-id.
;;     thumbify-frame: Use thumbfrm-frame-parameters.
;; 2004/12/24 dadams
;;     Added: thumbfrm-stack-display-edge.
;; 2004/12/23 dadams
;;     stack-thumbnail-frames: Raise each frame, so title bars show.
;;         Sort frames.  Account for window-mgr title bar.  Use copy-sequence,
;;         so no side effects.
;;     Use defcustom for user vars.  Added defgroup thumbfrm.
;;     Added: thumbfrm-sort-function, thumbfrm-last-sort-function,
;;            thumbfrm-sort-by-name, toggle-sort-thumbnail-frame-stack,
;;            window-mgr-title-bar-pixel-width.
;;     cull-thumbnail-frames: Remove invisible and iconified frames also.
;; 2004/12/21 dadams
;;     frame-thumbnail-font-difference: Changed defvar to defcustom.
;;     Added: iconify-frame, iconify-or-deiconify-frame, really-iconify-frame,
;;        really-iconify-or-deiconify-frame, thumbify-instead-of-iconify-flag.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
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

(require 'frame-cmds) ;; enlarge-font, rename-non-minibuffer-frame
(eval-when-compile (require 'cl)) ;; case (plus, for Emacs 20: dolist)

;; Quiet the byte-compiler
(defvar read-file-name-completion-ignore-case) ; Emacs 22+

;;;;;;;;;;;;;;;;;;;;;



;;; INTERNAL VARIABLES ;;;;;;;;;;

(defvar thumfr-last-sort-function nil
  "Last non-nil value for `thumfr-sort-function' during this session.")

(defvar thumfr-last-row-show 0.7
  "Minimum amount showing of frames in last row (or column).
Set to 1.0 to see whole frames.
A smaller value uses display real estate better.")

(defvar thumfr-next-stack-xoffset nil
  "X offset for position to stack next thumbnail frame.")

(defvar thumfr-next-stack-yoffset nil
  "Y offset for position to stack next thumbnail frame.")

;; Just so we don't need to test boundp each time.
(defvar 1on1-minibuffer-frame nil)



;;; USER OPTIONS ;;;;;;;;;;;;;;;;

;;;###autoload
(defgroup Thumbnail-Frames nil
  "Commands for thumbnail frames"
  :prefix "thumfr-" :group 'frames :group 'convenience
  :link `(url-link :tag "Send Bug Report"
          ,(concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=\
thumb-frm.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and library versions."))
  :link '(url-link :tag "Other Libraries by Drew"
          "http://www.emacswiki.org/DrewsElispLibraries")
  :link '(url-link :tag "Download" "http://www.emacswiki.org/thumb-frm.el")
  :link '(url-link :tag "Description"
          "http://www.emacswiki.org/FisheyeWithThumbs")
  :link '(emacs-commentary-link :tag "Commentary" "thumb-frm"))

;;;###autoload
(defcustom thumfr-font-difference 8
  "*Number of points for `thumfr-thumbify-frame' to decrease frame font.
This must be less than the current font size, since the new font size
cannot be less than 1 point.

If you get the error \"New font size is too small\" when you try to
thumbify a frame, then decrease `thumfr-font-difference'.

Note that the option value can be a negative integer, in which case
thumbifying actually increases the font and frame size, instead of
decreasing them."
  :type 'integer :group 'Thumbnail-Frames)

;;;###autoload
(defcustom thumfr-thumbify-dont-iconify-flag t
  "*Non-nil means thumbify frames instead of iconifying them."
  :type 'boolean :group 'Thumbnail-Frames)

;;;###autoload
(defcustom thumfr-rename-when-thumbify-flag t
  "*Non-nil means frames are renamed when thumbified.
The new name is the name of the current buffer."
  :type 'boolean :group 'Thumbnail-Frames)

;;;###autoload
(defcustom thumfr-stack-display-edge 'right+down
  "*Display edge to stack thumbnail frames along.
Possible values are symbols named EDGE+DIRECTION,
where EDGE is one of `left', `right', `top', and `bottom',
and DIRECTION is one of `up', `down', `to-left', and `to-right'.

For example, value `right+down' means to arrange thumbnail frames
along the right edge from top to bottom."
  :type
  '(choice
    (const
     :tag "Arrange thumbnail frames along display left, downward" left+down)
    (const
     :tag "Arrange thumbnail frames along display left, upward" left+up)
    (const
     :tag "Arrange thumbnail frames along display right, downward" right+down)
    (const
     :tag "Arrange thumbnail frames along display right, upward" right+up)
    (const
     :tag "Arrange thumbnail frames along display top, toward the right" top+to-right)
    (const
     :tag "Arrange thumbnail frames along display top, toward the left" top+to-left)
    (const
     :tag "Arrange thumbnail frames along display bottom, toward the right" bottom+to-right)
    (const
     :tag "Arrange thumbnail frames along display bottom, toward the left" bottom+to-left))
  :group 'Thumbnail-Frames)

;;;###autoload
(defun thumfr--frame-parameters-:set-function (sym defs)
  "Used for :set of `thumfr-frame-parameters' defcustom.
Needed because Emacs 20 does not have `dolist' without `cl.el'."
  (custom-set-default sym defs)
  (dolist (frm  (frame-list))
    (when (and (frame-live-p frm)  (frame-parameter frm 'thumfr-thumbnail-frame))
      (modify-frame-parameters frm thumfr-frame-parameters))))

;;;###autoload
(defcustom thumfr-frame-parameters
  '((menu-bar-lines . 0)
    (tool-bar-lines . 0)
    (scroll-bar-width . 6)
    (scroll-bar-height . 6)
    (alpha . (100 . 70)))               ; Ignored for Emacs < 23.
  "*Frame parameters for thumbnail frames.
Use this to show or hide things like the menu bar, tool bar, tab bar,
and scroll bars for thumbnail frames.  An `alpha' parameter controls
the frame transparency (Emacs 23+) - see (elisp) `Font and Color
Parameters'."
  :type '(repeat (cons symbol sexp))
  :group 'Thumbnail-Frames
  :set 'thumfr--frame-parameters-:set-function)

;;;###autoload
(defcustom thumfr-sort-function 'thumfr-sort-by-name
  "*Function to use for sorting the stacked thumbnail frames.
If nil, then no sorting is done.
Set this to `thumfr-sort-by-name' for alphabetical order.

The function should take two frames as arguments.  It should return
non-nil if the first frame comes before that of the second.  See, for
example, `thumfr-sort-by-name' and `thumfr-sort-by-window-id'.

Use `thumfr-toggle-sort-thumbnail-frame-stack' to turn sorting on
and off."
  :type '(choice (const :tag "No sorting" nil)
                 (const :tag "Sort by name" thumfr-sort-by-name)
                 (const :tag "Sort by name" thumfr-sort-by-window-id)
                 (function :tag "Another function"))
  :group 'Thumbnail-Frames)

(unless (boundp 'window-mgr-title-bar-pixel-width) ; Defined in `frame-cmds.el'.
  (defcustom window-mgr-title-bar-pixel-width 30
    "*Width of frame title bar provided by window manager, in pixels."
    :type 'integer :group 'Thumbnail-Frames)) ; Normally, the :group is `Frame-Commands'.




;;; FUNCTIONS ;;;;;;;;;;;;;;;;;;;

;; Standard definition from `frame.el'.  Define only if not defined (Emacs 22+ defines it).
(unless (fboundp 'set-frame-parameter)
  (defun set-frame-parameter (frame parameter value)
    "Set frame parameter PARAMETER to VALUE on FRAME.
If FRAME is nil, it defaults to the selected frame.
See `modify-frame-parameters'."
    (modify-frame-parameters frame (list (cons parameter value)))))


;; REPLACES ORIGINAL (built-in).
;; Thumbifies if `thumfr-thumbify-dont-iconify-flag'.
;;
(defadvice iconify-frame (around thumbify-replace-iconify activate)
  "Thumbify FRAME if `thumfr-thumbify-dont-iconify-flag'; else iconify.
To iconify a frame in spite of this flag, use
`\\[thumfr-really-iconify-frame]'."
  (if thumfr-thumbify-dont-iconify-flag
      (thumfr-thumbify-frame (ad-get-arg 0))
    ad-do-it))



;; REPLACES ORIGINAL.
;; Thumbifies/dethumbifies if `thumfr-thumbify-dont-iconify-flag'.
;;
(defadvice iconify-or-deiconify-frame
  (around thumbify-replace-iconify activate)
  "Thumbify frame if `thumfr-thumbify-dont-iconify-flag'; else iconify.
To iconify selected frame in spite of this flag, use
`\\[thumfr-really-iconify-or-deiconify-frame]'."
  (if thumfr-thumbify-dont-iconify-flag
      (thumfr-toggle-thumbnail-frame)
    ad-do-it))

;;;###autoload
(defun thumfr-thumbify-frame-upon-event (event)
  "Thumbify frame upon event EVENT.
To make the window-manager \"minimize\" button thumbify instead, bind
\[iconify-frame] to this command, as follows:
  (define-key special-event-map [iconify-frame]
              'thumfr-thumbify-frame-upon-event)
That will effectively replace iconification by thumbification
everywhere, except for `thumfr-really-iconify-frame' and
`thumfr-really-iconify-or-deiconify-frame'."
  (interactive "e")
  (select-frame (posn-window (event-start event)))
  (make-frame-visible)
  (thumfr-toggle-thumbnail-frame))

;; Thanks to Michael Cadilhac [Michael.Cadilhac-@t-lrde.epita.fr]
;; for a tip on using `discard-input' in a situation like this.
;;;###autoload
(defun thumfr-really-iconify-frame (&optional frame)
  "Iconify FRAME, even if `thumfr-thumbify-dont-iconify-flag' is non-nil."
  (interactive)
  (setq frame  (or frame  (selected-frame)))
  (let ((thumfr-thumbify-dont-iconify-flag  nil))
    ;; Ensure we iconify, even if [iconify-frame] is bound in `special-event-map'.
    (iconify-frame frame)
    (while (not (input-pending-p)) (sit-for 0))
    (discard-input)))

;; Thanks to Michael Cadilhac [Michael.Cadilhac-@t-lrde.epita.fr]
;; for a tip on using `discard-input' in a situation like this.
;;;###autoload
(defun thumfr-really-iconify-or-deiconify-frame ()
  "Iconify or deiconify frame, even if `thumfr-thumbify-dont-iconify-flag'
is non-nil."
  (interactive)
  (let ((thumfr-thumbify-dont-iconify-flag  nil))
    (iconify-or-deiconify-frame)
    ;; Ensure we iconify, even if [iconify-frame] is bound in `special-event-map'.
    (while (not (input-pending-p)) (sit-for 0))
    (discard-input)))

(defun thumfr-thumfr-parameter-p (parameter+value)
  "Return non-nil if PARAMETER+VALUE is a `thumb-frm.el' frame parameter.
PARAMETER+VALUE is a frame-parameter cons: (PARAMETER . VALUE).
This means that PARAMETER is either `thumbfr-thumbnail-frame' or
`thumbfr-non-thumbnail-frame'."
  (memq (car parameter+value) '(thumfr-thumbnail-frame thumfr-non-thumbnail-frame)))

;;;###autoload
(defun thumfr-thumbify-frame (&optional frame)
  "Create a thumbnail version of FRAME (default: selected frame).
Variable `thumfr-frame-parameters' is used to determine
which frame parameters (such as `menu-bar-lines') to remove."
  (interactive)
  (setq frame  (or frame  (selected-frame)))
  (let* ((fr-params      (frame-parameters frame))
         (tf-params      (frame-parameter frame 'thumfr-non-thumbnail-frame))
         (non-tf-params  (thumfr-remove-if #'thumfr-thumfr-parameter-p fr-params)))
    (unless (frame-parameter frame 'alpha) ; Ensure that opacity will be restored.
      (setq non-tf-params  (cons '(alpha . 100) non-tf-params)))
    (when thumfr-rename-when-thumbify-flag (rename-non-minibuffer-frame))
    (unless (frame-parameter frame 'thumfr-thumbnail-frame) ; No-op if already a thumbnail.
      (set-frame-parameter frame 'thumfr-thumbnail-frame     non-tf-params)
      (set-frame-parameter frame 'thumfr-non-thumbnail-frame nil)
      (condition-case thumfr-thumbify-frame
          (progn
            (enlarge-font (- thumfr-font-difference) frame) ; In `frame-cmds.el'.
            (when tf-params (modify-frame-parameters frame tf-params))
            (when thumfr-next-stack-xoffset
              (set-frame-position frame thumfr-next-stack-xoffset
                                  thumfr-next-stack-yoffset)
              (setq thumfr-next-stack-xoffset  nil
                    thumfr-next-stack-yoffset  nil))
            (modify-frame-parameters frame thumfr-frame-parameters))
        (font-too-small                 ; Try again, with a larger font.
         (set-frame-parameter frame 'thumfr-non-thumbnail-frame tf-params)
         (set-frame-parameter frame 'thumfr-thumbnail-frame     nil)
         (unless (> thumfr-font-difference 0)
           (error (error-message-string thumfr-thumbify-frame)))
         (let ((thumfr-font-difference  (1- thumfr-font-difference)))
           (thumfr-thumbify-frame frame)))
        (error
         (set-frame-parameter frame 'thumfr-non-thumbnail-frame tf-params)
         (set-frame-parameter frame 'thumfr-thumbnail-frame     nil)
         (error (error-message-string thumfr-thumbify-frame)))))))

;;;###autoload
(defun thumfr-dethumbify-frame (&optional frame)
  "Restore thumbnail FRAME to original size (default: selected frame)."
  (interactive)
  (setq frame  (or frame  (selected-frame)))
  (let* ((fr-params      (frame-parameters frame))
         (non-tf-params  (frame-parameter frame 'thumfr-thumbnail-frame))
         (tf-params      (thumfr-remove-if #'thumfr-thumfr-parameter-p fr-params)))
    (when non-tf-params                 ; No-op if not a thumbnail.
      (set-frame-parameter frame 'thumfr-non-thumbnail-frame tf-params)
      (set-frame-parameter frame 'thumfr-thumbnail-frame     nil)
      (condition-case thumfr-dethumbify-frame
          (progn
            (enlarge-font thumfr-font-difference frame) ; In `frame-cmds.el'.
            (modify-frame-parameters frame non-tf-params))
        (error
         (set-frame-parameter frame 'thumfr-thumbnail-frame     non-tf-params)
         (set-frame-parameter frame 'thumfr-non-thumbnail-frame nil)
         (error (error-message-string thumfr-dethumbify-frame))))
      (select-frame-set-input-focus frame)
      (thumfr-only-raise-frame frame))))

;;;###autoload
(defsubst thumfr-thumbnail-frame-p (&optional frame)
  "Return non-nil if FRAME is a thumbnail."
  (interactive)
  (frame-parameter frame 'thumfr-thumbnail-frame))


(or (fboundp 'thumfr-only-raise-frame)
    (fset 'thumfr-only-raise-frame (symbol-function 'raise-frame)))

;; REPLACES ORIGINAL (built-in):
;; Also dethumbifies frame.
;;
(defun raise-frame (&optional frame)
  "Bring FRAME to the front, so it occludes any frames it overlaps.
If FRAME is invisible, make it visible.
If FRAME is a thumbnail frame (see `thumb-frm.el'), dethumbify it also.
If you do not specify a frame, the selected frame is used.

If Emacs is displaying on an ordinary terminal or other device that
does not support multiple overlapping frames, then do nothing.

The original behavior function `raise-frame', which does not also
dethumbify, is available as function `thumfr-only-raise-frame'."
  (unless frame (setq frame  (selected-frame)))
  (thumfr-only-raise-frame frame)
  (thumfr-dethumbify-frame frame))

;;;###autoload
(defun thumfr-toggle-thumbnail-frame (&optional frame)
  "If FRAME is a thumbnail, restore it; else thumbify it.
FRAME defaults to the selected frame."
  (interactive)
  (setq frame  (or frame  (selected-frame)))
  (if (thumfr-thumbnail-frame-p frame)
      (thumfr-dethumbify-frame frame)
    (thumfr-thumbify-frame frame)))

;;;###autoload
(defalias 'thumfr-fisheye 'thumfr-thumbify-other-frames)
;;;###autoload
(defun thumfr-thumbify-other-frames (&optional frame)
  "Thumbify all visible non-minibuffer frames except FRAME.
Dethumbify FRAME, if it is a thumbnail frame.
FRAME is the selected frame, by default."
  (interactive)
  (setq frame  (or frame  (selected-frame)))
  (thumfr-dethumbify-frame frame)
  (let ((other-frames  (visible-frame-list)))
    (setq other-frames  (delq frame other-frames)
          other-frames  (delq 1on1-minibuffer-frame other-frames))
    (dolist (fr  (thumfr-nset-difference other-frames (thumfr-thumbnail-frames)))
      (thumfr-thumbify-frame fr))
    (thumfr-stack-thumbnail-frames))
  frame)                                ; Return frame.

(defun thumfr-thumbnail-frames ()
  "Return the current list of visible thumbnail frames."
  (thumfr-remove-if-not #'thumfr-thumbnail-frame-p (visible-frame-list)))

(defun thumfr-culled-thumbnail-frames (&optional keep-iconified-p)
  "Return the list of thumbnail frames without iconified and useless frames.
Useless frames here means includes dead and invisible frames.
Non-nil optional arg KEEP-ICONIFIED-P means do not remove iconified frames."
  (let ((frs  ()))
    (dolist (fr  (thumfr-thumbnail-frames))
      (when (and (frame-live-p fr)
                 (frame-visible-p fr)
                 (or keep-iconified-p  (eq t (frame-visible-p fr))))
        (push fr frs)))
    frs))

;; Same as `bmkp-remove-if' in `bookmark+-1.el'.
(defun thumfr-remove-if (pred xs)
  "A copy of list XS with no elements that satisfy predicate PRED."
  (let ((result  ()))
    (dolist (x  xs)  (unless (funcall pred x) (push x result)))
    (nreverse result)))

;; Same as `bmkp-remove-if-not' in `bookmark+-1.el'.
(defun thumfr-remove-if-not (pred xs)
  "A copy of list XS with only elements that satisfy predicate PRED."
  (let ((result  ()))
    (dolist (x  xs)  (when (funcall pred x) (push x result)))
    (nreverse result)))

;; Define this to avoid requiring `cl.el' at runtime.
(defun thumfr-nset-difference (list1 list2 &optional key)
  "Combine LIST1 and LIST2 using a set-difference operation.
The resulting list contains all items that appear in LIST1 but not LIST2.
This is a destructive function; it reuses the storage of LIST1 and LIST2
whenever possible.

Optional arg KEY is a function used to extract the part of each list
element to compare."
  (if (or (null list1)  (null list2))
      list1
    (apply #'thumfr-set-difference list1 list2 key)))

;; Define this to avoid requiring `cl.el' at runtime.  Same as `icicle-set-difference'.
(defun thumfr-set-difference (list1 list2 &optional key)
  "Combine LIST1 and LIST2 using a set-difference operation.
Optional arg KEY is a function used to extract the part of each list
element to compare.

The result list contains all items that appear in LIST1 but not LIST2.
This is non-destructive; it makes a copy of the data if necessary, to
avoid corrupting the original LIST1 and LIST2."
  (if (or (null list1)  (null list2))
      list1
    (let ((keyed-list2  (and key  (mapcar key list2)))
          (result       ()))
      (while list1
        (unless (if key
                    (member (funcall key (car list1)) keyed-list2)
                  (member (car list1) list2))
          (setq result  (cons (car list1) result)))
        (setq list1  (cdr list1)))
      result)))

;;;###autoload
(defun thumfr-fisheye-previous-frame ()
  "Call `thumfr-fisheye' on the previous frame.
Thumbify all visible non-minibuffer frames except the previous frame.
Dethumbify the previous frame (if it is a thumbnail frame)."
  (interactive)
  (select-frame (thumfr-thumbify-other-frames (other-frame -1)))
  (message "%s" (get-frame-name)))

;;;###autoload
(defun thumfr-fisheye-next-frame ()
  "Call `thumfr-fisheye' on the next frame.
Thumbify all visible non-minibuffer frames except the next frame.
Dethumbify the next frame (if it is a thumbnail frame)."
  (interactive)
  (select-frame (thumfr-thumbify-other-frames (other-frame 1)))
  (message "%s" (get-frame-name)))

;;;###autoload
(defun thumfr-dethumbify-all-frames (&optional iconified-also-p)
  "Dethumbify all visible frames
restoring them to their states before they were thumbified.
With a prefix arg, restore also iconified frames, not just thumbified
frames."
  (interactive "P")
  (dolist (fr  (thumfr-culled-thumbnail-frames iconified-also-p))
    (thumfr-dethumbify-frame fr)))

;; New row (or column) offset is based on the size of the previous
;; frame, not the current frame, which is not really correct.  This is
;; easier to do, and works OK most of the time.  It is not ideal if
;; frame sizes vary a great deal.
;;
;;;###autoload
(defun thumfr-stack-thumbnail-frames ()
  "Stack thumbnail frames along edge of display
according to the direction of `thumfr-stack-display-edge'."
  (interactive)
  (let* ((display-width   (x-display-pixel-width))
         (display-height  (if (boundp '1on1-minibuffer-frame)
                              (frame-parameter 1on1-minibuffer-frame 'top)
                            (x-display-pixel-height)))
         (xstart          (if (memq thumfr-stack-display-edge
                                    '(right+down right+up top+to-left bottom+to-left))
                              display-width
                            0))
         (ystart          (if (memq thumfr-stack-display-edge
                                    '(bottom+to-right bottom+to-left left+up right+up))
                              display-height
                            0))
         (xoffset         xstart)
         (yoffset         ystart)
         (thumb-frs       (thumfr-culled-thumbnail-frames))
         last-fr)
    (when thumfr-sort-function (setq thumb-frs  (sort thumb-frs thumfr-sort-function)))

    ;; These loops are similar, more or less symmetric.
    (case thumfr-stack-display-edge
      (left+down
       (dolist (fr  thumb-frs)
         (set-frame-position fr xoffset yoffset)
         (let ((next-position  (thumfr-next-stack-position
                                yoffset ystart (- display-height
                                                  window-mgr-title-bar-pixel-width
                                                  ;;(frame-pixel-height fr)
                                                  )
                                #'+ (+ (frame-pixel-height fr)
                                       window-mgr-title-bar-pixel-width) #'>
                                xoffset xstart (- display-width (frame-pixel-width fr))
                                #'+ (frame-pixel-width fr) #'>)))
           (setq yoffset  (cdr next-position)
                 xoffset  (car next-position)))
         (thumfr-only-raise-frame fr))
       (when thumb-frs
         (setq thumfr-next-stack-xoffset  xoffset
               thumfr-next-stack-yoffset  yoffset)))
      (left+up
       (dolist (fr  thumb-frs)
         (setq last-fr  fr)
         (set-frame-position
          fr
          xoffset
          (max 0 (- yoffset window-mgr-title-bar-pixel-width (frame-pixel-height fr))))
         (let ((next-position  (thumfr-next-stack-position
                                yoffset ystart (truncate (* (frame-pixel-height fr)
                                                            thumfr-last-row-show))
                                #'-
                                (+ (frame-pixel-height fr) window-mgr-title-bar-pixel-width)
                                #'<
                                xoffset xstart (- display-width (frame-pixel-width fr))
                                #'+
                                (frame-pixel-width fr)
                                #'>)))
           (setq yoffset  (cdr next-position)
                 xoffset  (car next-position))))
       (dolist (fr  (nreverse thumb-frs)) (thumfr-only-raise-frame fr))
       (when thumb-frs
         (setq thumfr-next-stack-xoffset  xoffset
               thumfr-next-stack-yoffset  (max 0 (- yoffset window-mgr-title-bar-pixel-width
                                                    (frame-pixel-height last-fr))))))
      (right+down
       (dolist (fr  thumb-frs)
         (setq last-fr  fr)
         (set-frame-position fr (- xoffset (frame-pixel-width fr)) yoffset)
         (let ((next-position  (thumfr-next-stack-position
                                yoffset ystart (- display-height
                                                  window-mgr-title-bar-pixel-width
                                                  ;;(frame-pixel-height fr)
                                                  )
                                #'+
                                (+ (frame-pixel-height fr) window-mgr-title-bar-pixel-width)
                                #'>
                                xoffset xstart 0
                                #'-
                                (frame-pixel-width fr)
                                #'<)))
           (setq yoffset  (cdr next-position)
                 xoffset  (car next-position)))
         (thumfr-only-raise-frame fr))
       (when thumb-frs
         (setq thumfr-next-stack-xoffset  (- xoffset (frame-pixel-width last-fr))
               thumfr-next-stack-yoffset  yoffset)))
      (right+up
       (dolist (fr  thumb-frs)
         (setq last-fr  fr)
         (set-frame-position
          fr
          (max 0 (- xoffset (frame-pixel-width fr)))
          (max 0 (- yoffset window-mgr-title-bar-pixel-width (frame-pixel-height fr))))
         (let ((next-position  (thumfr-next-stack-position
                                yoffset ystart (truncate (* (frame-pixel-height fr)
                                                            thumfr-last-row-show))
                                #'-
                                (+ (frame-pixel-height fr) window-mgr-title-bar-pixel-width)
                                #'<
                                xoffset xstart 0
                                #'-
                                (frame-pixel-width fr)
                                #'<)))
           (setq yoffset  (cdr next-position)
                 xoffset  (car next-position))))
       (dolist (fr  (nreverse thumb-frs)) (thumfr-only-raise-frame fr))
       (when thumb-frs
         (setq thumfr-next-stack-xoffset  (max 0 (- xoffset (frame-pixel-width last-fr)))
               thumfr-next-stack-yoffset  (max 0 (- yoffset window-mgr-title-bar-pixel-width
                                                    (frame-pixel-height last-fr))))))
      (top+to-right
       (dolist (fr  thumb-frs)
         (set-frame-position fr xoffset yoffset)
         (let ((next-position  (thumfr-next-stack-position
                                xoffset xstart (- display-width (frame-pixel-width fr))
                                #'+
                                (frame-pixel-width fr)
                                #'>
                                yoffset ystart (- display-height
                                                  window-mgr-title-bar-pixel-width
                                                  (frame-pixel-height fr))
                                #'+
                                (+ (frame-pixel-height fr) window-mgr-title-bar-pixel-width)
                                #'>)))
           (setq yoffset  (car next-position)
                 xoffset  (cdr next-position)))
         (thumfr-only-raise-frame fr))
       (when thumb-frs (setq thumfr-next-stack-xoffset  xoffset
                             thumfr-next-stack-yoffset  yoffset)))
      (top+to-left
       (dolist (fr  thumb-frs)
         (setq last-fr  fr)
         (set-frame-position fr (max 0 (- xoffset (frame-pixel-width fr))) yoffset)
         (let ((next-position  (thumfr-next-stack-position
                                xoffset xstart (truncate (* (frame-pixel-width fr)
                                                            thumfr-last-row-show))
                                #'-
                                (frame-pixel-width fr)
                                #'<
                                yoffset ystart (- display-height
                                                  window-mgr-title-bar-pixel-width
                                                  (frame-pixel-height fr))
                                #'+
                                (+ (frame-pixel-height fr) window-mgr-title-bar-pixel-width)
                                #'>)))
           (setq yoffset  (car next-position)
                 xoffset  (cdr next-position)))
         (thumfr-only-raise-frame fr))
       (when thumb-frs
         (setq thumfr-next-stack-xoffset  (max 0 (- xoffset (frame-pixel-width last-fr)))
               thumfr-next-stack-yoffset  yoffset)))
      (bottom+to-right
       (dolist (fr  thumb-frs)
         (setq last-fr  fr)
         (set-frame-position
          fr
          xoffset
          (max 0 (- yoffset window-mgr-title-bar-pixel-width (frame-pixel-height fr))))
         (let ((next-position  (thumfr-next-stack-position
                                xoffset xstart (- display-width (frame-pixel-width fr))
                                #'+
                                (frame-pixel-width fr)
                                #'>
                                yoffset ystart (- display-height
                                                  window-mgr-title-bar-pixel-width
                                                  (frame-pixel-height fr))
                                #'-
                                (+ (frame-pixel-height fr) window-mgr-title-bar-pixel-width)
                                #'<)))
           (setq yoffset  (car next-position)
                 xoffset  (cdr next-position))))
       (dolist (fr  (nreverse thumb-frs)) (thumfr-only-raise-frame fr))
       (when thumb-frs
         (setq thumfr-next-stack-xoffset  xoffset
               thumfr-next-stack-yoffset  (max 0 (- yoffset window-mgr-title-bar-pixel-width
                                                    (frame-pixel-height last-fr))))))
      (bottom+to-left
       (dolist (fr  thumb-frs)
         (setq last-fr  fr)
         (set-frame-position
          fr
          (max 0 (- xoffset (frame-pixel-width fr)))
          (max 0 (- yoffset window-mgr-title-bar-pixel-width (frame-pixel-height fr))))
         (let ((next-position  (thumfr-next-stack-position
                                xoffset xstart (* (truncate (frame-pixel-width fr)
                                                            thumfr-last-row-show))
                                #'-
                                (frame-pixel-width fr)
                                #'<
                                yoffset ystart (- display-height
                                                  window-mgr-title-bar-pixel-width
                                                  (frame-pixel-height fr))
                                #'-
                                (+ (frame-pixel-height fr) window-mgr-title-bar-pixel-width)
                                #'<)))
           (setq yoffset  (car next-position)
                 xoffset  (cdr next-position))))
       (dolist (fr  (nreverse thumb-frs)) (thumfr-only-raise-frame fr))
       (when thumb-frs
         (setq thumfr-next-stack-xoffset  (max 0 (- xoffset (frame-pixel-width last-fr)))
               thumfr-next-stack-yoffset  (max 0 (- yoffset window-mgr-title-bar-pixel-width
                                                    (frame-pixel-height last-fr)))))))))

;; Helper function for `thumfr-stack-thumbnail-frames'.
(defun thumfr-next-stack-position
  (major-position major-start major-limit major-+/- major-increment major-</>
   minor-position minor-start minor-limit minor-+/- minor-increment minor-</>)
  "Return new position for next-stacked frame.
MAJOR* are the settings for the major direction of movement (x or y).
MINOR* are the settings for the minor direction of movement (x or y).
New position is returned as a cons: (MINOR-POSITION . MAJOR-POSITION).

*-POSITION, as input, are the positions of the current frame.
*-START are the starting positions for the first row (or column).
*-LIMIT are the maximum positions on the display.
*-+/- are functions `+' or `-', used to increment or decrement.
*-</> are functions `<' or `>', used to compare."
  (setq major-position  (funcall major-+/- major-position major-increment))
  (when (funcall major-</> major-position major-limit)
    (setq major-position  major-start
          minor-position  (funcall minor-+/- minor-position minor-increment))
    (when (funcall minor-</> minor-position minor-limit)
      (setq minor-position  minor-start)))
  (cons minor-position major-position))


;;;###autoload
(defun thumfr-toggle-sort-thumbnail-frame-stack (force-p)
  "Toggle stacking thumbnail frames between sorting and not.
Non-nil prefix FORCE-P => Sort iff FORCE-P >= 0."
  (interactive "P")
  (cond (thumfr-sort-function
         (setq thumfr-last-sort-function  thumfr-sort-function) ; Save it.
         (when (or (null force-p)  (<= (prefix-numeric-value force-p) 0))
           (setq thumfr-sort-function  nil))) ; Don't sort.
        ((or (null force-p)  (> (prefix-numeric-value force-p) 0)) ; Ask to sort
         (if thumfr-last-sort-function ; Sort using saved sort fn.
             (setq thumfr-sort-function  thumfr-last-sort-function)
           (error "You first need to set `thumfr-sort-function'"))))
  (if thumfr-sort-function
      (message "Stacking of thumbnail frames is now sorted using `%s'."
               thumfr-sort-function)
    (message "Stacking of thumbnail frames is longer sorted.")))

;;;###autoload
(defun thumfr-iconify-thumbnail-frames ()
  "Iconify all thumbnail frames."
  (interactive)
  (let ((thumfr-thumbify-dont-iconify-flag  nil))
    (dolist (fr  (thumfr-culled-thumbnail-frames))
      (iconify-frame fr))))

;;;###autoload
(defun thumfr-deiconify-thumbnail-frames ()
  "Deiconify all thumbnail frames."
  (interactive)
  (dolist (fr  (thumfr-culled-thumbnail-frames 'KEEP-ICONIFIED))
    (make-frame-visible fr)))

;; Define this here to avoid requiring `cl.el' at runtime.
(defun thumfr-delete-if-not (predicate list)
  "Remove all occurrences of ITEM in LIST that do not satisfy PREDICATE.
This is a destructive function: It reuses the storage of LIST whenever
possible."
  (while (and list  (not (funcall predicate (car list))))
    (setq list  (cdr list)))
  (let ((cl-p  list))
    (while (cdr cl-p)
      (if (not (funcall predicate (cadr cl-p)))
          (setcdr cl-p (cddr cl-p))
        (setq cl-p  (cdr cl-p)))))
  list)

;;;###autoload
(defun thumfr-doremi-thumbnail-frames+ ()
  "Successively cycle through frames with `thumfr-fisheye'."
  (interactive)
  (unless (require 'doremi-frm nil t)
    (error "You need library `doremi-frm.el' to use this command"))
  (let ((other-frames  (visible-frame-list)))
    (setq other-frames  (delq 1on1-minibuffer-frame other-frames))
    (doremi (lambda (fr) (thumfr-thumbify-other-frames fr) fr)
            (selected-frame)
            nil                         ; ignored
            nil                         ; ignored
            other-frames)))

(defun thumfr-sort-by-name (frame1 frame2)
  "Alphabetical comparison of names of frames in FRAME1 and FRAME2.
Return non-nil if name of FRAME1 comes before that of FRAME2.
However, if the frame names name buffers visiting files, then compare
the names respecting `read-file-name-completion-ignore-case'."
  (let ((fname1  (frame-parameter frame1 'name))
        (fname2  (frame-parameter frame2 'name)))
    (if (or (not (boundp 'read-file-name-completion-ignore-case))
            (not read-file-name-completion-ignore-case))
        (string-lessp fname1 fname2)
      (let* ((buf1      (get-buffer fname1))
             (bufname1  (and buf1  (buffer-file-name buf1)))
             (file1     (and bufname1  (upcase bufname1)))
             (buf2      (get-buffer fname2))
             (bufname2  (and buf2  (buffer-file-name buf2)))
             (file2     (and bufname2  (upcase bufname2))))
        (if (and file1  file2)
            (string-lessp file1 file2)
          (string-lessp fname1 fname2))))))

(defun thumfr-sort-by-window-id (frame1 frame2)
  "Comparison of `window-id' parameters in FRAME1 and FRAME2.
Return non-nil if `window-id' parameter of first frame comes before
that of second.  The `window-id' can be used as a substitute for time
of window creation."
  (string-lessp (frame-parameter frame1 'window-id) (frame-parameter frame2 'window-id)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'thumb-frm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; thumb-frm.el ends here
